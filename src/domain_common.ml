(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 * Author Dave Scott <dave.scott@eu.citrix.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Printf
open Stringext
open Listext
open Pervasiveext

module D = Debug.Debugger(struct let name = "xenops" end)
open D

type create_info = {
	ssidref: int32;
	hvm: bool;
	hap: bool;
	name: string;
	xsdata: (string * string) list;
	platformdata: (string * string) list;
}

type build_hvm_info = {
	pae: bool;
	apic: bool;
	acpi: bool;
	nx: bool;
	smbios_pt: bool;
        smbios_oem_types_pt: string;
	acpi_pt: bool;
	viridian: bool;
	shadow_multiplier: float;
	timeoffset: string;
	timer_mode: int option;
	hpet: int option;
	vpt_align: int option;
	videoram: int option;
}

type build_pv_info = {
	cmdline: string;
	ramdisk: string option;
}

type builder_spec_info = BuildHVM of build_hvm_info | BuildPV of build_pv_info

type build_info = {
	memory_max: int64;    (* memory max in kilobytes *)
	memory_target: int64; (* memory target in kilobytes *)
	kernel: string;       (* in hvm case, point to hvmloader *)
	vcpus: int;           (* vcpus max *)
	priv: builder_spec_info;
}

type stubdom_info = {
	stubdom_target: int;
}

type domid = int

exception Restore_signature_mismatch
exception Domain_build_failed
exception Domain_restore_failed
exception Domain_restore_truncated_hvmstate
exception Xenguest_protocol_failure of string (* internal protocol failure *)
exception Xenguest_failure of string (* an actual error is reported to us *)
exception Timeout_backend
exception Could_not_read_file of string (* eg linux kernel/ initrd *)
exception Domain_stuck_in_dying_state of Xc.domid

let xend_save_signature = "LinuxGuestRecord"
let save_signature = "XenSavedDomain\n"
let qemu_save_signature = "QemuDeviceModelRecord\n"
let hvmloader = "/usr/lib/xen/boot/hvmloader"
let releaseDomain = "@releaseDomain"
let introduceDomain = "@introduceDomain"

let log_exn_continue msg f x = try f x with e -> debug "Ignoring exception: %s while %s" (Printexc.to_string e) msg

let log_exn_rm ~xs x = log_exn_continue ("xenstore-rm " ^ x) xs.Xs.rm x

let set_difference a b = List.filter (fun x -> not(List.mem x b)) a

let assert_file_is_readable filename = 
	try Unix.access filename [ Unix.F_OK; Unix.R_OK ]
	with _ -> raise (Could_not_read_file filename)

type domarch = Arch_HVM | Arch_native | Arch_X64 | Arch_X32

let string_of_domarch = function
	| Arch_HVM    -> "hvm"
	| Arch_native -> ""
	| Arch_X64    -> "x64"
	| Arch_X32    -> "x32"

let domarch_of_string = function
	| "hvm" -> Arch_HVM
	| "x64" -> Arch_X64
	| "x32" -> Arch_X32
	| _     -> Arch_native

let make ~xc ~xs info uuid =
	let flags =
		(if info.hvm then [ Xc.CDF_HVM ] else []) @
		(if (info.hvm && info.hap) then [ Xc.CDF_HAP ] else []) in
	let domid = Xc.domain_create xc info.ssidref flags uuid in
	let name = if info.name <> "" then info.name else sprintf "Domain-%d" domid in
	try
		let dom_path = xs.Xs.getdomainpath domid in
		let vm_path = "/vm/" ^ (Uuid.to_string uuid) in
		let vss_path = "/vss/" ^ (Uuid.to_string uuid) in
		let roperm = Xenbus.roperm_for_guest domid in
		let rwperm = Xenbus.rwperm_for_guest domid in
		debug "Regenerating the xenstored tree under: [%s]" dom_path;

		Xs.transaction xs (fun t ->
			(* Clear any existing rubbish in xenstored *)
			t.Xst.rm dom_path;
			t.Xst.mkdir dom_path;
			t.Xst.setperms dom_path roperm;

			t.Xst.rm vm_path;
			t.Xst.mkdir vm_path;
			t.Xst.setperms vm_path roperm;

			t.Xst.rm vss_path;
			t.Xst.mkdir vss_path;
			t.Xst.setperms vss_path rwperm;

			t.Xst.write (dom_path ^ "/vm") vm_path;
			t.Xst.write (dom_path ^ "/vss") vss_path;
			t.Xst.write (dom_path ^ "/name") name;

			(* create cpu and memory directory with read only perms *)
			List.iter (fun dir ->
				let ent = sprintf "%s/%s" dom_path dir in
				t.Xst.mkdir ent;
				t.Xst.setperms ent roperm
			) [ "cpu"; "memory" ];
			(* create read/write nodes for the guest to use *)
			List.iter (fun dir ->
				let ent = sprintf "%s/%s" dom_path dir in
				t.Xst.mkdir ent;
				t.Xst.setperms ent rwperm
			) [ "device"; "error"; "drivers"; "control"; "attr"; "data"; "messages" ];
		);

		xs.Xs.writev vm_path [
			"uuid", (Uuid.to_string uuid);
			"name", name;
		];

		xs.Xs.writev dom_path info.xsdata;
		xs.Xs.writev (dom_path ^ "/platform") info.platformdata;

		xs.Xs.write (dom_path ^ "/control/platform-feature-multiprocessor-suspend") "1";

		debug "Created domain with id: %d" domid;
		domid
	with e ->
		debug "Caught exception in domid %d creation: %s" domid (Printexc.to_string e);
		raise e

(** create store and console channels *)
let create_channels ~xc domid =
	let store = Xc.evtchn_alloc_unbound xc domid 0 in
	let console = Xc.evtchn_alloc_unbound xc domid 0 in
	store, console

(* pre build *)
let build_pre ~xc ~xs ~vcpus ~mem_max_kib ~shadow_kib ~video_ram_kib ~timer_mode ~hpet ~vpt_align domid =
	let shadow_mib : int option =
		match shadow_kib with
		| None     -> None
		| Some kib -> Some (Int64.to_int (Int64.div kib 1024L)) in
	debug "build_pre domid=%d; mem=%Lu KiB; shadow=%Lu KiB (%d MiB); video=%Lu KiB"
	      domid mem_max_kib (match shadow_kib with None -> 0L | Some i -> i)
	      (match shadow_mib with None -> 0 | Some i -> i) video_ram_kib;
	let maybe_exn_ign name f opt =
		maybe (fun opt -> try f opt with exn -> warn "exception setting %s: %s" name (Printexc.to_string exn)) opt
		in
	maybe_exn_ign "timer mode" (fun mode -> Xc.domain_set_timer_mode xc domid mode) timer_mode;
	maybe_exn_ign "hpet" (fun hpet -> Xc.domain_set_hpet xc domid hpet) hpet;
	maybe_exn_ign "vpt align" (fun vpt_align -> Xc.domain_set_vpt_align xc domid vpt_align) vpt_align;
	Xc.domain_max_vcpus xc domid vcpus;
	Xc.domain_setmaxmem xc domid (Int64.add mem_max_kib video_ram_kib);
	Xc.domain_set_memmap_limit xc domid mem_max_kib;
	maybe (Xc.shadow_allocation_set xc domid) shadow_mib;
	create_channels ~xc domid

(* puts value in store after the uncooperative domain resume *)
let resume_post ~xc ~xs domid store_port console_port =
	let dom_path = xs.Xs.getdomainpath domid in
	let store_mfn_s = xs.Xs.read (dom_path ^ "/store/ring-ref") in
	let store_mfn = Nativeint.of_string store_mfn_s in

	let ents = [
		("store/port", string_of_int store_port);
		("console/port", string_of_int console_port);
		("serial/0/limit", string_of_int 65536);
	] in
	Xs.transaction xs (fun t -> t.Xst.writev dom_path ents);
	xs.Xs.introduce domid store_mfn store_port

(* puts value in store after the domain build succeed *)
let build_post ~xc ~xs ~vcpus ~mem_target_kib ~mem_max_kib domid
               store_mfn store_port ents vments =
	let dom_path = xs.Xs.getdomainpath domid in
	(* expand local stuff with common values *)
	let ents =
		[ ("memory/static-max", Int64.to_string mem_max_kib);
		  ("memory/target", Int64.to_string mem_target_kib);
		  ("domid", string_of_int domid);
		  ("store/port", string_of_int store_port);
		  ("store/ring-ref", sprintf "%nu" store_mfn);
		] @ ents in
	Xs.transaction xs (fun t -> t.Xst.writev dom_path ents);
	if vments <> [] then (
		let vm_path = xs.Xs.read (dom_path ^ "/vm") in
		Xs.transaction xs (fun t -> t.Xst.writev vm_path vments)
	);
	xs.Xs.introduce domid store_mfn store_port

(** build a linux type of domain *)
let build_linux ~xc ~xs ~mem_max_kib ~mem_target_kib ~kernel ~cmdline ~ramdisk ~vcpus domid =
	assert_file_is_readable kernel;
	maybe assert_file_is_readable ramdisk;

	let mem_max_kib' = Memory.Linux.required_available mem_max_kib in

	let store_port, console_port =
		build_pre ~xc ~xs ~mem_max_kib:mem_max_kib' ~shadow_kib:None ~video_ram_kib:0L
		          ~vcpus ~timer_mode:None ~hpet:None ~vpt_align:None domid in

	let mem_target_mib = (Int64.to_int (Int64.div mem_target_kib 1024L)) in
	let cnx = XenguestHelper.connect
	  [
	    "-mode"; "linux_build";
	    "-domid"; string_of_int domid;
	    "-memsize"; string_of_int mem_target_mib;
	    "-image"; kernel;
	    "-ramdisk"; (match ramdisk with Some x -> x | None -> "");
	    "-cmdline"; cmdline;
	    "-features"; "";
	    "-flags"; "0";
	    "-store_port"; string_of_int store_port;
	    "-console_port"; string_of_int console_port;
	    "-fork"; "true";
	  ] [] in
	let line = finally
	  (fun () -> XenguestHelper.receive_success cnx)
	  (fun () -> XenguestHelper.disconnect cnx) in

	debug "Read [%s]" line;
	let store_mfn, console_mfn, protocol =
		match String.split ' ' line with
		| [ store_mfn; console_mfn; protocol ] ->
		    Nativeint.of_string store_mfn, Nativeint.of_string console_mfn, protocol
		| _ ->
		    raise Domain_build_failed in

	let local_stuff = [
		"serial/0/limit",    string_of_int 65536;
		"console/port",      string_of_int console_port;
		"console/ring-ref",  sprintf "%nu" console_mfn;
	] in
	build_post ~xc ~xs ~vcpus ~mem_target_kib ~mem_max_kib domid store_mfn store_port local_stuff [];
	match protocol with
	| "x86_32-abi" -> Arch_X32
	| "x86_64-abi" -> Arch_X64
	| _            -> Arch_native

(** build hvm type of domain *)
let build_hvm ~xc ~xs ~mem_max_kib ~mem_target_kib ~video_ram_mib ~shadow_multiplier ~vcpus
              ~kernel ~pae ~apic ~acpi ~nx ~smbios_pt ~smbios_oem_types_pt ~acpi_pt ~viridian ~timeoffset
	      ~timer_mode ~hpet ~vpt_align domid =
	assert_file_is_readable kernel;

	(* NB we reserve a little extra *)
	let mem_max_kib' = Memory.HVM.required_available mem_max_kib
	and shadow_kib = Memory.HVM.required_shadow vcpus mem_max_kib shadow_multiplier in
	(* HVM must have at least 1MiB of shadow *)
	let shadow_kib = max 1024L shadow_kib in
	let video_ram_kib = match video_ram_mib with | None -> 4096L | Some mib -> Int64.mul (Int64.of_int mib) 1024L in

	let store_port, console_port =
		build_pre ~xc ~xs ~mem_max_kib:mem_max_kib' ~shadow_kib:(Some shadow_kib)
		          ~video_ram_kib ~vcpus ~timer_mode ~hpet ~vpt_align domid in

	let mem_max_mib = (Int64.to_int (Int64.div mem_max_kib 1024L)) in

	let cnx = XenguestHelper.connect
	  [
	    "-mode"; "hvm_build";
	    "-domid"; string_of_int domid;
	    "-store_port"; string_of_int store_port;
	    "-image"; kernel;
	    "-memsize"; string_of_int mem_max_mib;
	    "-vcpus"; string_of_int vcpus;
	    "-pae"; string_of_bool pae;
	    "-apic"; string_of_bool apic;
	    "-acpi"; string_of_bool acpi;
	    "-nx"; string_of_bool nx;
	    "-smbios_pt"; string_of_bool smbios_pt;
            "-smbios_oem_types_pt"; smbios_oem_types_pt;
	    "-acpi_pt"; string_of_bool acpi_pt;
	    "-viridian"; string_of_bool viridian;
	    "-fork"; "true";
	  ] [] in
	let line = finally
	  (fun () -> XenguestHelper.receive_success cnx)
	  (fun () -> XenguestHelper.disconnect cnx) in

	(* XXX: domain builder will reduce our shadow allocation under our feet.
	   Detect this and override. *)
	let requested_shadow_mib = Int64.to_int (Int64.div shadow_kib 1024L) in
	let actual_shadow_mib = Xc.shadow_allocation_get xc domid in
	if actual_shadow_mib < requested_shadow_mib then (
		warn "HVM domain builder reduced our shadow memory from %d to %d MiB; reverting" 
		     requested_shadow_mib actual_shadow_mib;
		Xc.shadow_allocation_set xc domid requested_shadow_mib;
		let shadow = Xc.shadow_allocation_get xc domid in
		debug "Domain now has %d MiB of shadow" shadow;
	);

	debug "Read [%s]" line;
	let store_mfn =
		try Nativeint.of_string line
		with _ -> raise Domain_build_failed in
	let vm_stuff = [
		"rtc/timeoffset",    timeoffset;
	] in

	build_post ~xc ~xs ~vcpus ~mem_target_kib ~mem_max_kib domid store_mfn store_port [] vm_stuff;
	Arch_HVM

let build ~xc ~xs info domid =
	match info.priv with
	| BuildHVM hvminfo ->
		build_hvm ~xc ~xs ~mem_max_kib:info.memory_max ~mem_target_kib:info.memory_target
		          ~video_ram_mib:hvminfo.videoram
		          ~shadow_multiplier:hvminfo.shadow_multiplier ~vcpus:info.vcpus
		          ~kernel:info.kernel ~pae:hvminfo.pae ~apic:hvminfo.apic ~acpi:hvminfo.acpi
		          ~nx:hvminfo.nx ~smbios_pt:hvminfo.smbios_pt ~smbios_oem_types_pt:hvminfo.smbios_oem_types_pt 
                          ~acpi_pt:hvminfo.acpi_pt ~viridian:hvminfo.viridian ~timeoffset:hvminfo.timeoffset
		          ~timer_mode:hvminfo.timer_mode ~hpet:hvminfo.hpet ~vpt_align:hvminfo.vpt_align domid
	| BuildPV pvinfo   ->
		build_linux ~xc ~xs ~mem_max_kib:info.memory_max ~mem_target_kib:info.memory_target
		            ~kernel:info.kernel ~cmdline:pvinfo.cmdline ~ramdisk:pvinfo.ramdisk
		            ~vcpus:info.vcpus domid

let read_signature fd =
	let l_new_sig = String.length save_signature in
	let l_leg_sig = String.length xend_save_signature in
	let minlen = min l_new_sig l_leg_sig in

	let s = Io.read fd minlen in
	let end_to_read, oldformat =
		if String.startswith s save_signature then (
			String.sub save_signature minlen (l_new_sig - minlen), false
		) else if String.startswith s xend_save_signature then
			String.sub qemu_save_signature minlen (l_leg_sig - minlen), true
		else
			raise Restore_signature_mismatch;
		in
	if end_to_read <> "" then (
		if Io.read fd (String.length end_to_read) <> end_to_read then
			raise Restore_signature_mismatch;
	);
	oldformat

(* restore a domain from a file descriptor. it read first the signature
 * to be we are not trying to restore from random data.
 * the linux_restore process is in charge to allocate memory as it's needed
 *)
let restore_common ~xc ~xs ~hvm ~store_port ~console_port ~vcpus ~extras domid fd =
	let oldformat = read_signature fd in
	if oldformat then (
		let cfglen = Io.read_int fd in
		ignore (Io.read fd cfglen)
	);

	Unix.clear_close_on_exec fd;
	let cnx = XenguestHelper.connect
	  ([
	    "-mode"; if hvm then "hvm_restore" else "restore";
	    "-domid"; string_of_int domid;
	    "-fd"; string_of_int (Obj.magic fd);
	    "-store_port"; string_of_int store_port;
	    "-console_port"; string_of_int console_port;
	    "-fork"; "true";
	  ] @ extras) [ fd ] in

	let line = finally
		(fun () -> XenguestHelper.receive_success cnx)
		(fun () -> XenguestHelper.disconnect cnx) in

	debug "Read [%s]" line;
	let store_mfn, console_mfn =
		match String.split_f String.isspace line with
		| [ store; console ] -> Nativeint.of_string store, Nativeint.of_string console
		| _                  -> raise Domain_restore_failed
		in

	if hvm then (
		let qemu_save_signature =
			if oldformat then
				String.sub qemu_save_signature 0 (String.length qemu_save_signature - 1)
			else
				qemu_save_signature
			in
		(* restore qemu-dm tmp file *)
		if Io.read fd (String.length qemu_save_signature) <> qemu_save_signature then
			raise Restore_signature_mismatch;
		let limit = Int64.of_int (Io.read_int fd) in
		debug "qemu-dm state file size: %Ld" limit;

		let file = sprintf "/tmp/xen.qemu-dm.%d" domid in
		let fd2 = Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; ] 0o640 in
		finally (fun () ->
			if Unixext.copy_file ~limit fd fd2 <> limit then
				raise Domain_restore_truncated_hvmstate
		) (fun () -> Unix.close fd2);
	);
	store_mfn, console_mfn

let resume ~xc ~xs ~cooperative domid =
	if cooperative then
		Xc.domain_resume_fast xc domid
	else (
		(* FIXME release devices *)

		Xc.evtchn_reset xc domid;
		xs.Xs.rm (sprintf "%s/control/shutdown" (xs.Xs.getdomainpath domid));
		let store, console = create_channels ~xc domid in
		resume_post ~xc ~xs domid store console;

		(* FIXME create devices *)

		let cnx = XenguestHelper.connect [
			"-mode"; "slow_resume";
			"-domid"; string_of_int domid;
			"-fork"; "true";
		] [] in
		let line = finally (fun () ->
			XenguestHelper.receive_success cnx
		) (fun () -> XenguestHelper.disconnect cnx) in
		debug "Read [%s]" line;
	)

let pv_restore ~xc ~xs ~mem_max_kib ~mem_target_kib ~vcpus domid fd =
	let mem_max_kib' = Memory.Linux.required_available mem_max_kib in

	let store_port, console_port =
	        build_pre ~xc ~xs ~mem_max_kib:mem_max_kib' ~shadow_kib:None ~video_ram_kib:0L
		          ~vcpus ~timer_mode:None ~hpet:None ~vpt_align:None domid in

	let store_mfn, console_mfn = restore_common ~xc ~xs ~hvm:false
	                                            ~store_port ~console_port
	                                            ~vcpus ~extras:[] domid fd in
	let local_stuff = [
		"serial/0/limit",    string_of_int 65536;
		"console/port",     string_of_int console_port;
		"console/ring-ref", sprintf "%nu" console_mfn;
	] in
	build_post ~xc ~xs ~vcpus ~mem_target_kib ~mem_max_kib domid store_mfn store_port local_stuff []

let hvm_restore ~xc ~xs ~mem_max_kib ~mem_target_kib ~video_ram_mib ~shadow_multiplier ~vcpus ~pae ~viridian
	        ~timeoffset ~timer_mode ~hpet ~vpt_align domid fd =
	let shadow_kib = Memory.HVM.required_shadow vcpus mem_max_kib shadow_multiplier
	and mem_max_kib' = Memory.HVM.required_available mem_max_kib in
	let video_ram_kib = match video_ram_mib with | None -> 4096L | Some mib -> Int64.mul (Int64.of_int mib) 1024L in

	let store_port, console_port =
		build_pre ~xc ~xs ~mem_max_kib:mem_max_kib' ~shadow_kib:(Some shadow_kib)
		          ~video_ram_kib ~vcpus ~timer_mode ~hpet ~vpt_align domid in
	let extras = [
		"-pae"; string_of_bool pae;
		"-viridian"; string_of_bool viridian;
	] in

	let store_mfn, console_mfn = restore_common ~xc ~xs ~hvm:true
	                                            ~store_port ~console_port
	                                            ~vcpus ~extras domid fd in
	let vm_stuff = [
		"rtc/timeoffset",    timeoffset;
	] in
	(* and finish domain's building *)
	build_post ~xc ~xs ~vcpus ~mem_target_kib ~mem_max_kib domid store_mfn store_port [] vm_stuff

let restore ~xc ~xs info domid fd =
	let restore_fct = match info.priv with
	| BuildHVM hvminfo ->
		hvm_restore ~video_ram_mib:hvminfo.videoram ~shadow_multiplier:hvminfo.shadow_multiplier
		            ~pae:hvminfo.pae ~viridian:hvminfo.viridian
		            ~timeoffset:hvminfo.timeoffset ~timer_mode:hvminfo.timer_mode
		            ~hpet:hvminfo.hpet ~vpt_align:hvminfo.vpt_align
	| BuildPV pvinfo   ->
		pv_restore
		in
	restore_fct ~xc ~xs
	            ~mem_max_kib:info.memory_max ~mem_target_kib:info.memory_target ~vcpus:info.vcpus
	            domid fd

let pause ~xc domid =
	Xc.domain_pause xc domid

let unpause ~xc domid =
	Xc.domain_unpause xc domid

let send_s3resume ~xc domid = Xc.domain_send_s3resume xc domid

let make_stubdom ~xc ~xs info uuid =
	let createinfo =
		{ ssidref = 0l
		; hvm = false
		; hap = false
		; name = sprintf "stubdom-%d" info.stubdom_target
		; xsdata = []
		; platformdata = [] } in
	let buildinfo =
		{ memory_max = Int64.of_int (32 * 1024)
		; memory_target = Int64.of_int (32 * 1024)
		; kernel = "ioemu-stubdom.gz" (* need path *)
		; vcpus = 1
		; priv = BuildPV
			{ cmdline = sprintf " -d %d" info.stubdom_target
			; ramdisk = None }
		} in
	let stubdom_domid = make ~xc ~xs createinfo uuid in
	build ~xc ~xs buildinfo stubdom_domid;

	(* write to the target where it can find it stubdom *)
	xs.Xs.write (xs.Xs.getdomainpath info.stubdom_target ^ "/image/device-model-domid") (sprintf "%d" stubdom_domid);
	(* write to the stubdom who's the target *)
	xs.Xs.write (xs.Xs.getdomainpath stubdom_domid ^ "/target") (sprintf "%d" info.stubdom_target);

	Xc.domain_set_target xc stubdom_domid info.stubdom_target;
	xs.Xs.set_target stubdom_domid info.stubdom_target;

	let perms = (stubdom_domid, Xsraw.PERM_NONE, [ (info.stubdom_target, Xsraw.PERM_READ) ]) in
	Xs.transaction xs (fun t ->
		let dmpath = sprintf "/local/domain/0/device-model/%d" info.stubdom_target in
		let vfspath = sprintf "/local/domain/%d/device/vfs" stubdom_domid in

		t.Xst.mkdir dmpath;
		t.Xst.setperms dmpath perms;
		t.Xst.mkdir vfspath;
		t.Xst.setperms vfspath perms;
	);

	stubdom_domid
