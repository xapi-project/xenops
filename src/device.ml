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
open Hashtblext
open Pervasiveext
open Listext

open Device_common

module D = Debug.Debugger(struct let name = "xenops" end)
open D

(****************************************************************************************)

module Generic = struct
(* this transactionally hvm:bool
           -> add entries to add a device
   specified by backend and frontend *)
let add_device ~xs device backend_list frontend_list =

	let frontend_path = frontend_path_of_device ~xs device
	and backend_path = backend_path_of_device ~xs device
	and hotplug_path = Hotplug.get_hotplug_path device in
	debug "adding device  B%d[%s]  F%d[%s]  H[%s]" device.backend.domid backend_path device.frontend.domid frontend_path hotplug_path;
	Xs.transaction xs (fun t ->
		begin try
			ignore (t.Xst.read frontend_path);
			if Xenbus.of_string (t.Xst.read (frontend_path ^ "/state"))
			   <> Xenbus.Closed then
				raise (Device_frontend_already_connected device)
		with Xb.Noent -> () end;

		t.Xst.rm frontend_path;
		t.Xst.rm backend_path;
		(* CA-16259: don't clear the 'hotplug_path' because this is where we
		   record our own use of /dev/loop devices. Clearing this causes us to leak
		   one per PV .iso *)

		t.Xst.mkdir frontend_path;
		t.Xst.setperms frontend_path (device.frontend.domid, Xsraw.PERM_NONE, [ (device.backend.domid, Xsraw.PERM_READ) ]);

		t.Xst.mkdir backend_path;
		t.Xst.setperms backend_path (device.backend.domid, Xsraw.PERM_NONE, [ (device.frontend.domid, Xsraw.PERM_READ) ]);

		t.Xst.mkdir hotplug_path;
		t.Xst.setperms hotplug_path (device.backend.domid, Xsraw.PERM_NONE, []);

		t.Xst.writev frontend_path
		             (("backend", backend_path) :: frontend_list);
		t.Xst.writev backend_path
		             (("frontend", frontend_path) :: backend_list)
	)

let safe_rm ~xs path =
  try 
    debug "xenstore-rm %s" path;
    xs.Xs.rm path
  with _ -> debug "Failed to xenstore-rm %s; continuing" path 

(* Helper function to delete the frontend, backend and error trees for a device.
   This must only be done after synchronising with the hotplug scripts.
   Cleaning up is best-effort; some of it might fail but as much will be
   done as possible. *)
let rm_device_state ~xs (x: device) =
	debug "Device.rm_device_state %s" (string_of_device x);
	safe_rm ~xs (frontend_path_of_device ~xs x);
	safe_rm ~xs (backend_path_of_device ~xs x);
	(* Cleanup the directory containing the error node *)
	safe_rm ~xs (Filename.dirname (error_path_of_device ~xs x))

let can_surprise_remove ~xs (x: device) =
  (* "(info key in xenstore) && 2" tells us whether a vbd can be surprised removed *)
        let key = backend_path_of_device ~xs x ^ "/info" in
	try
	  let info = Int64.of_string (xs.Xs.read key) in
	  (Int64.logand info 2L) <> 0L
	with _ -> false

(*
(* Assume we've told the backend to close. Watch both the error node and one other path.
   When the watch fires, call a predicate function and look for an error node.
   If an error node appears, throw Device_error. If the predicate returns true then
   return unit. If the timeout expires throw Device_disconnect_timeout. *)
let wait_for_error_or ~xs ?(timeout=Hotplug.hotplug_timeout) doc predicate otherpath domid kind devid = 
	let doc' = Printf.sprintf "%s (timeout = %f; %s)" doc timeout (print_device domid kind devid) in
  	let errorpath = error_node domid kind devid in
	debug "Device.wait_for_error_or %s (watching [ %s; %s ])" doc' otherpath errorpath;

	let finished = ref false and error = ref None in
	let callback watch =
		finished := predicate ();
		error := (try Some (xs.Xs.read errorpath) with Xb.Noent -> None);
		(* We return if the predicate is true of an error node has appeared *)
		!finished || !error <> None in
	begin try
		Xs.monitor_paths xs [ otherpath, "X";
				      errorpath, "X" ] timeout callback;
	with
		Xs.Timeout ->
			warn "Device.wait_for_error_or %s: timeout" doc';
			raise (Device_disconnect_timeout (domid, kind, devid))
	end;
	begin match !error with
	| Some error ->
		warn "Device.wait_for_error_or %s: failed: %s" doc' error;
		raise (Device_error (domid, kind, devid, error))
	| None ->
		debug "Device.wait_for_error_or %s: succeeded" doc'
	end

(** When destroying a whole domain, we blow away the frontend tree of individual devices.
    NB we only ever blow away the frontend (blowing away the backend risks resource leaks)
    NB we only ever blow away frontends of domUs which are being destroyed - we don't
    expect them to recover from this! *)
let destroy ~xs domid kind devid =
	let frontend_path = get_frontend_path ~xs domid kind devid in
	xs.Xs.rm frontend_path
*)
end

(****************************************************************************************)
module Tap2 = struct

exception Mount_failure of string * string * string
exception Unmount_failure of string * string

let string_of_unix_process process =
	match process with
	| Unix.WEXITED i -> sprintf "exited(%d)" i
	| Unix.WSIGNALED i -> sprintf "signaled(%d)" i
	| Unix.WSTOPPED i -> sprintf "stopped(%d)" i

(* call tapdisk2 and return the device path *)
let mount ty path keydir =
	let env = Array.append [| "TAPDISK2=/usr/sbin/tapdisk2" |] ( match keydir with
		| None     -> [| |]
		| Some dir -> [| "TAPDISK2_CRYPTO_KEYDIR="^dir |]
		)
	in
	let out, log =
		try Forkhelpers.execute_command_get_output ~withpath:true ~env "/usr/sbin/tap-ctl"
	                                        [ "create"; "-a"; sprintf "%s:%s" ty path; ]
		with Forkhelpers.Spawn_internal_error (log, output, status) ->
			let s = sprintf "output=%S status=%s" output (string_of_unix_process status) in
			raise (Mount_failure (ty, path, s))
		in
	let device_path = String.strip (fun c -> c = '\n') out in
	info "tap2: mounting %s:%S at %s" ty path device_path;
	device_path

let unmount device =
	info "tap2: unmounting %s" device;
	try Forkhelpers.execute_command_get_output ~withpath:true "/usr/sbin/tap-ctl"
					[ "destroy"; "-d"; device; ];
		()
	with Forkhelpers.Spawn_internal_error (log, output, status) ->
		let s = sprintf "output=%S status=%s" output (string_of_unix_process status) in
		raise (Unmount_failure (device, s))

end

(****************************************************************************************)
(** Disks:                                                                              *)
module Vbd = struct

let major_number_table = [| 3; 22; 33; 34; 56; 57; 88; 89; 90; 91 |]

(** Given a string device name, return the major and minor number *)
let device_major_minor name =
	(* This is the same algorithm xend uses: *)
	let a = int_of_char 'a' in
	(* Interpret as 'sda1', 'hda' etc *)
	try
		let number chars =
			if chars = [] then
				0
			else
			int_of_string (String.implode chars) in
		match String.explode name with
		| 's' :: 'd' :: ('a'..'p' as letter) :: rest ->
			8, 16 * (int_of_char letter - a) + (number rest)
		| 'x' :: 'v' :: 'd' :: ('a'..'p' as letter) :: rest ->
			202, 16 * (int_of_char letter - a) + (number rest)
		| 'h' :: 'd' :: ('a'..'t' as letter) :: rest ->
			let n = int_of_char letter - a in
			major_number_table.(n / 2), 64 * (n mod 2) + (number rest)
		| _ ->
			raise (Device_unrecognized name)
	with _ ->
		let file = if Filename.is_relative name then "/dev/" ^ name else name in
		Unixext.get_major_minor file

(** Given a major and minor number, return a device name *)
let major_minor_to_device (major, minor) =
	let a = int_of_char 'a' in
	let number x = if x = 0 then "" else string_of_int x in
	match major with
	| 8 -> Printf.sprintf "sd%c%s" (char_of_int (minor / 16 + a)) (number (minor mod 16))
	| 202 -> Printf.sprintf "xvd%c%s" (char_of_int (minor / 16 + a)) (number (minor mod 16))
	| x ->
	    (* Find the index of x in the table *)
	    let n = snd(Array.fold_left (fun (idx, result) n -> idx + 1, if x = n then idx else result) (0, -1) major_number_table) in
	    if n = -1 then failwith (Printf.sprintf "Couldn't determine device name for (%d, %d)" major minor)
	    else
	      let plus_one, minor = if minor >= 64 then 1, minor - 64 else 0, minor in
	      Printf.sprintf "hd%c%s" (char_of_int (n * 2 + plus_one + a)) (number minor)

(* Try 'int_of_string' *)

let device_number name =
	begin try
		let major, minor = device_major_minor name in
		256 * major + minor
	with _ ->
		try int_of_string name
		with _ -> raise (Device_unrecognized name)
	end

let device_name number =
	let major, minor = number / 256, number mod 256 in
	major_minor_to_device (major, minor)

type mode = ReadOnly | ReadWrite

let string_of_mode = function
	| ReadOnly -> "r"
	| ReadWrite -> "w"

let mode_of_string = function
	| "r" -> ReadOnly
	| "w" -> ReadWrite
	| s   -> invalid_arg "mode_of_string"

type lock = string

(** The format understood by blocktap *)
let string_of_lock lock mode = lock ^ ":" ^ (string_of_mode mode)

type physty = File | Phys | Qcow | Vhd | Aio

let backendty_of_physty = function
	| File -> "file"
	| Phys -> "phy"
	| Qcow -> "tap"
	| Vhd  -> "tap"
	| Aio  -> "tap"

let string_of_physty = function
	| Qcow -> "qcow"
	| Vhd  -> "vhd"
	| Aio  -> "aio"
	| File -> "file"
	| Phys -> "phys"

let physty_of_string s =
	match s with
	| "qcow" -> Qcow
	| "vhd"  -> Vhd
	| "aio"  -> Aio
	| "phy"  -> Phys
	| "file" -> File
	| _      -> invalid_arg "physty_of_string"

type devty = CDROM | Disk

let string_of_devty = function
	| CDROM -> "cdrom"
	| Disk  -> "disk"

let devty_of_string = function
	| "cdrom" -> CDROM
	| "disk"  -> Disk
	| _       -> invalid_arg "devty_of_string"

let string_of_major_minor file =
	let major, minor = device_major_minor file in
	sprintf "%x:%x" major minor

let kind_of_physty physty =
	match physty with
	| Qcow -> Tap
	| Vhd  -> Tap
	| Aio  -> Tap
	| Phys -> Vbd
	| File -> Vbd

let add_backend_keys ~xs (x: device) subdir keys =
	let backend_stub = backend_path_of_device ~xs x in
	let backend = backend_stub ^ "/" ^ subdir in
	debug "About to write data %s to path %s" (String.concat ";" (List.map (fun (a,b) -> "("^a^","^b^")") keys)) backend;
	Xs.transaction xs (fun t ->
		ignore(t.Xst.read backend_stub);
		t.Xst.writev backend keys
	)

let remove_backend_keys ~xs (x: device) subdir keys =
	let backend_stub = backend_path_of_device ~xs x in
	let backend = backend_stub ^ "/" ^ subdir in
	Xs.transaction xs (fun t ->
		List.iter (fun key -> t.Xst.rm (backend ^ "/" ^ key)) keys
	)


let uses_blktap ~phystype = List.mem phystype [ Qcow; Vhd; Aio ]

(** Request either a clean or hard shutdown *)
let request_shutdown ~xs (x: device) (force: bool) =
	let request = if force then "force" else "normal" in

	debug "Device.Vbd.request_shutdown %s %s" (string_of_device x) request;

	let backend_path = backend_path_of_device ~xs x in
	let request_path = backend_shutdown_request_path_of_device ~xs x in
	let online_path = backend_path ^ "/online" in

	(* Prevent spurious errors appearing by not writing online=0 if force *)
	if not(force) then begin
	  debug "xenstore-write %s = 0" online_path;
	  xs.Xs.write online_path "0";
	end;
	debug "xenstore-write %s = %s" request_path request;
	xs.Xs.write request_path request

(** Return the event to wait for when the shutdown has completed *)
let shutdown_done ~xs (x: device): string Watch.t = 
	Watch.value_to_appear (backend_shutdown_done_path_of_device ~xs x)

let hard_shutdown_request ~xs (x: device) = request_shutdown ~xs x true
let hard_shutdown_complete = shutdown_done

let clean_shutdown ~xs (x: device) =
	debug "Device.Vbd.clean_shutdown %s" (string_of_device x);

	request_shutdown ~xs x false; (* normal *)
	(* Allow the domain to reject the request by writing to the error node *)
	let shutdown_done = shutdown_done ~xs x in
	let error = Watch.value_to_appear (error_path_of_device ~xs x) in
	match Watch.wait_for ~xs (Watch.any_of [ `OK, shutdown_done; `Failed, error ]) with
	| `OK, _ ->
	    debug "Device.Vbd.shutdown_common: shutdown-done appeared";
	    (* Delete the trees (otherwise attempting to plug the device in again doesn't
	       work.) This also clears any stale error nodes. *)
	    Generic.rm_device_state ~xs x
	| `Failed, error ->
	    (* CA-14804: Delete the error node contents *)
	    Generic.safe_rm ~xs (error_path_of_device ~xs x);
	    debug "Device.Vbd.shutdown_common: read an error: %s" error;
	    raise (Device_error (x, error))

let hard_shutdown ~xs (x: device) = 
	debug "Device.Vbd.hard_shutdown %s" (string_of_device x);
	request_shutdown ~xs x true; (* force *)

	(* We don't watch for error nodes *)
	ignore_string (Watch.wait_for ~xs (shutdown_done ~xs x));
	Generic.rm_device_state ~xs x;

	debug "Device.Vbd.hard_shutdown complete"

let release ~xs (x: device) =
	debug "Device.Vbd.release %s" (string_of_device x);
	(* Make sure blktap/blkback fire the udev remove event by deleting the
	   backend now *)
	Generic.safe_rm ~xs (backend_path_of_device ~xs x);
	Hotplug.release ~xs x;
	(* As for add above, if the frontend is in dom0, we can wait for the frontend 
	 * to unplug as well as the backend. CA-13506 *)
	if x.frontend.domid = 0 then Hotplug.wait_for_frontend_unplug ~xs x

let pause ~xs (x: device) = 
	debug "Device.Vbd.pause %s" (string_of_device x);
	let request_path = backend_pause_request_path_of_device ~xs x in
	let response_path = backend_pause_done_path_of_device ~xs x in
	(* Both request and response should be clear *)
	if (try ignore(xs.Xs.read request_path); true with Xb.Noent -> false)
	then failwith (Printf.sprintf "xenstore path %s already exists" request_path);
	if (try ignore(xs.Xs.read response_path); true with Xb.Noent -> false)
	then failwith (Printf.sprintf "xenstore path %s already exists" response_path);

	debug "xenstore-write %s = \"\"" request_path;
	xs.Xs.write request_path "";

	ignore(Watch.wait_for ~xs (Watch.value_to_appear response_path));
	debug "Device.Vbd.pause %s complete" (string_of_device x)
  
let unpause ~xs (x: device) = 
	debug "Device.Vbd.unpause %s" (string_of_device x);
	let request_path = backend_pause_request_path_of_device ~xs x in
	let response_path = backend_pause_done_path_of_device ~xs x in
	(* Both request and response should exist *)
	if (try ignore(xs.Xs.read request_path); false with Xb.Noent -> true)
	then failwith (Printf.sprintf "xenstore path %s does not exist" request_path);
	if (try ignore(xs.Xs.read response_path); false with Xb.Noent -> true)
	then failwith (Printf.sprintf "xenstore path %s does not exist" response_path);

	debug "xenstore-rm %s" request_path;
	xs.Xs.rm request_path;

	Watch.wait_for ~xs (Watch.key_to_disappear response_path);
	debug "Device.Vbd.unpause %s complete" (string_of_device x)

(* Add the SCSI inquiry information for the standard inquiry and VPD page 0x80
   and 0x83. The front end can then report this informationt to the VM for
   example to present the same disk serial number. *)
let add_disk_info back_tbl physpath =
	(* TODO: the censored device is currently hard-coded to /dev/sda *)
	let physpath =
		match physpath with
		| "/dev/mapper/censored" -> "/dev/sda"
		| _                      -> physpath
		in
	try
		let std_inq = Scsi.scsi_inq_standard physpath 1 in
		let page80_inq = Scsi.scsi_inq_vpd physpath 0x80 in
		let page83_inq = Scsi.scsi_inq_vpd physpath 0x83 in
		
		if String.length std_inq > 0 && String.length page80_inq > 0 then (
			debug "Adding SCSI disk information.";
			Hashtbl.add back_tbl "sm-data/scsi/0x12/default" (Base64.encode std_inq);
			Hashtbl.add back_tbl "sm-data/scsi/0x12/0x80" (Base64.encode page80_inq);
			if String.length page83_inq > 0 then
				Hashtbl.add back_tbl "sm-data/scsi/0x12/0x83" (Base64.encode page83_inq)
		)
	with e ->
		warn "Caught exception during SCSI inquiry: %s" (Printexc.to_string e)

type info = {
	mode: mode;
	virtpath: string;
	phystype: physty;
	physpath: string;
	dev_type: devty;
	unpluggable: bool;
	info_pt: bool;
	extra_backend_keys: (string*string) list option;
}

(* Add the VBD to the domain, taking care of allocating any resources (specifically
   loopback mounts). When this command returns, the device is ready. (This isn't as
   concurrent as xend-- xend allocates loopdevices via hotplug in parallel and then
   performs a 'waitForDevices') *)
let add_struct ~xs ~hvm ?(protocol=Protocol_Native) ?(backend_domid=0) diskinfo domid  =
	let back_tbl = Hashtbl.create 16 and front_tbl = Hashtbl.create 16 in
	let devid = device_number diskinfo.virtpath in

	let backend_tap ty physpath =
		Hashtbl.add back_tbl "params" (ty ^ ":" ^ physpath);
		"tap", { domid = backend_domid; kind = Tap; devid = devid }
		in
	let backend_blk ty physpath =
		Hashtbl.add back_tbl "params" physpath;
		"vbd", { domid = backend_domid; kind = Vbd; devid = devid }
	in

	debug "Device.Vbd.add (virtpath=%s | physpath=%s | phystype=%s)"
	  diskinfo.virtpath diskinfo.physpath (string_of_physty diskinfo.phystype);
	(* Notes:
	   1. qemu accesses devices images itself and so needs the path of the original
              file (in params)
           2. when windows PV drivers initialise, the new blockfront connects to the
              up-til-now idle blockback and this requires the loop-device, in the file
	      case
           3. when the VM is fully PV, Ioemu devices do not work; all devices must be PV
	   4. in the future an HVM guest might support a mixture of both
	*)

	(match diskinfo.extra_backend_keys with
	 | Some keys ->
	     List.iter (fun (k, v) -> Hashtbl.add back_tbl k v) keys
	 | None -> ());

	let frontend = { domid = domid; kind = Vbd; devid = devid } in

	let backend_ty, backend = match diskinfo.phystype with
	| File ->
		(* Note: qemu access device images itself, so requires the path
		   of the original file or block device. CDROM media change is achieved
		   by changing the path in xenstore. Only PV guests need the loopback *)
		let backend_ty, backend = backend_blk "file" diskinfo.physpath in
		if not(hvm) then begin
		  let device = { backend = backend; frontend = frontend } in
		  let loopdev = Hotplug.mount_loopdev ~xs device diskinfo.physpath (diskinfo.mode = ReadOnly) in
		  Hashtbl.add back_tbl "physical-device" (string_of_major_minor loopdev);
		  Hashtbl.add back_tbl "loop-device" loopdev;
		end;
		backend_ty, backend
	| Phys ->
		Hashtbl.add back_tbl "physical-device" (string_of_major_minor diskinfo.physpath);
		if diskinfo.info_pt then
			add_disk_info back_tbl diskinfo.physpath;
		backend_blk "raw" diskinfo.physpath
	| Qcow | Vhd | Aio ->
		backend_tap (string_of_physty diskinfo.phystype) diskinfo.physpath
		in

	let device = { backend = backend; frontend = frontend } in
	

	Hashtbl.add_list front_tbl [
		"backend-id", string_of_int backend_domid;
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
		"virtual-device", string_of_int devid;
		"device-type", if diskinfo.dev_type = CDROM then "cdrom" else "disk";
	];
	Hashtbl.add_list back_tbl [
		"frontend-id", sprintf "%u" domid;
		(* Prevents the backend hotplug scripts from running if the frontend disconnects.
		   This allows the xenbus connection to re-establish itself *)
		"online", "1";
		"removable", if diskinfo.unpluggable then "1" else "0";
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
		(* HACK qemu wants a /dev/ in the dev field to find the device *)
		"dev", (if domid = 0 && diskinfo.virtpath.[0] = 'x' then "/dev/" else "") ^ diskinfo.virtpath;
		"type", backendty_of_physty diskinfo.phystype;
		"mode", string_of_mode diskinfo.mode;
	];
	if protocol <> Protocol_Native then
		Hashtbl.add front_tbl "protocol" (string_of_protocol protocol);

	let back = Hashtbl.to_list back_tbl in
	let front = Hashtbl.to_list front_tbl in

	Generic.add_device ~xs device back front;
	Hotplug.wait_for_plug ~xs device;
	(* 'Normally' we connect devices to other domains, and cannot know whether the
	   device is 'available' from their userspace (or even if they have a userspace).
	   The best we can do is just to wait for the backend hotplug scripts to run,
	   indicating that the backend has locked the resource.
	   In the case of domain 0 we can do better: we have custom hotplug scripts
	   which call us back when the device is actually available to userspace. We need
	   to wait for this condition to make the template installers work.
	   NB if the custom hotplug script fires this implies that the xenbus state
	   reached "connected", so we don't have to check for that first. *)
	if domid = 0 then begin
	  try
	    (* CA-15605: clean up on dom0 block-attach failure *)
	    Hotplug.wait_for_frontend_plug ~xs device;
	  with Hotplug.Frontend_device_error _ as e ->
	    debug "Caught Frontend_device_error: assuming it is safe to shutdown the backend";
	    clean_shutdown ~xs device; (* assumes double-failure isn't possible *)
	    release ~xs device;
	    raise e
	end;
	device

(* for compat *)
let add ~xs ~hvm ~mode ~virtpath ~phystype ~physpath ~dev_type ~unpluggable ~diskinfo_pt
        ?protocol ?extra_backend_keys ?backend_domid domid =
	let diskinfo = {
		mode = mode; virtpath = virtpath; phystype = phystype; physpath = physpath;
		dev_type = dev_type; unpluggable = unpluggable;
		info_pt = diskinfo_pt; extra_backend_keys = extra_backend_keys } in
	add_struct ~xs ~hvm ?protocol ?backend_domid diskinfo domid

let qemu_media_change ~xs ~virtpath domid _type params =
	let devid = device_number virtpath in
	let back_dom_path = xs.Xs.getdomainpath 0 in
	let backend  = sprintf "%s/backend/vbd/%u/%d" back_dom_path domid devid in
	let back_delta = [
		"type",           _type;
		"params",         params;
	] in
	Xs.transaction xs (fun t -> t.Xst.writev backend back_delta);
	debug "Media changed"

let media_tray_is_locked ~xs ~virtpath domid =
  let devid = device_number virtpath in
  let backend = { domid = 0; kind = Vbd; devid = devid } in
  let path = sprintf "%s/locked" (backend_path ~xs backend domid) in
    try
      xs.Xs.read path = "true"
    with _ ->
      false

let media_eject ~xs ~virtpath domid =
	qemu_media_change ~xs ~virtpath domid "" ""

let media_insert ~xs ~virtpath ~physpath ~phystype domid =
	let _type = backendty_of_physty phystype
	and params = physpath in
	qemu_media_change ~xs ~virtpath domid _type params

let media_refresh ~xs ~virtpath ~physpath domid =
	let devid = device_number virtpath in
	let back_dom_path = xs.Xs.getdomainpath 0 in
	let backend = sprintf "%s/backend/vbd/%u/%d" back_dom_path domid devid in
	let path = backend ^ "/params" in
	(* unfortunately qemu filter the request if on the same string it has,
	   so we trick it by having a different string, but the same path, adding a
	   spurious '/' character at the beggining of the string.  *)
	let oldval = try xs.Xs.read path with _ -> "" in
	let pathtowrite =
		if oldval = physpath then (
			"/" ^ physpath
		) else
			physpath in
	xs.Xs.write path pathtowrite;
	()

let media_is_ejected ~xs ~virtpath domid =
	let devid = device_number virtpath in
	let back_dom_path = xs.Xs.getdomainpath 0 in
	let backend = sprintf "%s/backend/vbd/%u/%d" back_dom_path domid devid in
	let path = backend ^ "/params" in
	try xs.Xs.read path = "" with _ -> true

end

(****************************************************************************************)
(** VIFs:                                                                               *)

(**
   Generate a random MAC address, using OUI (Organizationally Unique
   Identifier) 00-16-3E, allocated to Xensource, Inc.

   The remaining 3 fields are random, with the first bit of the first random
   field set 0.
 *)

module Vif = struct

type info = {
	vifid: int; (* devid *)
	netty: Netman.netty;
	mac: string;
	mtu: int option;
	rate: (int64 * int64) option;
}

exception Invalid_Mac of string

let check_mac mac =
        try
                if String.length mac <> 17 then failwith "mac length";
	        Scanf.sscanf mac "%2x:%2x:%2x:%2x:%2x:%2x" (fun a b c d e f -> ());
	        mac
        with _ ->
		raise (Invalid_Mac mac)

let get_backend_dev ~xs (x: device) =
        try
		let path = Hotplug.get_hotplug_path x in
		xs.Xs.read (path ^ "/vif")
	with Xb.Noent ->
		raise (Hotplug_script_expecting_field (x, "vif"))

(** Plug in the backend of a guest's VIF in dom0. Note that a guest may disconnect and
    then reconnect their network interface: we have to re-run this code every time we
    see a hotplug online event. *)
let plug ~xs ~netty ~mac ?(mtu=0) ?rate ?protocol (x: device) =
	let backend_dev = get_backend_dev xs x in

	if mtu > 0 then
		Netdev.set_mtu backend_dev mtu;
	Netman.online ~xs (backend_path_of_device ~xs x) backend_dev netty;

	(* set <backend>/hotplug-status = connected to interact nicely with the
	   xs-xen.pq.hq:91e986b8e49f netback-wait-for-hotplug patch *)
	xs.Xs.write (Hotplug.connected_node ~xs x) "connected";

	x

let add_struct ~xs ?(protocol=Protocol_Native) ?(backend_domid=0) vifinfo domid =
	debug "Device.Vif.add domid=%d devid=%d mac=%s rate=%s" domid vifinfo.vifid vifinfo.mac
	      (match vifinfo.rate with None -> "none" | Some (a, b) -> sprintf "(%Ld,%Ld)" a b);
	let frontend = { domid = domid; kind = Vif; devid = vifinfo.vifid } in
	let backend = { domid = backend_domid; kind = Vif; devid = vifinfo.vifid } in
	let device = { backend = backend; frontend = frontend } in

	let mac = check_mac vifinfo.mac in

	let back_options =
		match vifinfo.rate with
		| None                              -> []
		| Some (kbytes_per_s, timeslice_us) ->
			let (^*) = Int64.mul and (^/) = Int64.div in
			let timeslice_us =
				if timeslice_us > 0L then
					timeslice_us
				else
					50000L (* 50ms by default *) in
			let bytes_per_interval = ((kbytes_per_s ^* 1024L) ^* timeslice_us)
			                         ^/ 1000000L in
			if bytes_per_interval > 0L && bytes_per_interval < 0xffffffffL then
				[ "rate", sprintf "%Lu,%Lu" bytes_per_interval timeslice_us ]
			else (
				debug "VIF qos: invalid value for byte/interval: %Lu" bytes_per_interval;
				[]
			)
		in

	let back = [
		"frontend-id", sprintf "%u" domid;
		"online", "1";
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
		"script", "/etc/xensource/scripts/vif";
		"mac", mac;
		"handle", string_of_int vifinfo.vifid
	] @ back_options in

	let front_options =
		if protocol <> Protocol_Native then
			[ "protocol", string_of_protocol protocol; ]
		else
			[] in

	let front = [
		"backend-id", string_of_int backend_domid;
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
		"handle", string_of_int vifinfo.vifid;
		"mac", mac;
	] @ front_options in

	Generic.add_device ~xs device back front;
	Hotplug.wait_for_plug ~xs device;
	plug ~xs ~netty:vifinfo.netty ~mac ~rate:vifinfo.rate ?mtu:vifinfo.mtu device

let add ~xs ~devid ~netty ~mac ?mtu ?(rate=None) ?(protocol=Protocol_Native) ?(backend_domid=0) domid =
	let vifinfo = { vifid = devid; netty = netty; mac = mac; mtu = mtu; rate = rate; } in
	add_struct ~xs ~protocol ~backend_domid vifinfo domid

(** When hot-unplugging a device we ask nicely *)
let request_closure ~xs (x: device) =
	let backend_path = backend_path_of_device ~xs x in
	let state_path = backend_path ^ "/state" in
	Xs.transaction xs (fun t ->
		let online_path = backend_path ^ "/online" in
		debug "xenstore-write %s = 0" online_path;
		t.Xst.write online_path "0";
		let state = try Xenbus.of_string (t.Xst.read state_path) with _ -> Xenbus.Closed in
		if state == Xenbus.Connected then (
			debug "Device.del_device setting backend to Closing";
			t.Xst.write state_path (Xenbus.string_of Xenbus.Closing);
		)
	)

let unplug_watch ~xs (x: device) = Watch.map (fun () -> "") (Watch.key_to_disappear (Hotplug.status_node x))
let error_watch ~xs (x: device) = Watch.value_to_appear (error_path_of_device ~xs x) 

let clean_shutdown ~xs (x: device) =
	debug "Device.Vif.clean_shutdown %s" (string_of_device x);

	request_closure ~xs x;
	match Watch.wait_for ~xs (Watch.any_of [ `OK, unplug_watch ~xs x; `Failed, error_watch ~xs x ]) with
	| `OK, _ ->
	    (* Delete the trees (otherwise attempting to plug the device in again doesn't
	       work. This also clears any stale error nodes. *)
	    Generic.rm_device_state ~xs x
	| `Failed, error ->
	    debug "Device.Vif.shutdown_common: read an error: %s" error;
	    raise (Device_error (x, error))	

let hard_shutdown ~xs (x: device) =
	debug "Device.Vif.hard_shutdown %s" (string_of_device x);

	let backend_path = backend_path_of_device ~xs x in
	let online_path = backend_path ^ "/online" in
	debug "xenstore-write %s = 0" online_path;
	xs.Xs.write online_path "0";
	(* blow away the frontend *)
	debug "Device.Vif.hard_shutdown about to blow away frontend";
	let frontend_path = frontend_path_of_device ~xs x in
	xs.Xs.rm frontend_path;

	ignore(Watch.wait_for ~xs (unplug_watch ~xs x))

let release ~xs (x: device) =
	debug "Device.Vif.release %s" (string_of_device x);
	Hotplug.release ~xs x
end

(****************************************************************************************)
(** VWIFs:                                                                              *)
module Vwif = struct

exception Invalid_Mac of string

let check_mac mac =
        try
                if String.length mac <> 17 then failwith "mac length";
	        Scanf.sscanf mac "%2x:%2x:%2x:%2x:%2x:%2x" (fun a b c d e f -> ());
	        mac
        with _ ->
		raise (Invalid_Mac mac)

let get_backend_dev ~xs (x: device) =
        try
		let path = Hotplug.get_hotplug_path x in
		xs.Xs.read (path ^ "/vif")
	with Xb.Noent ->
		raise (Hotplug_script_expecting_field (x, "vif"))

(** Plug in the backend of a guest's VIF in dom0. Note that a guest may disconnect and
    then reconnect their network interface: we have to re-run this code every time we
    see a hotplug online event. *)
let plug ~xs ~netty ~mac ?(mtu=0) ?rate ?protocol (x: device) =
	let backend_dev = get_backend_dev xs x in

	if mtu > 0 then
		Netdev.set_mtu backend_dev mtu;
	Netman.online ~xs (backend_path_of_device ~xs x) backend_dev netty;

	(* set <backend>/hotplug-status = connected to interact nicely with the
	   xs-xen.pq.hq:91e986b8e49f netback-wait-for-hotplug patch *)
	xs.Xs.write (Hotplug.connected_node ~xs x) "connected";

	x


let add ~xs ~devid ~netty ~mac ?mtu ?(rate=None) ?(protocol=Protocol_Native) ?(backend_domid=0) domid =
	debug "Device.Vwif.add domid=%d devid=%d mac=%s rate=%s" domid devid mac
	      (match rate with None -> "none" | Some (a, b) -> sprintf "(%Ld,%Ld)" a b);
	let frontend = { domid = domid; kind = Vwif; devid = devid } in
	let backend = { domid = backend_domid; kind = Vif; devid = devid } in
	let device = { backend = backend; frontend = frontend } in

	let mac = check_mac mac in

	let back_options =
		match rate with
		| None                              -> []
		| Some (kbytes_per_s, timeslice_us) ->
			let (^*) = Int64.mul and (^/) = Int64.div in
			let timeslice_us =
				if timeslice_us > 0L then
					timeslice_us
				else
					50000L (* 50ms by default *) in
			let bytes_per_interval = ((kbytes_per_s ^* 1024L) ^* timeslice_us)
			                         ^/ 1000000L in
			if bytes_per_interval > 0L && bytes_per_interval < 0xffffffffL then
				[ "rate", sprintf "%Lu,%Lu" bytes_per_interval timeslice_us ]
			else (
				debug "VIF qos: invalid value for byte/interval: %Lu" bytes_per_interval;
				[]
			)
		in

	let back = [
		"frontend-id", sprintf "%u" domid;
		"online", "1";
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
		"script", "/etc/xensource/scripts/vif";
		"mac", mac;
		"handle", string_of_int devid
	] @ back_options in

	let front_options =
		if protocol <> Protocol_Native then
			[ "protocol", string_of_protocol protocol; ]
		else
			[] in

	let front = [
		"backend-id", string_of_int backend_domid;
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
		"handle", string_of_int devid;
		"mac", mac
	] @ front_options in


	Generic.add_device ~xs device back front;
	Hotplug.wait_for_plug ~xs device;
	plug ~xs ~netty ~mac ?rate ?mtu device

(** When hot-unplugging a device we ask nicely *)
let request_closure ~xs (x: device) =
	let backend_path = backend_path_of_device ~xs x in
	let state_path = backend_path ^ "/state" in
	Xs.transaction xs (fun t ->
		let online_path = backend_path ^ "/online" in
		debug "xenstore-write %s = 0" online_path;
		t.Xst.write online_path "0";
		let state = try Xenbus.of_string (t.Xst.read state_path) with _ -> Xenbus.Closed in
		if state == Xenbus.Connected then (
			debug "Device.del_device setting backend to Closing";
			t.Xst.write state_path (Xenbus.string_of Xenbus.Closing);
		)
	)

let unplug_watch ~xs (x: device) = Watch.map (fun () -> "") (Watch.key_to_disappear (Hotplug.status_node x))
let error_watch ~xs (x: device) = Watch.value_to_appear (error_path_of_device ~xs x) 

let clean_shutdown ~xs (x: device) =
	debug "Device.Vwif.clean_shutdown %s" (string_of_device x);

	request_closure ~xs x;
	match Watch.wait_for ~xs (Watch.any_of [ `OK, unplug_watch ~xs x; `Failed, error_watch ~xs x ]) with
	| `OK, _ ->
	    (* Delete the trees (otherwise attempting to plug the device in again doesn't
	       work. This also clears any stale error nodes. *)
	    Generic.rm_device_state ~xs x
	| `Failed, error ->
	    debug "Device.Vwif.shutdown_common: read an error: %s" error;
	    raise (Device_error (x, error))	

let hard_shutdown ~xs (x: device) =
	debug "Device.Vwif.hard_shutdown %s" (string_of_device x);

	let backend_path = backend_path_of_device ~xs x in
	let online_path = backend_path ^ "/online" in
	debug "xenstore-write %s = 0" online_path;
	xs.Xs.write online_path "0";
	(* blow away the frontend *)
	debug "Device.Vwif.hard_shutdown about to blow away frontend";
	let frontend_path = frontend_path_of_device ~xs x in
	xs.Xs.rm frontend_path;

	ignore(Watch.wait_for ~xs (unplug_watch ~xs x))

let release ~xs (x: device) =
	debug "Device.Vwif.release %s" (string_of_device x);
	Hotplug.release ~xs x
end

(*****************************************************************************)
(** Vcpus:                                                                   *)
module Vcpu = struct

let add ~xs ~devid domid =
	let path = sprintf "/local/domain/%d/cpu/%d" domid devid in
	xs.Xs.writev path [
		"availability", "online"
	]

let del ~xs ~devid domid =
	let path = sprintf "/local/domain/%d/cpu/%d" domid devid in
	xs.Xs.rm path

let set ~xs ~devid domid online =
	let path = sprintf "/local/domain/%d/cpu/%d/availability" domid devid in
	xs.Xs.write path (if online then "online" else "offline")

let status ~xs ~devid domid =
	let path = sprintf "/local/domain/%d/cpu/%d/availability" domid devid in
	try match xs.Xs.read path with
	| "online"  -> true
	| "offline" -> false
	| _         -> (* garbage, assuming false *) false
	with Xb.Noent -> false

end

module PV_Vnc = struct

let vncterm_wrapper = "/opt/xensource/libexec/vncterm-wrapper"

let path domid = sprintf "/local/domain/%d/serial/0/vnc-port" domid

exception Failed_to_start

let start ~xs domid =
	let l = [ string_of_int domid; (* absorbed by vncterm-wrapper *)
		  (* everything else goes straight through to vncterm-wrapper: *)
		  "-x"; sprintf "/local/domain/%d/serial/0" domid;
		] in
	(* Now add the close fds wrapper *)
	let cmdline = Forkhelpers.close_and_exec_cmdline [] vncterm_wrapper l in
	debug "Executing [ %s ]" (String.concat " " cmdline);

	let argv_0 = List.hd cmdline and argv = Array.of_list cmdline in
	Unixext.double_fork (fun () ->
		Sys.set_signal Sys.sigint Sys.Signal_ignore;

		Unix.execvp argv_0 argv
	);
	(* Block waiting for it to write the VNC port into the store *)
	try
	  let port = Watch.wait_for ~xs (Watch.value_to_appear (path domid)) in
	  debug "vncterm: wrote vnc port %s into the store" port;
	  int_of_string port
	with Watch.Timeout _ ->
	  warn "vncterm: Timed out waiting for vncterm to start";
	  raise Failed_to_start

end

module PCI = struct

type desc = { domain: int; bus: int; slot: int; func: int }

type resources = {
	irq: int;
	memaddr: (int64 * int64 * int64) list;
	driver: string;
}

type dev = {
	desc: desc;
	guest_slot: int option;
	msitranslate: int;
	power_mgmt: int;
	vdevfn: int option;
}

let string_of_desc desc = sprintf "%04x:%02x:%02x.%1x" desc.domain desc.bus desc.slot desc.func

let string_of_dev dev =
	let at = match dev.guest_slot with
	| None -> ""
	| Some slot -> sprintf "@%02x" slot
	in
	string_of_desc dev.desc ^ at

let dev_of_string devstr =
	try
		Scanf.sscanf devstr "%04x:%02x:%02x.%1x@%02x" (fun a b c d e -> { desc = { domain = a; bus = b; slot = c; func = d }; guest_slot = Some e; msitranslate = 0; power_mgmt = 0; vdevfn = None })
	with _ ->
		Scanf.sscanf devstr "%04x:%02x:%02x.%1x" (fun a b c d -> { desc = { domain = a; bus = b; slot = c; func = d }; guest_slot = None; msitranslate = 0; power_mgmt = 0; vdevfn = None })

exception Cannot_add of dev list * exn (* devices, reason *)
exception Cannot_use_pci_with_no_pciback of (dev * resources) list

let get_from_system desc =
	let map_resource file =
		let resources = Array.create 7 (0L, 0L, 0L) in
		let i = ref 0 in
		Unixext.readfile_line (fun line ->
			if !i < Array.length resources then (
				Scanf.sscanf line "0x%Lx 0x%Lx 0x%Lx" (fun s e f ->
					resources.(!i) <- (s, e, f));
				incr i
			)
		) file;
		List.filter (fun (s, _, _) -> s <> 0L) (Array.to_list resources);
		in
	let map_irq file =
		let irq = ref (-1) in
		try Unixext.readfile_line (fun line -> irq := int_of_string line) file; !irq
		with _ -> -1
		in
		
	let name = string_of_desc desc in
	let dir = "/sys/bus/pci/devices/" ^ name in
	let resources = map_resource (dir ^ "/resource") in
	let irq = map_irq (dir ^ "/irq") in
	let driver =
		try Filename.basename (Unix.readlink (dir ^ "/driver"))
		with _ -> "" in
	irq, resources, driver

let passthrough_mmio ~xc domid (startreg, endreg) enable =
	if endreg < startreg then
		failwith "mmio end region invalid";
	let action = if enable then "add" else "remove" in
	let mem_to_pfn m = Int64.to_nativeint (Int64.div m 4096L) in
	let first_pfn = mem_to_pfn startreg and end_pfn = mem_to_pfn endreg in
	let nr_pfns = Nativeint.add (Nativeint.sub end_pfn first_pfn) 1n in

	debug "mmio %s %Lx-%Lx" action startreg endreg;
	Xc.domain_iomem_permission xc domid first_pfn nr_pfns enable

let passthrough_io ~xc domid (startport, endport) enable =
	if endport < startport then
		failwith "io end port invalid";
	let action = if enable then "add" else "remove" in
	let nr_ports = endport - startport + 1 in
	debug "mmio %s %x-%x" action startport endport;
	Xc.domain_ioport_permission xc domid startport nr_ports enable

let grant_access_resources xc domid resources v =
	let action = if v then "add" else "remove" in
	let constant_PCI_BAR_IO = 0x01L in
	List.iter (fun (s, e, flags) ->
		if Int64.logand flags constant_PCI_BAR_IO = constant_PCI_BAR_IO then (
			let first_port = Int64.to_int s in
			let nr_ports = (Int64.to_int e) - first_port + 1 in

			debug "pci %s io bar %Lx-%Lx" action s e;
			Xc.domain_ioport_permission xc domid first_port nr_ports v
		) else (
			let mem_to_pfn m = Int64.to_nativeint (Int64.div m 4096L) in
			let first_pfn = mem_to_pfn s and end_pfn = mem_to_pfn e in
			let nr_pfns = Nativeint.add (Nativeint.sub end_pfn first_pfn) 1n in

			debug "pci %s mem bar %Lx-%Lx" action s e;
			Xc.domain_iomem_permission xc domid first_pfn nr_pfns v
		)
	) resources

let add_noexn ~xc ~xs ~hvm ?(flrscript=None) pcidevs domid devid =
	let msitranslate = (List.hd pcidevs).msitranslate in
	let power_mgmt   = (List.hd pcidevs).power_mgmt in
	let pcidevs = List.map (fun info ->
		let (irq, memaddr, driver) = get_from_system info.desc in
		info, { irq = irq; memaddr = memaddr; driver = driver; }
	) pcidevs in

	let baddevs = List.filter (fun (info, resources) -> resources.driver <> "pciback") pcidevs in
	if List.length baddevs > 0 then (
		raise (Cannot_use_pci_with_no_pciback baddevs);
	);

	List.iter (fun (dev, resources) ->
		if hvm then (
			ignore_bool (Xc.domain_test_assign_device xc domid (dev.desc.domain, dev.desc.bus, dev.desc.slot, dev.desc.func));
			()
		);
		grant_access_resources xc domid resources.memaddr true;
		if resources.irq > 0 then
			Xc.domain_irq_permission xc domid resources.irq true
	) pcidevs;

	let device = {
		backend = { domid = 0; kind = Pci; devid = devid };
		frontend = { domid = domid; kind = Pci; devid = devid };
	} in

	let others = (match flrscript with None -> [] | Some script -> [ ("script", script) ]) in
	let xsdevs = List.mapi (fun i (dev,_) ->
		[ sprintf "dev-%d" i, string_of_dev dev ] @
		(match dev.vdevfn with None -> [] | Some n -> [ sprintf "vdevfn-%d" i, string_of_int n ])
	) pcidevs in

	let backendlist = [
		"frontend-id", sprintf "%u" domid;
		"online", "1";
		"num_devs", string_of_int (List.length xsdevs);
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
		"msitranslate", string_of_int (msitranslate);
		"pci_power_mgmt", string_of_int (power_mgmt);
	] and frontendlist = [
		"backend-id", "0";
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
	] in
	Generic.add_device ~xs device (others @ List.concat xsdevs @ backendlist) frontendlist;
	()

let add ~xc ~xs ~hvm ?flrscript pcidevs domid devid =
	try add_noexn ~xc ~xs ~hvm ?flrscript pcidevs domid devid
	with exn ->
		raise (Cannot_add (pcidevs, exn))

let release ~xc ~xs ~hvm pcidevs domid devid =
	let pcidevs = List.map (fun dev ->
		let (irq, memaddr, driver) = get_from_system dev.desc in
		dev, { irq = irq; memaddr = memaddr; driver = driver; }
	) pcidevs in

	let baddevs = List.filter (fun (_, resources) -> resources.driver <> "pciback" && resources.driver <> "") pcidevs in
	if List.length baddevs > 0 then (
		raise (Cannot_use_pci_with_no_pciback baddevs);
	);

	List.iter (fun (_, resources) ->
		grant_access_resources xc domid resources.memaddr false;
		if resources.irq > 0 then
			Xc.domain_irq_permission xc domid resources.irq false
	) pcidevs;
	()

let write_string_to_file file s =
	let fn_write_string fd = Unixext.really_write fd s 0 (String.length s) in
	Unixext.with_file file [ Unix.O_WRONLY ] 0o640 fn_write_string

let do_flr device =
	let script = "/opt/xensource/libexec/pci-flr" in
	let callscript =
                let f s devstr =
	                try ignore (Forkhelpers.execute_command_get_output ~withpath:true script [ s; devstr; ])
			        with _ -> ()
			in
			f
		in
        callscript "" device

let bind pcidevs =
	let bind_to_pciback device =
		let newslot = "/sys/bus/pci/drivers/pciback/new_slot" in
		let bind = "/sys/bus/pci/drivers/pciback/bind" in
		write_string_to_file newslot device;
		write_string_to_file bind device;
		do_flr device;
		in
	List.iter (fun dev ->
		let devstr = string_of_desc dev.desc in
		let s = "/sys/bus/pci/devices/" ^ devstr in
		let driver =
			try Some (Filename.basename (Unix.readlink (s ^ "/driver")))
			with _ -> None in
		begin match driver with
		| None           ->
			bind_to_pciback devstr
		| Some "pciback" ->
			debug "pci: device %s already bounded to pciback" devstr;
                        do_flr devstr
		| Some d         ->
			debug "pci: unbounding device %s from driver %s" devstr d;
			let f = s ^ "/driver/unbind" in
			write_string_to_file f devstr;
			bind_to_pciback devstr
		end;
	) pcidevs;
	()

let enumerate_devs ~xs (x: device) =
	let backend_path = backend_path_of_device ~xs x in
	let num =
		try int_of_string (xs.Xs.read (backend_path ^ "/num_devs"))
		with _ -> 0
		in
	let devs = Array.make num None in
	for i = 0 to num
	do
		try
			let devstr = xs.Xs.read (backend_path ^ "/dev-" ^ (string_of_int i)) in
			let dev = dev_of_string devstr in
			devs.(i) <- Some dev
		with _ ->
			()
	done;
	List.rev (List.fold_left (fun acc dev ->
		match dev with
		| None -> acc
		| Some dev -> dev :: acc
	) [] (Array.to_list devs))

let reset ~xs (x: device) =
	debug "Device.Pci.reset %s" (string_of_device x);
	let pcidevs = enumerate_devs ~xs x in
	List.iter (fun dev ->
		let devstr = string_of_desc dev.desc in
		do_flr devstr
	) pcidevs;
	()

let clean_shutdown ~xs (x: device) =
	debug "Device.Pci.clean_shutdown %s" (string_of_device x);
	let devs = enumerate_devs ~xs x in
	Xc.with_intf (fun xc ->
		let hvm =
			try (Xc.domain_getinfo xc x.frontend.domid).Xc.hvm_guest
			with _ -> false
			in
		try release ~xc ~xs ~hvm devs x.frontend.domid x.frontend.devid
		with _ -> ());
	()

let hard_shutdown ~xs (x: device) =
	debug "Device.Pci.hard_shutdown %s" (string_of_device x);
	clean_shutdown ~xs x

let signal_device_model ~xc ~xs domid cmd parameter =
	debug "Device.Pci.signal_device_model domid=%d cmd=%s param=%s" domid cmd parameter;
	let dom0 = xs.Xs.getdomainpath 0 in (* XXX: assume device model is in domain 0 *)
	Xs.transaction xs (fun t ->
		t.Xst.writev dom0 [ Printf.sprintf "device-model/%d/command" domid, cmd;
				    Printf.sprintf "device-model/%d/parameter" domid, parameter ]
	);
	(* XXX: no response protocol *)
	()

let plug ~xc ~xs device domid devid =
	signal_device_model ~xc ~xs domid "pci-ins" (string_of_dev device)

let unplug ~xc ~xs device domid devid =
	signal_device_model ~xc ~xs domid "pci-rem" (string_of_dev device)

end

module Vfb = struct

let add ~xc ~xs ~hvm ?(protocol=Protocol_Native) domid =
	debug "Device.Vfb.add %d" domid;

	let frontend = { domid = domid; kind = Vfb; devid = 0 } in
	let backend = { domid = 0; kind = Vfb; devid = 0 } in
	let device = { backend = backend; frontend = frontend } in

	let back = [
		"frontend-id", sprintf "%u" domid;
		"online", "1";
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
	] in
	let front = [
		"backend-id", string_of_int 0;
		"protocol", (string_of_protocol protocol);
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
	] in
	Generic.add_device ~xs device back front;
	()

let hard_shutdown ~xs (x: device) =
	debug "Device.Vfb.hard_shutdown %s" (string_of_device x);
	()

let clean_shutdown ~xs (x: device) =
	debug "Device.Vfb.clean_shutdown %s" (string_of_device x);
	()

end

module Vkb = struct

let add ~xc ~xs ~hvm ?(protocol=Protocol_Native) domid =
	debug "Device.Vkb.add %d" domid;

	let frontend = { domid = domid; kind = Vkb; devid = 0 } in
	let backend = { domid = 0; kind = Vkb; devid = 0 } in
	let device = { backend = backend; frontend = frontend } in

	let back = [
		"frontend-id", sprintf "%u" domid;
		"online", "1";
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
	] in
	let front = [
		"backend-id", string_of_int 0;
		"protocol", (string_of_protocol protocol);
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
	] in
	Generic.add_device ~xs device back front;
	()

let hard_shutdown ~xs (x: device) =
	debug "Device.Vkb.hard_shutdown %s" (string_of_device x);
	()

let clean_shutdown ~xs (x: device) =
	debug "Device.Vkb.clean_shutdown %s" (string_of_device x);
	()

end

module V4V = struct

let add ~xc ~xs ~hvm domid =
	debug "Device.V4V.add %d" domid;

	let frontend = { domid = domid; kind = V4V; devid = 0 } in
	let backend  = { domid = 0    ; kind = V4V; devid = 0 } in
	let device   = { backend = backend; frontend = frontend } in
	let back = [
		"frontend-id", sprintf "%u" domid;
		"state", string_of_int (Xenbus.int_of Xenbus.Unknown);
	] in
	let front = [
		"backend-id", string_of_int 0;
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
	] in
	Generic.add_device ~xs device back front;
	()

let hard_shutdown ~xs (x: device) =
	debug "Device.V4V.hard_shutdown %s" (string_of_device x);
	()

let clean_shutdown ~xs (x: device) =
	debug "Device.V4V.clean_shutdown %s" (string_of_device x);
	()
end

module Console = struct

type consback = XenConsoled | Ioemu
exception Invalid_console_type

let add ~xs ~hvm ?(protocol=Protocol_Native) ?(backend_domid=0) ~output ~consback ~devid domid =
	debug "Device.Console.add %d" domid;
	if not hvm && consback == Ioemu
		then raise Invalid_console_type;

	let frontend = { domid = domid; kind = Console; devid = devid } in
	let backend = { domid = 0; kind = Console; devid = devid } in
	let device = { backend = backend; frontend = frontend } in

	let back = [
		"frontend-id", sprintf "%u" domid;
		"online", "1";
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
		"domain", "some name";
		"protocol", "vt100";
	] in
	(* with devid == 0 add to console not device/console *)
	let front = if devid > 0 then
	[
		"backend-id", string_of_int backend_domid;
		"protocol", (string_of_protocol protocol);
		"state", string_of_int (Xenbus.int_of Xenbus.Initialising);
		"limit", "0";
		"protocol", "vt100";
		"type", if consback = XenConsoled then "xenconsoled" else "ioemu";
		"output", output;
	] else [] in
	Generic.add_device ~xs device back front;
	()

let hard_shutdown ~xs (x: device) =
	debug "Device.Console.hard_shutdown %s" (string_of_device x);
	()

let clean_shutdown ~xs (x: device) =
	debug "Device.Console.clean_shutdown %s" (string_of_device x);
	()

end

let hard_shutdown ~xs (x: device) = match (x.backend.kind, x.frontend.kind) with
  | Vif,_ -> Vif.hard_shutdown ~xs x
  | Vwif,_ -> Vwif.hard_shutdown ~xs x
  | _,Vwif -> Vwif.hard_shutdown ~xs x
  | Vbd,_ | Tap,_ -> Vbd.hard_shutdown ~xs x
  | Pci,_ -> PCI.hard_shutdown ~xs x
  | Vfb,_ -> Vfb.hard_shutdown ~xs x
  | Vkb,_ -> Vkb.hard_shutdown ~xs x
  | V4V,_ -> V4V.hard_shutdown ~xs x
  | Console,_ -> Console.hard_shutdown ~xs x

let clean_shutdown ~xs (x: device) = match (x.backend.kind, x.frontend.kind) with
  | Vif,_ -> Vif.clean_shutdown ~xs x
  | Vwif,_ -> Vwif.clean_shutdown ~xs x
  | _,Vwif -> Vwif.clean_shutdown ~xs x
  | Vbd,_ | Tap,_ -> Vbd.clean_shutdown ~xs x
  | Pci,_ -> PCI.clean_shutdown ~xs x
  | Vfb,_ -> Vfb.clean_shutdown ~xs x
  | Vkb,_ -> Vkb.clean_shutdown ~xs x
  | V4V,_ -> V4V.clean_shutdown ~xs x
  | Console,_ -> Console.clean_shutdown ~xs x

let can_surprise_remove ~xs (x: device) = Generic.can_surprise_remove ~xs x
