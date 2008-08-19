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

module D = Debug.Debugger(struct let name = "xenops" end)
open D

let one_page_kib = 4

let get_free_memory_kib ~xc =
	Xc.pages_to_kib (Int64.of_nativeint (Xc.physinfo xc).Xc.free_pages)

(** Returns the total amount of memory available in this host. *)
let get_total_memory_mib ~xc =
	Xc.pages_to_mib (Int64.of_nativeint ((Xc.physinfo xc).Xc.total_pages))

(** Gets the current page size (in bytes). *)
let get_page_size_bytes () =
	Int64.of_int (Mmap.getpagesize ())

(** Gets the current page size (in kib). *)
let get_page_size_kib () =
	Int64.div (get_page_size_bytes ()) 1024L

(** For the given values x and y, calculates the greatest *)
(** value x' <= x, such that x' is evenly divisible by y. *)
let round_down_to_multiple_of x y =
	Int64.mul (Int64.div x y) y

(** Rounds down the given value (in bytes) to the nearest page boundary. *)
let round_down_to_nearest_page_boundary_bytes value =
	round_down_to_multiple_of value (get_page_size_bytes ())
(** Rounds down the given value (in kib) to the nearest page boundary. *)
let round_down_to_nearest_page_boundary_kib value =
	round_down_to_multiple_of value (get_page_size_kib ())

(** Converts the given free memory value from bytes to kib. *)
let free_kib_of_bytes value = Int64.div value 1024L
(** Converts the given free memory value from bytes to kib. *)
let free_mib_of_kib value = Int64.div value 1024L

(** Converts the given used memory value from bytes to kib. *)
let used_kib_of_bytes value = Int64.div (Int64.add value 1023L) 1024L
(** Converts the given used memory value from bytes to kib. *)
let used_mib_of_kib value = Int64.div (Int64.add value 1023L) 1024L

(** Converts the given memory value from bytes to kib. *)
let bytes_of_kib value = Int64.mul value 1024L
(** Converts the given memory value from bytes to kib. *)
let bytes_of_mib value = Int64.mul value 1048576L
(** Converts the given memory value from mib to kib. *)
let kib_of_mib value = Int64.mul value 1024L

(** See the calculations in tools/python/xen/xend *)
module HVM = struct
	(** The amount of memory needed to be free in KiB.
	    See image.py:getRequiredAvailableMemory *)
	let required_initial_reservation kib =
		(* Apparently this was derived empirically:
		 * 2.4 KiB overhead per 1 MiB RAM
		 * + 4 MiB to avoid running out of memory altogether *)
		let extra_kib = 2.4 *. (Int64.to_float kib /. 1024.) +. 4096. in
		(* round up to the nearest page *)
		let one_page_kib = float_of_int one_page_kib in
		let extra_kib = Int64.of_float (ceil (extra_kib /. one_page_kib) *. one_page_kib) in
		Int64.add kib extra_kib

	(** See image.py:getRequiredAvailableMemory *)
	let required_available = required_initial_reservation

	(** Shadow memory needed by the domain.
	    See image.py:getRequiredShadowMemory. *)
	let required_shadow vcpus max_kib multiplier =
		(* add extra overheads *)
		let kib = required_initial_reservation max_kib in
		let mib = int_of_float (ceil (Int64.to_float kib /. 1024.)) in

		(* Apparently we need the following shadow allocation: *)
		let vcpu_pages = 256 * vcpus in
		let for_p2m_map = mib in
		let shadow_resident_processes = mib in
		let kib = one_page_kib * (vcpu_pages + for_p2m_map + shadow_resident_processes) in
		(* NB: the Xen default is lower than this but is for safety, not performance *)
		(* round up to next MiB *)
		let kib = (kib + 1023) / 1024 * 1024 in
		let result = Int64.of_float (float_of_int kib *. multiplier) in
		result

	let round_shadow_multiplier vcpus kib requested domid = 
	  let mib_from_kib kib = Int64.to_int (Int64.div kib 1024L) in
	  let requested_shadow_mib = mib_from_kib (required_shadow vcpus kib requested) in
	  let default_shadow_mib = mib_from_kib (required_shadow vcpus kib 1.) in
	  Xc.with_intf
	    (fun xc ->
	       let actual = Xc.shadow_allocation_get xc domid in
	       let actual_multiplier = float_of_int actual /. (float_of_int default_shadow_mib) in
	       debug "Actual shadow value is %d MiB [multiplier = %0.2f]; requested value was %d MiB [multiplier = %.2f]" actual actual_multiplier requested_shadow_mib requested;
	       (* Inevitably due to rounding the 'actual' multiplier will be different from the user-supplied
		  value. However if the requested MiB value was set then we record the user's float. If Xen
		  overrode us then we record the actual multiplier value *)
	       if actual <> requested_shadow_mib then actual_multiplier else requested)
end

module Linux = struct
	let required_initial_reservation kib = kib
	let required_available = required_initial_reservation
end

let required_to_boot hvm vcpus mem_kib mem_target_kib shadow_multiplier =
	if hvm then (
		Int64.add (HVM.required_available mem_kib)
		          (HVM.required_shadow vcpus mem_kib shadow_multiplier)
	) else (
		mem_target_kib
	)

let wait_xen_free_mem ~xc ?(max_attempts=10) ?(waiting_time_per_attempt=1) requested_kib =
	let attempt_left = ref max_attempts and success = ref false in
	while !attempt_left > 0 && not !success
	do
		let free_mem_kib = get_free_memory_kib ~xc in
		if free_mem_kib >= requested_kib then
			success := true
		else (
			decr attempt_left;
			Unix.sleep waiting_time_per_attempt
		)
	done;
	!success = true
