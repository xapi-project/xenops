(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
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

let report_function : (string -> float -> unit) option ref = ref None

let time_this str f =
	let call_report t =
		match !report_function with
		| None -> ()
		| Some r -> try r str t with _ -> ()
		in
	let t1 = Unix.gettimeofday () in
	let r = try
			f ()
		with exn ->
			let t2 = Unix.gettimeofday () in
			call_report (t2 -. t1);
			raise exn
		in
	let t2 = Unix.gettimeofday () in
	call_report (t2 -. t1);
	r
