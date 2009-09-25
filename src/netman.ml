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
module D = Debug.Debugger(struct let name = "netman" end)
open D


type netty = Bridge of string | DriverDomain | Nat

let log_exn name f =
	try f ()
	with exn ->
		warn "exception during %s: %s" name (Printexc.to_string exn)

let online vif netty =
	match netty with
	| Bridge bridgename ->
		let setup_bridge_port dev =
			log_exn "link.down" (fun () -> Netdev.Link.down dev);
			log_exn "link.arp" (fun () -> Netdev.Link.arp dev false);
			log_exn "link.multicast" (fun () -> Netdev.Link.multicast dev false);
			log_exn "link.set_addr" (fun () -> Netdev.Link.set_addr dev "fe:ff:ff:ff:ff:ff");
			log_exn "addr.flush" (fun () -> Netdev.Addr.flush dev);
			in
		let add_to_bridge br dev =
			log_exn "bridge.set_forward_delay" (fun () -> Netdev.Bridge.set_forward_delay br 0);
			log_exn "bridge.intf_add" (fun () -> Netdev.Bridge.intf_add br dev);
			log_exn "link.up" (fun () -> Netdev.Link.up dev);
			in
		debug "Adding %s to bridge %s" vif bridgename;
		begin try
			setup_bridge_port vif;
			add_to_bridge bridgename vif
		with exn ->
			warn "exception in netman.online ignoring: %s" (Printexc.to_string exn)
		end;
	| DriverDomain -> ()
	| _ ->
		failwith "not supported yet"

let offline vif netty =
	match netty with
	| Bridge bridgename ->
		debug "Removing %s from bridge %s" vif bridgename;
		begin try
			log_exn "bridge.intf_del" (fun () -> Netdev.Bridge.intf_del bridgename vif);
			log_exn "link.down" (fun () -> Netdev.Link.down vif);
		with _ ->
			warn "interface %s already removed from bridge %s" vif bridgename;
		end;
	| DriverDomain -> ()
	| _                 ->
		failwith "not supported yet"
