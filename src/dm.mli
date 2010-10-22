exception Ioemu_failed of string
exception Ioemu_failed_dying

type disp_opt =
	| NONE
	| VNC of bool * string * int * string (* auto-allocate, bind address, port it !autoallocate, keymap *)
	| SDL of string (* X11 display *)

type info = {
	hvm: bool;
	memory: int64;
	boot: string;
	serial: string;
	vcpus: int;
	usb: string list;
	nics: (string * string * string option * bool) list;
	acpi: bool;
	disp: disp_opt;
	pci_emulations: string list;
	sound: string option;
	power_mgmt: int;
	oem_features: int;
	inject_sci: int;
	videoram: int;
	extras: (string * string option) list;
	disks: Device.Vbd.info list;
	vifs: Device.Vif.info list;
	stubdom: ([`domain] Uuid.t) option;
}

val write_logfile_to_log : int -> unit
val unlink_logfile : int -> unit

val vnc_port_path : Xc.domid -> string

val signal : xs:Xs.xsh -> domid:Xc.domid -> ?wait_for:string -> ?param:string
	  -> string -> unit

val start : xc:Xc.handle -> xs:Xs.xsh -> dmpath:string -> ?timeout:float -> info -> Xc.domid -> (int option) * (int option)
val restore : xc:Xc.handle -> xs:Xs.xsh -> dmpath:string -> ?timeout:float -> info -> Xc.domid -> (int option) * (int option)

val suspend : xs:Xs.xsh -> Xc.domid -> unit
val resume : xs:Xs.xsh -> Xc.domid -> unit
val stop : xs:Xs.xsh -> Xc.domid -> int -> unit
