
type netty = Bridge of string | DriverDomain of string | Nat

val online : xs:Xs.xsh -> string -> string -> netty -> unit
val offline : string -> netty -> unit
