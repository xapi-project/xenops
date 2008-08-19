
type netty = Bridge of string | DriverDomain | Nat

val online : string -> netty -> unit
val offline : string -> netty -> unit
