
type t

val dollar : string -> t
val on : t -> string -> (unit -> unit) -> unit
val modal : t -> string -> unit
