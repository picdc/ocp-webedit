val db_initial: unit -> unit

val user_create: string -> string -> ?name:string -> unit -> unit

val user_delete: string -> unit

val user_change_psw: string -> string -> string -> unit

val user_identify: string -> string -> unit

val user_list: unit -> unit

val opam_initial: unit -> unit

val pkg_operation: string -> ?package:string -> unit -> unit

val get_pkg_op: unit -> string list