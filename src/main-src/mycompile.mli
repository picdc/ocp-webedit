
(** Record that contains all the necessary to compile a project *)
type compile_options = {
  co_path : string; (** Project's path that will be compiled *)
  co_src : (string * string) list ; (** Sources to compile in the correct
                                     compilation order *)
  co_output : string (** Executable name *)
}

(** [compile f opts] execute compilation with [opts] and computes result with
    [f] *)
val compile : (Global.compile_result -> unit) ->  compile_options -> unit

(** [main ()] simply wrap [compile] to be used directly in js *)
val main : unit -> unit
