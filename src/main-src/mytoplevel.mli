
(** The toplevel is in a WebWorker, so we need an interface here to
    manager messages and responses **)

(** The result type for the output message of the toplevel WebWorker **)
type toplevel_worker_res = {
  eval : string ; (** Eval result **)
  output : string (** Channel out (stdout & stderr) **)
}

(** [execute phrase callback] sends [phrase] to the toplevel WebWorker to
    evaluate and the result will be called in [callback] **)
val execute : string -> (toplevel_worker_res -> unit) -> unit

(** [evaluate_input ()] executes the phrase located in the toplevel's
    textarea **)
val evaluate_input : unit -> unit

(** [evaluate_input ()] executes the editor's selected area  **)
val evaluate_selection : unit -> unit 

(** [reset_toplevel ()] resets toplevel's environment **)
val reset_toplevel : unit -> unit

(** [make_output ()] returns the DOM element of the output for the toplevel **)
val make_output : unit -> Dom_html.element Js.t

(** [make_topleve ()] returns the DOM element for the input (and eval 
    responses) for the toplevel **)
val make_toplevel : unit -> Dom_html.element Js.t
