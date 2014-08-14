(** WebWorker has a weird API, be careful *)


(** Event sent and received by the worker and its caller *)
class type event = object

(** Message value from post_message *)
  method data : Js.js_string Js.t Js.readonly_prop
end

(** a webWorker is a Javascript Thread *)
class type webWorker = object

    (** [w##onmessage(f)] calls [f] when the worker sends a message *)
  method onmessage : (event Js.t -> unit) Js.prop

    (** [w##postMessage(s)] sends the message [s] to the worker *)
  method postMessage : Js.js_string Js.t -> unit Js.meth
  method terminate : unit Js.meth
end

(** [jsnew webWorker("hello.js")] instantiates a new webWorker that uses the
    ["hello.js"] script as his environment *)
val webWorker : (Js.js_string Js.t -> webWorker Js.t) Js.constr

(** Those two functions must be called from the worker's script *)

(** [onmessage f] calls [f] when the worker receives a message *)
val onmessage : (event Js.t -> unit) -> unit

(** [postMessage s] sends a message to the worker's caller *)
val postMessage : string -> unit
