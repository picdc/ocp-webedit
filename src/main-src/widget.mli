
(** [make c ev] make [ev] a widget in [c] by linking all its events with the
    [Eventmanager] after calling its [init] function. **)
val make : Dom_html.element Js.t -> Global.eventlistener -> unit
