
(** [watch ~loggedUser ~onready ~onlogin ~onlogout] initialize the Persona's
    watcher.
    When a user arrives on a web page with the watcher initiated, if he has
    already been logged it will automatically call [onlogin], and in case it
    fails it will call [onlogout].
    * [onready] is an optional function to be called when the user arrives on the
    web page.
    * [loggedUser] is an optional parameter to attribute a username to the user.
*)
val watch : ?loggedUser:(Js.js_string Js.t) -> ?onready:(unit -> unit)
  -> onlogin:(Js.js_string Js.t -> unit) -> onlogout:(unit -> unit) -> unit -> unit

(** [logout ()] calls the Persona's logout function (which calls
    watch.onlogout()) *)
val logout : unit -> unit

(** [request ()] will trigger the login popup.
    The optional parameters are personnalization options. See Persona's API for
    more details. *)
val request : ?backgroundColor:Js.js_string Js.t
  -> ?oncancel:(unit -> unit)
  -> ?privacyPolicy:Js.js_string Js.t
  -> ?returnTo:Js.js_string Js.t
  -> ?siteLogo:Js.js_string Js.t
  -> ?siteName:Js.js_string Js.t
  -> ?termsOfService:Js.js_string Js.t
  -> unit
  -> unit
