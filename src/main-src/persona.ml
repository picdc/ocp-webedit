
(** Id **)
open Js.Unsafe

(** [watch ~loggedUser ~onready ~onlogin ~onlogout] initialize the Persona's
    watcher.
    When a user arrives on a web page with the watcher initiated, if he has
    already been logged it will automatically call [onlogin], and in case it
    fails it will call [onlogout].
    * [onready] is an optional function to be called when the user arrives on the
    web page.
    * [loggedUser] is an optional parameter to attribute a username to the user.
*)
let watch ?(loggedUser=Js.string "") ?(onready=(fun _ -> ()))
    ~onlogin ~onlogout () =
  let id = variable "navigator.id" in
  let w = obj [| "loggedUser", inject loggedUser;
                 "onlogin", inject onlogin;
                 "onlogout", inject onlogout;
                 "onready", inject onready |]
  in
  meth_call id "watch" [| inject w |]



(** [logout ()] calls the Persona's logout function (which calls
    watch.onlogout()) *)
let logout _ =
  let id = variable "navigator.id" in
  meth_call id "logout" [||]

(** [request ()] will trigger the login popup.
    The optional parameters are personnalization options. *)
let request ?(backgroundColor=Js.string "") ?(oncancel=(fun () -> ()))
    ?(privacyPolicy=Js.string "")
    ?(returnTo=Js.string "")
    ?(siteLogo=Js.string "")
    ?(siteName=Js.string "")
    ?(termsOfService=Js.string "") () =
  let _s = Js.string in
  let i_v v = inject (variable (Js.to_string v)) in
  let id = variable "navigator.id" in
  let r = obj [| "backgroundColor", i_v backgroundColor;
                 "oncancel", inject oncancel;
                 "privacyPolicy", i_v privacyPolicy;
                 "returnTo", i_v returnTo;
                 "siteLogo", i_v siteLogo;
                 "siteName", i_v siteName;
                 "termsOfService", i_v termsOfService |]
  in
  ignore (meth_call id "request" [| inject r |])
