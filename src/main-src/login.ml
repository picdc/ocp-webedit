

(** Module for the login/logout service **)


exception Bad_request_url of string

open Dom_html
open Myutils


(** [loginin_dom ()] changes the load login panel into the editor panel **)
let loginin_dom () =
  hide_element Content.welcome_content;
  display_elements Content.editor_content


(** [onlogout ()] called when user want to logout and return to the welcome
    panel **)
(*let onlogout () =
  hide_elements Content.editor_content;
  display_element Content.welcome_content;
  Eventmanager.close_workspace#trigger ()*)


(** [onclick_signin] is the action when user click for login in  **)
(*let onclick_signin () =
  hide_element Content.welcome_content;
  display_element Content.load_login_content;
  if Config.offline_mode then begin
    let success _ =
      Eventmanager.open_workspace#trigger ();
      loginin_dom () in
    let msg = "assertion=offline_mode" in
    Request.pull_request ~success ~url:"login" ~msg ()
  end
  else Persona.request () (* Call verify_assertion *)*)

(** [onclick_signout] is the action when user click for logout **)
(*let onclick_signout () =
  if not Config.offline_mode then Persona.logout ()
  else onlogout ()*)

(** [verify_assertion callback assertion] verify the [assertion] key given by
    the user with Persona and login in the user if it's successfull and call
    [callback], otherwise cancel the procedure by logout the user.  Display the
    "login" panel when the login verification is loading **)
(*let verify_assertion callback assertion =
  let assertion = Js.to_string assertion in
  let callback json =
    Eventmanager.open_workspace#trigger ();
    loginin_dom ();
    callback json in
  let callback_failure json = Persona.logout () in
  let msg = Format.sprintf "assertion=%s" assertion in
  Request.pull_request ~success:callback ~failure:callback_failure
    ~url:"login" ~msg ()*)

(** [main ()] inits the login functions and Persona's watcher *)
(*let main () =
  if not Config.offline_mode then begin
    (Js.Unsafe.coerce Dom_html.window)##verifyAssertion <-
      Js.wrap_callback (verify_assertion (fun _ -> ()));
    (Js.Unsafe.coerce Dom_html.window)##onlogoutFunction <-
      Js.wrap_callback onlogout;

    let onlogin assertion =
      hide_element Content.welcome_content;
      display_element Content.load_login_content;
      verify_assertion (fun _ -> ()) assertion in
    Persona.watch ~onlogin ~onlogout () end
  else display_element Content.welcome_content;
  Eventmanager.login#set onclick_signin;
  Eventmanager.logout#set onclick_signout*)

let onclick_sign_in () =
  let success _ =
    Eventmanager.open_workspace#trigger ();
    loginin_dom () in
  Dialog.prompt_login ~success ()

(*
let onclick_sign_up () =
  let success str =
    Eventmanager.open_workspace#trigger ();
    loginin_dom () in
  Dialog.prompt_signup ~success () *)

let onclick_sign_out () =
  hide_elements Content.editor_content;
  display_element Content.welcome_content;
  Eventmanager.close_workspace#trigger ()

let main () =
  display_element Content.welcome_content;
  Eventmanager.login#set onclick_sign_in;
  (*Eventmanager.signup#set onclick_sign_up;*)
  Eventmanager.logout#set onclick_sign_out;
