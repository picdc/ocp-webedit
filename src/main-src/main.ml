
open Myutils
open Dom_html

(** [main_main id_container] builds the main container indentified by
    [id_container] **)
let main_main id_container =
  let main_container = get_element_by_id id_container in

  hide_element Content.welcome_content;
  hide_element Content.load_login_content;
  Dom.appendChild main_container Content.load_login_content;
  Dom.appendChild main_container Content.welcome_content;

  Ace.require("Range");

  document##body##onclick <- handler (fun _ ->
    Dialog.Right_click_dialog.hide_all ();
    Js._true);

  let editor = get_element_by_id "editor"  in
  Global.init_editor editor;
  (Js.Unsafe.coerce window)##editor <- (Global.editor ());

  let launch _ =
    display_elements Content.editor_content;
    hide_element Content.load_login_content
  in
  Eventmanager.open_workspace#add_event launch


(** Program's start point **)
let main (id_container: Js.js_string Js.t) =
  Global.init_editor_content Content.editor_content;
  Indent.main ();
  main_main (Js.to_string id_container);
  hide_element Content.load_content;
  display_element Content.welcome_content;
  Login.main () (* Always in last because, it will hide "welcome_content"
                   when the Persona's "watch" is loginin *)


let _ =
  (Js.Unsafe.coerce window)##main <- Js.wrap_callback main
