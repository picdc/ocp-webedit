
open Myutils
open Js
open Dom
open Dom_html
open Global

type menuItem = Dom_html.element Js.t
type menuButton = Dom_html.element Js.t

(** [int_to_px i] converts the integer [i] to a string representing [i]
    following by the string "px" **)
let int_to_px i = string (Format.sprintf "%dpx" i)

(** [active_menu] is the current menu active **)
let active_menu = ref None

(** [show_menu button menu] shows the [menu] matched with [button] **)
let show_menu button menu =
  menu##style##top <- int_to_px (button##clientTop + button##offsetTop +
                                   button##clientHeight);
  menu##style##left <- int_to_px (button##clientLeft + button##offsetLeft);
  menu##style##display <- string "";
  button##className <- string "menu_button menu_button_active";
  active_menu := Some (button, menu)

(** [hide_menu button menu] hides the [menu] matched with [button] **)
let hide_menu button menu =
  menu##style##display <- string "none";
  button##className <- string "menu_button"



let create_menu_item str ?(activate=true) ?(shortcut="") callback =
  let item = get_element_by_id ("menu-item-" ^ str) in
  if activate then
    item##onclick <- handler (fun _ ->
      match !active_menu with
      | None -> Js._true
      | Some (ab,am) -> hide_menu ab am; active_menu := None; callback ();
        Js._true)
  else begin
    item##style##color <- string "#AAAAAA";
    item##style##textDecoration <- string "line-through" end;
  item


let create_menu_button name menuItems =
  let button = get_element_by_id ("menu-button-" ^ name) in
  let menu = get_element_by_id ("menu-popup-" ^ name) in
  button##onclick <- handler (fun _ ->
    begin match !active_menu with
      | None -> show_menu button menu
      | Some (ab, am) when am != menu -> hide_menu ab am; show_menu button menu
      | _ -> hide_menu button menu; active_menu := None
    end;
    Js._false);
  button


let create_menu_bar container menuButtons =
  container##className <- string ((to_string container##className)^" menu_bar");
  List.iter (fun button -> Dom.appendChild container button) menuButtons






(** Functions below are handlers for [menuItem]s **)


let not_impl () = Dom_html.window##alert(Js.string "Not implemented")

let callback_project_create () = Dialog.prompt_create_project ()

let callback_project_import () = Dialog.prompt_import_project ()

(*
let callback_project_download () =
  let todo project =
    Eventmanager.export_directory#trigger (Project project) in
  match Filemanager.get_current_file () with
    | None -> Dialog.prompt_select_project todo
    | Some file ->
        let project = Filemanager.get_project_from_file file in
        todo project
*)

let callback_project_save () = Eventmanager.save_all (fun () -> ())

let callback_project_save_signout () =
  Eventmanager.save_all (fun () -> Eventmanager.logout#trigger ())

let callback_project_signout () = Eventmanager.logout#trigger ()

let callback_file_create () =
  let project = Filemanager.get_current_project () in
  Dialog.prompt_new_module ~project ()

let callback_lexer_create () =
  let project = Filemanager.get_current_project () in
  Dialog.prompt_new_lexer ~project ()

let callback_grammar_create () =
  let project = Filemanager.get_current_project () in
  Dialog.prompt_new_grammar ~project ()

let callback_file_import () =
  let project = Filemanager.get_current_project () in
  Dialog.prompt_import_file ~project ()

let callback_library_import () =
  let project = Filemanager.get_current_project () in
    Dialog.prompt_import_library ~project ()

let callback_edit_copy () = not_impl ()
let callback_edit_cut () = not_impl ()
let callback_edit_paste () = not_impl ()
let callback_edit_settings () =
  let theme = (Filemanager.get_edit_settings ()).ec_theme in
  Dialog.prompt_edit_settings ~theme ()

let prev_compiled_project = ref None
let callback_compile_compile () =
  let project = Filemanager.get_current_project () in
  Dialog.prompt_compile ~project ()

let callback_compile_recompile () =
  match !prev_compiled_project with
  | None -> callback_compile_compile ()
  | Some p -> Eventmanager.compile#trigger p

let callback_compile_settings () =
  let project = Filemanager.get_current_project () in
  Dialog.prompt_compile_settings ~project ()

let callback_ocaml_tryocaml () =
  (* Little trick to open window in a new tab *)
  let b = createInput ~_type:(Js.string "button") document in
  b##onclick <- handler (fun _ ->
    ignore (window##open_(Js.string "http://try.ocamlpro.com",
                  Js.string "Try OCaml", Js.null));
    Js._false); (* false = open in new tab *)
  b##click ()

let callback_help_about () =
  let title = "About us" in
  let text =
    String.concat "\n" [
      "This web site has been developped by OCamlPro & INRIA.";
      "Authors:";
      "Pierrick COUDERC";
      "David MAISON";
      "Fabrice LE FESSANT";
    ] in
  Dialog.alert ~title ~text ()

let callback_help_online () = not_impl ()






(** The list of [menuItem] **)

let project_items =
  [
    (* "create-new-project", true, "", callback_project_create ; *)
(*    "import-new-project", true, "", callback_project_import ; *)
(*    "download-current-project", true, "", callback_project_download ; *)
    "save-project", true, "", callback_project_save ;
    "save-and-signout", true, "", callback_project_save_signout ;
    "signout-without-saving", true, "", callback_project_signout
  ]

let file_items =
  [
    (* "create-module", true, "", callback_file_create ; *)
    "new-lexer", true, "", callback_lexer_create;
    "new-grammar", true, "", callback_grammar_create;
    "import-file", true, "", callback_file_import;
    "import-library", true, "", callback_library_import;
  ]

let edit_items =
  [     "edition-settings", true, "", callback_edit_settings; ]

let compile_items =
  [ "compile-project", true, "F6", callback_compile_compile ;
    "recompile", true, "F7", callback_compile_recompile ;
    "compilation-settings", true, "", callback_compile_settings ]

let ocaml_items =
  [ (* "start-try-ocaml", true, "", callback_ocaml_tryocaml *) ]

let help_items =
  [ (* "about", true, "", callback_help_about ; *)
    (* "online-help", false, "", callback_help_online *)
]








(** Functions below are the ones of the eventlistener for the menu
    widget **)

let init div =
    let set_menu_items items =
      List.iter (fun (name, activate, shortcut, callback) ->
        let ele = "menu-item-" ^ name in
        try
          let item = get_element_by_id ele in
          item##onclick <- handler (fun _ -> callback (); Js._true);
        with e ->
          alert (Printf.sprintf "Element %S: %s" ele
          (Printexc.to_string e))
      ) items
    in
    set_menu_items project_items;
    set_menu_items file_items;
    set_menu_items edit_items;
    set_menu_items compile_items;
    set_menu_items help_items;

    (Js.Unsafe.coerce window)##shortcutCompile <-
      Js.wrap_callback callback_compile_compile;
    (Js.Unsafe.coerce window)##shortcutRecompile <-
      Js.wrap_callback callback_compile_recompile;

    create_menu_bar div
      []

let callback_compile comp_res =
  let p = Filemanager.get_project_from_path comp_res.cr_path in
  prev_compiled_project := Some p


let t = { empty_eventlistener with
  init;
  compile = callback_compile }

