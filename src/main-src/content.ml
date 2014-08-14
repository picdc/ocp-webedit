
open Js
open Dom_html
open Myutils
open Global


(** [editor_t] creates the editor eventlistener **)
let editor_t =
  let goto_next_error (f, l, c) =
    let e = Global.editor () in
    let line = l - 1 in
    match c with
    | Some (sc, ec) ->
      e##moveCursorTo(line, sc);
      e##getSelection()##selectTo(line, ec)
    | _ ->
      e##moveCursorTo(line, 0);
      e##getSelection()##selectLine()
  in
  { empty_eventlistener with
    goto_next_error }

(** [make_compilation_widget ()] creates the container for the compiler output
    and the bytecode download button *)
let make_compilation_widget () =
  let div = get_element_by_id "compilation" in  div


type bottap = {
  li : Dom_html.element Js.t;
  a : Dom_html.element Js.t;
  tab : Dom_html.element Js.t;
}

let get_bottab name tab =
  let li = get_element_by_id (Printf.sprintf "bottabs_%s_title_li" name) in
  let a = get_element_by_id (Printf.sprintf "bottabs_%s_title_a" name) in
  { li; a; tab }

(** [make_bottom_widget ()] creates the widget that contains the toplevel, the
    standard output and the compilation output *)
let make_bottom_widget div =
  (* Création du widget pour le toplevel *)
  (* let toplevel = Mytoplevel.make_toplevel () in *)
  (* let output = Mytoplevel.make_output () in *)
  let compilation = make_compilation_widget () in

  (* let title_toplevel = get_bottab "toplevel" toplevel in
  let title_output = get_bottab "output" output in *)
  let title_compilation = get_bottab "compilation" compilation in

  let curr_tab = ref title_compilation in

(*
  title_toplevel##innerHTML <- string "Toplevel";
  title_toplevel##id <- string ;
  title_output##innerHTML <- string "Output";
  title_output##id <- string ;
  title_compilation##innerHTML <- string "Compilation";
  title_compilation##id <- string ;
  title_toplevel##className <- string "bottabs_tab bottabs_tab_active";
  title_output##className <- string "bottabs_tab bottabs_tab_noactive";
  title_compilation##className <- string "bottabs_tab bottabs_tab_noactive";
*)
 (*  toplevel##className <- string "bottabs_content";
  display_element toplevel;
  hide_element output;
  output##className <- string "bottabs_content"; *)
  compilation##className <- string "bottabs_content";
  display_element compilation;

(*   let switch_to_tab new_tab =
      let old_tab = !curr_tab in
      old_tab.li##className <- string "";
      hide_element old_tab.tab;
      curr_tab := new_tab;
      display_element new_tab.tab;
      new_tab.li##className <- string "active";
  in
  title_toplevel.a##onclick <- handler (fun _ ->
      switch_to_tab title_toplevel;
    Js._true);
  title_output.a##onclick <- handler (fun _ ->
      switch_to_tab title_output;
      Js._true);
  let switch_to_compilation_tab () =
    switch_to_tab title_compilation
  in
  title_compilation.a##onclick <- handler (fun _ ->
    switch_to_compilation_tab ();
    Js._true); *)
  
  (* Ajout de l'event pour la compilation *)
  Eventmanager.compile#add_event (fun result ->
    (* switch_to_compilation_tab (); *)
    let container = query_selector compilation "#compilation_output" in
    let button = coerceTo_button
      (query_selector compilation "#bytecode") in
    let stdout, errors =
      Errors_lexer.parse_compile_output result.cr_stdout in
    container##innerHTML <- string (stdout);

    let project = Filemanager.get_project_from_path result.cr_path in
    Errors_report.add_errors_reports project errors;

    if result.cr_code = 0 then begin
      let blob = string_to_blob result.cr_bytecode in
      button##onclick <- handler (fun _ ->
        ignore (Js.Unsafe.fun_call (Js.Unsafe.variable "saveAs")
                  [| Js.Unsafe.inject blob;
                     Js.Unsafe.inject (string result.cr_exec)|]);
        Js._true);
      button##disabled <- Js._false end
    else button##disabled <- Js._true);
  ()

(** [try_widget id evl] tries to make the widget with the eventlistener
    [evl] in the DOM element identified by its [id], returns it if exists **)
let try_widget id evl  =
  try
    let c = get_element_by_id id in
    hide_element c;
    Widget.make c evl;
    Some c
  with
  | Myutils.Element_missing _ ->  None
  | e -> raise e


(** <div> containing the editor and its widgets. Kept in order to prevent
   recomputation and recreating it in the DOM *)
let editor_content =
  let widgets =
    [ "menu_bar", Menu.t ;
      "sidepanel", Sidepanel.t ;
      "tabs", Tabs.t ;
      "editor", editor_t ;
      "bottabs", { empty_eventlistener with init = make_bottom_widget };
      "toolbar", empty_eventlistener;
    ] in

  Toolbar.(
    let _ = () in
    id_onclick "new_module" callback_new_module;
    id_onclick "new_interface" callback_new_interface;
    id_onclick "switch_ml_mli" callback_switch_ml_mli;
    id_onclick "save_button" callback_save;
    id_onclick "compile_button" callback_compile;
    id_onclick "link_before" callback_link_before;
    id_onclick "link_after" callback_link_after;
  );

  List.fold_left (fun l (id, evl) ->
    match try_widget id evl with
    | None -> l
    | Some el -> el::l
  ) [] widgets


(** <div> containing the welcome message when the user isn't logged *)
let welcome_content =
  let main = get_element_by_id "welcome_content" in
  Toolbar.id_onclick "sign_button" (fun _ ->
      Eventmanager.login#trigger ();
      Js._true);
  (*
  Toolbar.id_onclick "signup_button" (fun _ ->
    Eventmanager.signup#trigger ();
    Js._true); *)
  main

(** <div> containing the loading message. Its content is actually in the file
    load.js in www/. *)
let load_content =
  (* Don't use "get_element_by_id" *)
  (Js.Unsafe.coerce Dom_html.window)##loadcontent


(** <div> containing the login message *)
let load_login_content =
  let div = createDiv document in
  div##innerHTML <- string "Logging in...";
  div##id <- string "logging-msg";
  div
