
open Js
open Dom
open Dom_html
open Global
open Filemanager
open Myutils

let id_onclick name callback =
  let b = get_element_by_id name in
  b##onclick <- handler (fun _ -> callback (); _true);
  ()

(** Functions below are the ones of the eventlistener for the toolbar
    widget **)

let callback_new_module () =
  let project = get_current_project () in
  Dialog.prompt_new_module ~project ()

let callback_new_interface () =
  let project = get_current_project () in
  Dialog.prompt_new_interface ~project ()

let callback_switch_ml_mli () =
  match get_current_file () with
  | None -> ()
  | Some f ->
    let p = get_project f.f_project in
    let nnewf =
      let n = Filename.chop_extension f.f_name in
      match get_type_of_file f with
      | Module -> n ^ ".mli"
      | Interface -> n ^ ".ml" 
      | Library -> n ^ ".cma" in
    try
      let newf = List.find (fun f -> f.f_name = nnewf) p.p_files in
      let after _ = Eventmanager.switch_file#trigger newf in
      if newf.f_is_open then after ()
      else Eventmanager.open_file#trigger ~after newf
    with Not_found ->
      let title = "Switch Module/Interface" in
      let text = "File \"" ^ nnewf ^
        " doesn't exist in the project\nDo you want to create it ?" in
      let callback () =
        let after f = Eventmanager.switch_file#trigger f in
        Eventmanager.create_file#trigger ~after (p, nnewf) in
      Dialog.confirm ~title ~text ~callback

let callback_save () =
  match get_current_file () with
  | None -> ()
  | Some file -> Eventmanager.save_file#trigger file

let callback_compile () =
  let project_opt = get_current_project () in
  match project_opt with
  | None -> Dialog.prompt_compile ~project:project_opt ()
  | Some project -> Eventmanager.compile#trigger project

let callback_link_before () =
  match get_current_file () with
  | None -> ()
  | Some file -> Eventmanager.link_before#trigger file

let callback_link_after () =
  match get_current_file () with
  | None -> ()
  | Some file -> Eventmanager.link_after#trigger file
