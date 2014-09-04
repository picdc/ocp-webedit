
open Global

class ['a, 'b] event act = object
  val action = (act: ('a -> unit) -> 'b -> unit)
  val mutable events = []
  method add_event e = events <- e::events
  method trigger ?(after=fun _ -> ()) args =
    let f s = List.iter (fun f -> f s) (events@[after]) in
    action f args
end

class ['a, 'b] action act = object
  val mutable action = (act: ('a -> 'b))
  method trigger ?(after=(fun _ _ -> ())) args =
    let res = action args in
    after args res;
    res
end

class ['a, 'b] mutable_action act = object
  inherit ['a, 'b] action act
  method set act = action <- act
end

let open_workspace = new event Filemanager.open_workspace
let close_workspace = new event Filemanager.close_workspace
let create_file = new event Filemanager.create_file
let create_project = new event Filemanager.create_project
let create_directory = new event Filemanager.create_directory
let rename_file = new event Filemanager.rename_file
let rename_project = new event Filemanager.rename_project
let rename_directory = new event Filemanager.rename_directory
let switch_file = new event Filemanager.switch_file
let open_file = new event Filemanager.open_file
let close_file = new event Filemanager.close_file
let save_file = new event Filemanager.save_file
let import_project = new event Filemanager.import_project
let import_file = new event Filemanager.import_file
let import_library = new event Filemanager.import_library
let unsave_file = new event Filemanager.unsave_file
let delete_project = new event Filemanager.delete_project
let delete_file = new event Filemanager.delete_file
let delete_directory = new event Filemanager.delete_directory
let save_conf = new event Filemanager.save_conf
let compile = new event Filemanager.compile

let link_after_action callback file =
  let callback ((conftype, conf, file) as args) =
    callback args;
    save_conf#trigger (conftype, conf) in
  Filemanager.link_after callback file

let link_before_action callback file =
  let callback ((conftype, conf, file) as args) =
    callback args;
    save_conf#trigger (conftype, conf) in
  Filemanager.link_before callback file

let link_after = new event link_after_action
let link_before = new event link_before_action


let goto_error_action callback (project, error) =
  let open Errors_format in
  let file = Filemanager.get_file_from_project project error.file in
  let after _ =
    let after _ =
      callback (file, error.line, error.chars) in
    match Filemanager.get_current_file () with
    | Some f when f.f_id = file.f_id -> after ()
    | _ -> switch_file#trigger ~after file in
  if file.f_is_open then after ()
  else open_file#trigger ~after file

let goto_next_error = new event goto_error_action

let login = new mutable_action (fun () -> ())
(*let signup = new mutable_action (fun () -> ())*)
let logout = new mutable_action (fun () -> ())

let save_all callback =
  let nb_files_unsaved = ref (Filemanager.count_unsaved_files ()) in
  if !nb_files_unsaved = 0 then callback ()
  else
    let todo _ =
      decr nb_files_unsaved;
      if !nb_files_unsaved = 0 then callback ()
    in
    List.iter (fun f ->
      if f.f_is_unsaved then
        save_file#trigger ~after:todo f
    ) (Filemanager.get_files ())


let export_directory_action dt =
  let p = Myutils.coerceTo_input
    (Myutils.get_element_by_id "project-path-form") in
  p##value <- Js.string (Filemanager.get_path dt);
  let n = Myutils.coerceTo_input
    (Myutils.get_element_by_id "project-name-form") in
  n##value <- Js.string (Filename.basename (Filemanager.get_path dt));
  let s = Myutils.coerceTo_input
    (Myutils.get_element_by_id "export-submit-form") in
  s##click()

let export_directory = new action export_directory_action

let export_file_action file =
  let open Errors_format in
  let after _ =
    let c = Filemanager.get_content file in
    (* Workaround for the first "0" in file given by Ace content *)
    match c with
    | None -> ()
    | Some c ->
      (* Workaround for the first "0" in file given by Ace content *)
      let c = Myutils.string_to_blob c in
      Myutils.save_as file.f_name c
  in
  if file.f_is_open then after ()
  else open_file#trigger ~after file

let export_file = new action export_file_action
