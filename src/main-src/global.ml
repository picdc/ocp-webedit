
type config = { mutable editor : Ace.editor Js.t option ;
                mutable editor_content : Dom_html.element Js.t list }

let global_conf = { editor = None ;
                    editor_content = [] }

let editor () = match global_conf.editor with
    None -> failwith "Editor not init"
  | Some e -> e

let init_editor el =
  global_conf.editor <- Some (Ace.edit el);
  (editor())##setTheme(Js.string "ace/theme/eclipse")

let init_editor_content el = global_conf.editor_content <- el




type compile_conf = {
  cc_files : string list ;
  cc_output : string }

type s_dirtree =
  S_Directory of string * s_dirtree list
| S_Project of string * string list * string


type file = {
  f_id : int;
  f_project : int;
  mutable f_name : string;
  mutable f_is_open : bool;
  mutable f_is_unsaved : bool }

type project = {
  p_id : int;
  p_parent : int ;
  mutable p_name : string ;
  mutable p_files : file list ;
  mutable p_compile_opts : compile_conf }

type directory = {
  dir_id : int ;
  dir_is_root : bool ;
  dir_parent : int ;
  mutable dir_name : string ;
  mutable dir_dirs : dirtree list }

and dirtree =
| Directory of directory
| Project of project
| File of file

type conftype =
| Compile of project
| Edit
type conf = (string * string) list
type edit_conf = {
  ec_theme: string }
  
type compile_result = {
  cr_path: string;
  cr_stdout : string ;
  cr_exec : string ;
  cr_bytecode : string;
  cr_code: int }


type eventlistener = {
 init : Dom_html.element Js.t -> unit ;
 open_workspace : directory -> unit ;
 open_file : file * string -> unit ;
 close_workspace : unit -> unit ;
 close_file : file -> unit ;
 create_file : file -> unit ;
 create_project : project -> unit ;
 create_directory : directory -> unit ;
 rename_file : file -> unit ;
 rename_project : project -> unit ;
 rename_directory : directory -> unit ;
 delete_file : file -> unit ;
 delete_project : project -> unit ;
 delete_directory : directory -> unit ;
 save_file : file -> unit ;
 save_conf : conftype * conf -> unit ;
 unsaved_file : file -> unit ;
 import_project : project -> unit;
 import_file : file -> unit ;
 import_library : file -> unit;
 switch_file : file option * file -> unit ;
 link_after : conftype * conf * file -> unit ;
 link_before : conftype * conf * file -> unit ;
 compile : compile_result -> unit;
 goto_next_error : file * int * (int * int) option -> unit }

let efun _ = ()
let empty_eventlistener = {
  init = efun;
  open_workspace = efun;
  open_file = efun;
  close_workspace = efun;
  close_file = efun;
  create_file = efun;
  create_project = efun;
  create_directory = efun;
  rename_file = efun;
  rename_project = efun;
  rename_directory = efun;
  delete_file = efun;
  delete_project = efun;
  delete_directory = efun;
  save_file = efun;
  save_conf = efun;
  unsaved_file = efun;
  import_project = efun;
  import_file = efun;
  import_library = efun; 
  switch_file = efun;
  link_after = efun;
  link_before = efun;
  compile = efun;
  goto_next_error = efun}
