
(** Main config for the workspace
    Include the editor and the Content.editor_content (ie the editor panel)
    to avoid circular dependancies **)
type config = { mutable editor : Ace.editor Js.t option ;
                mutable editor_content : Dom_html.element Js.t list }

(** [global_conf] global main config for the project **)
val global_conf : config

(** [editor] return the current editor of the workspace **)
val editor : unit -> Ace.editor Js.t

(** [init_editor container] initialize the Ace editor in the [container] **)
val init_editor : Dom_html.element Js.t -> unit

(** [init_man_content container] initalize the main content to [container] **)
val init_editor_content : Dom_html.element Js.t list -> unit



(** Compilation settings type **)
type compile_conf = {
  cc_files : string list ; (* the order determines the compiling order *)
  cc_output : string
}


(** Types for describe user's directory's arborescence
   (Server side) **)
type s_dirtree =
  S_Directory of string * s_dirtree list (* dirname, directories *)
| S_Project of string * string list * string
(* project's name, files's name, compile settings *)


(** Types for describe user's directory's arborescence
   (Client side) **)
type file = {
  f_id : int; (* unique id between all files *)
  f_project : int;
  mutable f_name : string;
  mutable f_is_open : bool;
  mutable f_is_unsaved : bool }

type project = {
  p_id : int; (* unique id between all projects *)
  p_parent : int ; (* id of the parent directory *)
  mutable p_name : string ;
  mutable p_files : file list ; (* the order determines the compiling order *)
  mutable p_compile_opts : compile_conf }

type directory = {
  dir_id : int ; (* unique id between all directories *)
  dir_is_root : bool ; (* a root can't be deleted or renamed *)
  dir_parent : int ; (* -1 if dir_is_root *)
  mutable dir_name : string ;
  mutable dir_dirs : dirtree list } (* the order isn't important *)

and dirtree =
| Directory of directory
| Project of project
| File of file

(** Some Configuration/Settings types **)
type conftype =
| Compile of project
| Edit
type conf = (string * string) list
type edit_conf = {
  ec_theme : string }

(** Record that will be sent by the compiler once compilation has been done
    (successfully or not) *)
type compile_result = {
  cr_path : string; (** Compiled project's path *)
  cr_stdout : string ; (** Standard output from the compiler *)
  cr_exec : string ; (** Executable name *)
  cr_bytecode : string; (** Bytecode produced *)
  cr_code: int (** Code returned by the compiler :
                   - 0 : Success
                   - 2 : Fail *)
}


(** Type that widgets must respect to be linked with the eventmanager **)
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
 import_library: file -> unit;
 switch_file : file option * file -> unit ;
 link_after : conftype * conf * file -> unit ;
 link_before : conftype * conf * file -> unit ;
 compile : compile_result -> unit;
 goto_next_error : file * int * (int * int) option -> unit }

(** [empty_eventlistener] is an eventlistener for widget that does nothing **)
val empty_eventlistener : eventlistener
