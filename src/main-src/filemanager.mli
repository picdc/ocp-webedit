

(**
   The filemanager is where all files and arborescence of the user workspace
   is centralized.
   If you want to modify the user workspace on the server, it's really advised
   to use functions below in order to modify the client side and server side
   workspace at the same point
 **)


open Global

type typefile = Module | Interface | Library | Lexer | Grammar

exception Bad_directory_name of directory * string (* parent dir, name *)
exception Bad_file_name of project * string (* file's project, filename *)
exception File_not_found of int (* file's id *)
exception Project_not_found of int (* project's id *)
exception Directory_not_found of int (* dir's id *)
exception Workspace_already_opened
exception Workspace_closed
exception Bad_path of string (* path *)
exception Operation_denied of string (* operation's name *)

(** [get_file id] returns the file corresponding to [id].
    Raise [File_not_found] if there is not match for [id] **)
val get_file : int -> file

(** [get_project id] returns the project corresponding to [id]
    Raise [Project_not_found] if there is not match for [id] **)
val get_project : int -> project

(** [get_directory id] returns the directory corresponding to [id]
    Raise [Directory_not_found] if there is not match for [id]  **)
val get_directory : int -> directory

(** [get_workspace ()] returns the workspace's root directory **)
val get_workspace : unit -> directory

(** [get_lib_list ()] returns the list of available libraries on the server side**)
val get_lib_list: unit -> string list

(** [get_current_file ()] returns the current opened file if exists **)
val get_current_file : unit -> file option

(** [get_current_project ()] returns the project of the current opened file
    if it exists **)
val get_current_project : unit -> project option

(** [get_current_directory ()] returns the project's directory of the current
    opened file if exists **)
val get_current_directory : unit -> directory option

(** [get_files ()] returns the list of all files in the workspace **)
val get_files : unit -> file list

(** [get_projects ()] returns the list of all projects in the workspace **)
val get_projects : unit -> project list

(** [get_directories ()] returns the list of all directories in the workspace **)
val get_directories : unit -> directory list

(** [get_file_from_projects project name] returns the [name] file of the
    [project]  **)
val get_file_from_project : project -> string -> file

(** [get_project_from_file file] returns the [file]'s project **)
val get_project_from_file : file -> project

(** [can_create_dir dir name] returns true if you can create a new directory
    [name] in [dir] **)
val can_create_dir : directory -> string -> bool

(** [can_create_file project name] returns true if you can create a new file
    [name] in [project] **)
val can_create_file : project -> string -> bool

(** [get_path dirtree] returns the relative path of [dirtree]
    since the workspace's root **)
val get_path : dirtree -> string

(** [get_all_projects_path ()] returns the list of all projects' path
    existing in the workspace **)
val get_all_projects_path : unit -> string list

(** [get_file_path file] returns the relative path (since the workspace's
    root) where [file] is located **)
val get_file_path : file -> string

(** [get_file_location file] returns the relative path (since the workspace's
    root) of [file]'s parent directory **)
val get_file_location : file -> string

(** [get_dirtree_from_path path] returns the directory or project corresponding
    to [path].
    Raises [Workspace_closed] if workspace is not opened, and [Bad_path] if
    [path] don't match with a directory or project **)
val get_dirtree_from_path : string -> dirtree

(** [get_file_from_path path] returns the file corresponding to [path].
    Raises [Bad_path] if [path] don't match with a file **)
val get_file_from_path : string -> file

(** [get_project_from_path path] returns the project corresponding to [path].
    Raises [Bad_path] if [path] don't match with a project **)
val get_project_from_path : string -> project

(** [get_directory_from_path path]
    Like [get_project_from_path] but for a directory. Raise [Bad_path] too. **)
val get_directory_from_path : string -> directory

(** [count_opened_files ()] returns the number of opened files in the
    workspace **)
val count_opened_files : unit -> int

(** [count_unsaved_files ()] returns the number of unsaved files in the
    workspace **)
val count_unsaved_files : unit -> int

(** [get_prev_opened_file ()] returns the previous opened file (if exists)
    before the one currently opened **)
val get_prev_opened_file : unit -> file option

(** [get_content file] returns the [file] content if [file] has already been
    opened, otherwise returns [None] **)
val get_content : file -> string option

(** [get_type_of_file file] returns [Module] if [file] is a "ml" file, or
    [Interface] if it's a "mli" file.
    Raises [Bad_file_name] if [file] doesn't have a "ml" or "mli" extension **)
val get_type_of_file : file -> typefile

(** [get_type_of_file t] returns the correct extension for type [t] *)
val get_extension_of_type : typefile -> string

(** [verify_module_name name] verifies if [name] is correct as module name.
    Raises [Invalid_argument] if [name] is not a module name **)
val verify_module_name : string -> unit

(** [verify_file_name name] verifies if [name] is correct as file name.
    Raises [Invalid_argument] if [name] is not a file name **)
val verify_file_name : string -> unit

(** [verify_archive_name name] verifies if [name] has the correct extension.
    Raises [Invalid_argument] if it doesn't match *)
val verify_archive_name : string -> unit

(** [get_edit_settings ()] returns the user's edit settings **)
val get_edit_settings : unit -> edit_conf



(** All function below are main function to modify the user workspace,
    by sending request to the server if necessary before to call the
    function given in argument as a callback **)

(** [open_workspace callback ()] gets back the user workspace stored on the
    server and call [callback] with the root directory of the workspace.
    Raise [Workspace_already_opened] if workspace is already opened (yes,
    there is not trap here !) **)
val open_workspace : (directory -> unit) -> unit -> unit

(** [open_file callback file] loads the [file] content and call [callback] with
    the [file] itself and its content **)
val open_file : (file * string -> unit) -> file -> unit

(** [save_file callback file] saves the current content of [file] stored in
    the filemanager to the file stored on the server and call
    [callback] with [file] **)
val save_file : (file -> unit) -> file -> unit

(** [save_conf callback conf] saves the [conf] file to the server and
    call [callback] with [conf] **)
val save_conf : (conftype * conf -> unit) -> conftype * conf -> unit

(** [unsave_file callback file] labels [file] as an unsaved file and call
    [callback] with [file] **)
val unsave_file : (file -> unit) -> file -> unit

(** [create_file callback project name] creates a file [name] in [project] in
    the filemanager and on the server. And then, call [callback] with the
    file created.
    Raise [Bad_file_name] if a file named [name] already exists in [project] **)
val create_file : (file -> unit) -> project * string -> unit

(** [create_project callback directory name] creates a project [name] in
    [directory] in the filemanager and on the server. And then, call
    [callback] with the project created.
    Raise [Bad_directory_name] if a directory or project named [name]
    already exists in [directory] **)
val create_project : (project -> unit) -> directory * string -> unit

(** [create_directory callback directory name] creates a directory [name] in
    [directory] in the filemanager and on the server. And then, call
    [callback] with the directory created.
    Raise [Bad_directory_name] if a directory or project named [name] already
    exists in [directory] **)
val create_directory : (directory -> unit) -> directory * string -> unit

(** [rename_project callback (project, name)] renames the [project] into [name]
    and call [callback] with the renamed project.
    Raise [Bad_directory_name] if [name] if already an existing project or
    directory in the [project]'s parent directory **)
val rename_project : (project -> unit) -> project * string -> unit

(** [rename_directory callback (directory, name)] renames the [directory] into
    [name] and call [callback] with the renamed directory.
    Raise [Bad_directory_name] if [name] if already an existing project or
    directory in the [directory]'s parent directory
    Raise [Operation_denied] if [directory] is root. **)
val rename_directory : (directory -> unit) -> directory * string -> unit

(** [rename_file callback (file, name)] renames [file] into [name] and call
    [callback] with the renamed file.
    Raise [Bad_file_name] if [name] is already a existing file in [file]'s
    project **)
val rename_file : (file -> unit) -> file * string -> unit

(** [close_workspace callback ()] closes the workspace and reset all datas
    in the filemanager and call [callback] with nothing to notify that
    the workspace has been closed successfully. **)
val close_workspace : (unit -> unit) -> unit -> unit

(** [close_file callback file] closes [file] and removes its content from
    the filemanager and then call [callback] with the closed file. **)
val close_file : (file -> unit) -> file -> unit

(** [delete_file callback file] deletes [file] and all occurences of it in
    the filemanager and on the server and call [callback] with the deleted file.
    *Warning* : the file given in the callback no longer exists in the
    filemanager. **)
val delete_file : (file -> unit) -> file -> unit

(** [delete_project callback project] deletes [project] and all occurences of
    it in the filemanager and on the server and call [callback] with
    the deleted project. Delete also recursively its files.
    *Warning* : the project given in the callback no longer exists in the
    filemanager. **)
val delete_project : (project -> unit) -> project -> unit

(** [delete_directory callback directory]
    Like [delete_project but for a directory **)
val delete_directory : (directory -> unit) -> directory -> unit

(** [import_project callback (dir, filename, file_content)] imports the project
    contained in [filename] (whose binary content is in [file_content]) and
    places it in the directory [dir]. Calls [callback] when the server has
    correctly added in on filesystem *)
val import_project : (project -> unit) -> directory * string * string -> unit

(** [import_file callback (project, name, content)] import a file stored on
    user computer with [name] and [content] in the [project] of the filemanager
    and import it also on the server and call [callback] with the
    imported file.
    Note: if [name] is already an existing file in [project] then it will be
    renamed until it's accepted **)
val import_file : (file -> unit) -> project * string * string -> unit

(** [switch_file callback file] switches the current active file of the
    workspace to [file] and call [callback] with the old current file (if
    exists) and [file] **)
val switch_file : (file option * file -> unit) -> file -> unit

(** [link_before callback file] modifies [file]'s projects's compilation
    settings in order to move up a rung [file] in the file order for the
    project's compilation, and call [callback] with the new compilation
    settings and [file] **)
val link_before : (conftype * conf * file -> unit) -> file -> unit

(** [link_after callback file]
    Like [link_before] but in order to move down a rung [file] **)
val link_after : (conftype * conf * file -> unit) -> file -> unit

(** [compile callback project] compiles [project] and call
    [callback] with the result of the compilation process **)
val compile : (compile_result -> unit) -> project -> unit

val import_library: (file -> unit) -> (project * string) -> unit