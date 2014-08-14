
(** [pull_request ~success ?failure ~meth ~url ~asyn ~msg] :
    Send a [meth] request to [url] service of our server with the text [msg].
    The request is asynchronous if [asyn] is true.
    [meth] is a type of request like POST, GET, HEAD, PUT, ...
    When the server reply successfully (ie with a code 200) the callback
    [success] is called, otherwise [failure] is called with the error code and
    message **)
val pull_request : success:(string -> unit) -> ?failure:(exn -> unit) ->
  ?meth:string -> url:string -> ?asyn:bool -> msg:string -> unit -> unit


(** All functions below send a request to the server with a callback if the
    request is a success (can also have a callback for failure), and also
    arguments for the service they will call.

    The server's response is always a string, and can be parsed and
    interpreted by the function before the call the callback with appropriate
    arguments.

    By default, cookies are always send and they contains the user
    identification (that's why we don't need a "user" argument **)


(** Return the workspace of the user **)
val get_workspace :
  callback:(Global.s_dirtree -> unit) -> unit

(** Get the content of a [name] file located in [path] **)
val get_file_content :
  callback:(string -> unit) -> path:string -> name:string -> unit

(** Create a "project" directory in [path] with the name [name] **)
val create_project :
  callback:(unit -> unit) -> path:string -> name:string -> unit

(** Create a normal directory in [path] with the name [name] **)
val create_directory :
  callback:(unit -> unit) -> path:string -> name:string -> unit

(** Create an empty file [name] located in [path] **)
val create_file :
  callback:(unit -> unit) -> path:string -> name:string -> unit

(** Save a file [name] located in [path] by writing the [content] into
    this file **)
val save_file :
  callback:(unit -> unit) -> path:string -> name:string -> content:string -> unit

(** Import a file, ie create a file [name] in [path], and save
    the [content] of the file **)
val import_file :
  callback:(unit -> unit) -> path:string -> name:string -> content:string -> unit

(** Rename [newname] a file [name] in [path] **)
val rename_file :
  callback:(unit -> unit) -> path:string -> name:string -> newname:string -> unit

(** Like [rename_file] but for a directory/project **)
val rename_directory :
  callback:(unit -> unit) -> path:string -> name:string -> newname:string -> unit

(** Delete a directory/path [name] located in [path]
    Warning : this action can not be undone **)
val delete_directory :
  callback:(unit -> unit) -> path:string -> name:string -> unit

(** Like [delete_directory] but for a file **)
val delete_file :
  callback:(unit -> unit) -> path:string -> name:string -> unit

(** Imports the project contained in [file] (and its binary [content]) in the
    directory [path], and calls [callback] with the s_dirtree sent by the server *)
val import_project :
  callback:(Global.s_dirtree -> unit)
  -> path:string -> file:string -> content:string
  -> unit

(** Download a directory/project [name] in [path],
    Note: export_file not implemented yet **)
val export_directory :
  callback:(unit -> unit) -> path:string -> name:string -> unit

(** Like [save_file] for a configuration file (but they are less
    server side verifications) **)
val save_conf :
  callback:(unit -> unit) -> path:string -> name:string -> content:string -> unit

(** Like [get_file_content] for the same reason of [save_conf].
    [failure] is the function to execute if the load request failed. **)
val load_conf :
  callback:(string -> unit) -> failure:(exn -> unit) -> path:string -> 
  name:string -> unit


val compile_project:
  callback:(Global.compile_result -> unit) 
  -> path: string -> obj: string
  -> unit

val load_lib: callback: (string list -> unit) -> unit

val install_lib: callback: (unit -> unit) -> path:string -> library:string -> unit
