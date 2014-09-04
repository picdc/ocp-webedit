
open Global

(**
   We provides 4 dialog types for the user interaction :

    - right click dialog : is an handler list when the user did a
        right click with his mouse
    - alert : like javascript's "window.alert()", but more pretty
    - confirm : like javascript's "window.confirm()", but more pretty
    - prompt : like javascript's "window.prompt()", but more pretty and
        with more fields in the same window
**)

(** Items for prompt **)
type dialogInput

(** [dialogInput_text ~name ~label ?default ()] creates a dialogInput item
    made up with a "input" with type "text", [name], a [label] and a default
    value [default] **)
val dialogInput_text : name:string -> label:string -> ?default:string -> unit
  -> dialogInput

(** [dialogInput_slect ~name ~label ~values ?default ()] creates a dialogInput
    item made up with a "select" element with [name], a [label] and a default
    value [default].
    You can also give a function which will be executed [onchange], when
    the value change, with the current values? **)
val dialogInput_select : name:string -> label:string -> values:string list ->
  ?default:string option -> ?onchange:(string -> unit) -> unit -> dialogInput

(** [dialogInput_import ~name ~label] creates a dialogInput item
    made up with a "input" with type "file", [name], and a [label] **)
val dialogInput_import : name:string -> label:string -> dialogInput

(** [dialogInput_file ~name ?default ()] creates a dialogInput item
    in order to select a workspace's file with the default value
    [default] **)
val dialogInput_file : name:string -> ?default:file option -> unit
  -> dialogInput

(** [dialogInput_project ~name ?default ()] creates a dialogInput item
    in order to select a workspace's project with the default value
    [default] **)
val dialogInput_project : name:string -> ?default:project option -> unit
  -> dialogInput

(** [dialogInput_directory ~name ?with_root ?default ()] creates a dialogInput
    item in order to select a workspace's directory with the default value
    [default]. If [with_root] is true then, it allows to select root
    directories. **)
val dialogInput_directory : name:string -> ?with_root:bool
  -> ?default:directory option -> unit -> dialogInput

(** [dialogInput_dirtree ~name ?default ()] creates a dialogInput item
    in order to select a workspace's directory or project with the default value
    [default] **)
val dialogInput_dirtree : name:string -> ?default:dirtree option -> unit
  -> dialogInput



(** [Right_click_dialog] is the module that manage all "right click" dialogs **)
module Right_click_dialog : sig

  (** Type for the "right click" dialog **)
  type t

  (** [show t x y] shows a "right click" dialog [t] at the page
      coord ([x],[y]) **)
  val show : t -> int -> int -> unit

  (** [hide t] hides a "right click" dialog [t] **)
  val hide : t -> unit

    (** [create labels handlers] creates a "right click" dialog by using
        and associating [labels] list with [handlers] list
        *Warning* : [labels] and [handlers] must have the same length
           and the first label will be associate with the first handler
           and so on. **)
  val create : string -> (string * (unit -> unit)) list -> t

  (** [hide_all ()] hides all existing "right click" dialogs **)
  val hide_all : unit -> unit

end

(** [alert ~title ~text ?callback ()] creates and displays an "alert" dialog
    with [title] and [text] as content. [callback] will be called after
    user clicks on the "Continue" button (by default the [callback] does
    nothing) **)
val alert : title:string -> text:string -> ?callback:(unit -> unit) ->
  unit -> unit

(** [confirm ~title ~text ~callback ()] creates and displays a "confirm"
    dialog with [title] and [text] as content, and call [callback] if the
    user clicks on "Confirm". Do nothing if user clicks on "Cancel" **)
val confirm : title:string -> text:string -> callback:(unit -> unit) -> unit

(** [prompt ~title ~inputs ~callback ()] creates and displays a "prompt"
    dialog with [title] as title content, and [inputs], a list of [dialogItem],
    associated with a [callback].
    This dialog can ask as many [dialogItem] as you want, and the [callback]
    takes as argument a [(string * string) list] which corresponds to
    the (name, value) list of all [dialogItem]'s results.
    If you make a single [dialogItem] with name "foobar", then the callback
    will receive a list of one element : ("foobar", what_the_user_entered)
    [cancel] is the optionnal callback when user click on the cancel button.

    *Warning* : the name choice for [dialogInput]s are important to parse
      the result list in [callback] **)
val prompt : title:string -> inputs:dialogInput list ->
  callback:((string * string) list -> unit) -> ?cancel:(unit -> unit) ->
  unit -> unit




(** [prompt_select_file callback] asks the user to choose an existing
    file and call the [callback] with the choosen file as argument **)
val prompt_select_file : (file -> unit) -> unit

(** [prompt_select_project callback] asks the user to choose an existing
    project and call the [callback]  with the choosen project as argument **)
val prompt_select_project : (project -> unit) -> unit

(** [prompt_select_directory callback] asks the user to choose an existing
    directory and call the [callback] with the choosen directory as argument **)
val prompt_select_directory : (directory -> unit) -> unit

(** [prompt_create_project ?dir ()] asks the user to create a new
    project (located in [dir] by default) **)
val prompt_create_project : ?dir:directory option -> unit -> unit

(** [prompt_create_directory ?dir ()] asks the user to create a new
    directory (located in [dir] by default) **)
val prompt_create_directory : ?dir:directory option -> unit -> unit

(** [prompt_rename_project ?project ()] asks the user the change the
    [project]'s name **)
val prompt_rename_project : ?project:project option -> unit -> unit

(** [prompt_rename_directory ?dir ()] asks the user the change the [dir]'s
    name **)
val prompt_rename_directory : ?dir:directory option -> unit -> unit

(** [prompt_new_module ?project ()] asks user to create an new .ml file
    (by default in [project]) **)
val prompt_new_module : ?project:project option -> unit -> unit

(** [prompt_new_interface project ()] asks user to create an new .mli file
    (by default in [project]) **)
val prompt_new_interface : ?project:project option -> unit -> unit

val prompt_new_lexer : ?project:project option -> unit -> unit

val prompt_new_grammar : ?project:project option -> unit -> unit

(** [prompt_rename_file file ?default ()] asks user to rename the [file]
    with [default] as default value for the new module's name **)
val prompt_rename_file : file -> ?default:string -> unit -> unit

(** [prompt_import_project ?dir ()] asks the user to import a local tgz
    archive as a project into [dir] *)
val prompt_import_project : ?dir:directory option -> unit -> unit

(** [prompt_import_file ?project ()] asks the user to import a local file
    into the server storage in the [project] **)
val prompt_import_file : ?project:project option -> unit -> unit


val prompt_import_library: ?project:project option -> unit -> unit

(** [prompt_compile ?project] asks the user which project he wants to
    compile (by default [project]) **)
val prompt_compile : ?project:project option -> unit -> unit

(** [prompt_compile_settings ?project] asks user to change the compile
    settings of a project (by default [project]) **)
val prompt_compile_settings : ?project:project option -> unit -> unit

(** [prompt_edit_settings ?theme] asks user to change his edit
    settings.
    [theme] is the editor's theme. **)
val prompt_edit_settings : ?theme:string -> unit -> unit

val prompt_delete_file : Global.file -> (unit -> unit) -> unit

val prompt_delete_project : Global.project -> (unit -> unit) -> unit

val prompt_delete_directory : Global.directory -> (unit -> unit) -> unit

val prompt_login : success:(string->unit) -> ?failure:(exn->unit) -> unit -> unit
