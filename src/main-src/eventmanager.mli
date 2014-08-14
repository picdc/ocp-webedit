
open Global


(**
    An [event] is an object which contains a function we call "action" which
    need a ['b] to be called, and return a type ['a].

    The function contains a "callback" argument (because of requests which
    we have to wait the result) and also other arguments as you wish
    to parameter the function behaviour.
      - The callback has a ['a -> unit] type
      - The other arguments needs to have a ['b] type (so use tuples if
          necessary)

    By default, the callback argument is a empty function. You can add
    ['a -> unit] functions to the callback. Thoses functions will be listed
    in an arbitrary order and regrouped to form the final callback function
    which will be given to the "action" when you will trigger with the ['b]
    argument.

    Use the method [add_event] to add a function to the list of events
    (which will be grouped to form the final callback during the trigger)

    Use the method [trigger] to call "action" with all the "events"
    recorded as the "action" callback


    For example:

    You have 2 modules : one want to display the file name when
    it's created and the other the content.
    First you need to create a function for create a file, which need a name
    and a content, and return a type t = { n : string ; c : content }
    Then you will have your create function :
      val create_function : ( t -> unit ) -> (string * string) -> unit
    because the result which will be given to the callback is a type t
    but in order to construct t, you need 2 string (name and content).
    The associate event will be :
      let create_event = new event create_function
      val create_event = (t, string*string) event
    Then the 2 modules will add their events by calling :
      create_event#add_event (fun t -> print_endline t.n); //in module 1
      create_event#add_event (fun t -> print_endline t.c); //in module 2
    And when a random module want to create a file, it will call :
      create_event#trigger ("foo.ml", "let x = 10");
    After this calling, module 1 and 2 will print what they wanted
    by itselves (with a random order).

   [class ['a, 'b] event : action -> object]
**)
class ['a, 'b] event : (('a -> unit) -> 'b -> unit) -> object

  (** [add_event f] adds a event to the list, which be used when [trigger]
      will be called **)
  method add_event : ('a -> unit) -> unit

  (** [method trigger ?after args] calls the "action" with the list of
      events as callback with the [args].
      You can also add a function [after] which will be executed at the end
      of the list of events and won't be recorded as an event **)
  method trigger : ?after:('a -> unit) -> 'b -> unit

end

(** An [action] is centralized / stored here, to be used by all modules
    of ocp-webedit and take any function.
    The "action" is trigged by the method [trigger].

    The [action] class is like the [event] class without callbacks.

    [class ['a, 'b] action : act -> object]
**)
class ['a, 'b] action : ('a -> 'b) -> object

  (** [trigger ?after args] triggers the stored action with [args] and
      return the action value.
      You can also add a function [after] which will be executed after the
      action **)
  method trigger : ?after:('a -> 'b -> unit) -> 'a -> 'b

end

(** A [mutable_action] is like an [action] which you can change the
    action whenever you want

    [class ['a, 'b] mutable_action : act -> object] **)
class ['a, 'b] mutable_action : ('a -> 'b) -> object

  inherit ['a, 'b] action

  (** [set act] changes the action to [act] **)
  method set : ('a -> 'b) -> unit

end


(** [open_workspace] is the event linked to [Filemanager.open_workspace]
    - trigged by [()]
    - provides the [directory] root **)
val open_workspace : (directory, unit) event

(** [close_workspace] is the event linked to [Filemanager.close_workspace]
    - trigged by [()]
    - provides [()] **)
val close_workspace : (unit, unit) event

(** [create_file] is the event linked to [Filemanager.create_file]
    - trigged by the [project] where you want to create the file and its [name]
    - provides the [file] created **)
val create_file : (file, project * string) event

(** [create_project] is the event linked to [Filemanager.create_project]
    - trigged by the [directory] where you want to create the project and
      its [name]
    - provides the [project] created **)
val create_project : (project, directory * string) event

(** [create_directory] is the event linked to [Filemanager.create_directory]
    - trigged by the [directory] where you want to create the directory and
      its [name]
    - provides the [directory] created **)
val create_directory : (directory, directory * string) event

(** [rename_file] is the event linked to [Filemanager.rename_file]
    - trigged by the [file] you want to rename and the new [name]
    - provides the [file] renamed **)
val rename_file : (file, file * string) event

(** [rename_project] is the event linked to [Filemanager.rename_project]
    - trigged by the [project] you want to rename and the new [name]
    - provides the [project] renamed **)
val rename_project : (project, project * string) event

(** [rename_directory] is the event linked to [Filemanager.rename_directory]
    - trigged by the [directory] you want to rename and the new [name]
      Note : [directory] can't be root
    - provides the [directory] renamed **)
val rename_directory : (directory, directory * string) event

(** [open_file] is the event linked to [Filemanager.open_file]
    - trigged by the [file] you want to open and to load its content
    - provides the [file] and its [content] **)
val open_file : (file * string, file) event

(** [close_file] is the event linked to [Filemanager.close_file]
    - trigged by the [file] you want to close and remove its content from
      the filemanager
    - provides the closed [file] **)
val close_file : (file, file) event

(** [save_file] is the event linked to [Filemanager.save_file]
    - trigged by the [file] you want to save
    - provides the saved [file] **)
val save_file : (file, file) event

(** [import_project] is the event linked to [Filemanager.import_project]
    - triggered by the [directory] where the project must be imported, the
    project archive's [name] and its binary [content]
    - provides the corresponding [project] *)
val import_project : (project, directory * string * string) event

(** [import_file] is the event linked to [Filemanager.import_file]
    - trigged by the [project] where you want to import the file, the file's
      [name] and its [content]
    - provides the imported [file]
      Note : the [name] can be changed if there already exists a [name] file
        in [project] **)
val import_file : (file, project * string * string) event

(** [unsave_file] is the event linked to [Filemanager.unsave_file]
    - trigged by the [file] you want to unsave
    - provides the unsaved [file] **)
val unsave_file : (file, file) event

(** [switch_file] is the event linked to [Filemanager.switch_file]
    - trigged by the [file] you want to switch to
    - provides the [previous_file] (focused before the switch) and the
      switched [file] **)
val switch_file : (file option * file, file) event

(** [delete_file] is the event linked to [Filemanager.delete_file]
    - trigged by the [file] you want to delete
    - provides the deleted [file]
      *Warning* : the [file] provided no longer exists in filemanager **)
val delete_file : (file, file) event

(** [delete_project] is the event linked to [Filemanager.delete_project]
    - trigged by the [project] you want to delete
    - provides the deleted [project]
      *Warning* : the [project] provided no longer exists in filemanager **)
val delete_project : (project, project) event

(** [delete_directory] is the event linked to [Filemanager.delete_directory]
    - trigged by the [directory] you want to delete
    - provides the deleted [directory]
      *Warning* : the [directory] provided no longer exists in filemanager **)
val delete_directory : (directory, directory) event

(** [save_conf] is the event linked to [Filemanager.save_conf]
    - trigged by the [conf] you want to save
    - provides the saved [conf] **)
val save_conf : (conftype * conf, conftype * conf) event

(** [link_after] is the event linked to [Filemanager.link_after]
    - trigged by the [file] you want to "link after" in the [file]'s project's
      compilation settings
    - provides the changed [conf] and the [file] **)
val link_after : (conftype * conf * file, file) event

(** [link_before] is the event linked to [Filemanager.link_before]
    - trigged by the [file] you want to "link before" in the [file]'s project's
      compilation settings
    - provides the changed [conf] and the [file] **)
val link_before : (conftype * conf * file, file) event

(** [compile] is the event linked to [Filemanager.compile]
    - trigged by the [project] you want to compile
    - provides the [compile_result] of the compilation process**)
val compile : (compile_result, project) event

(** [goto_next_error] is the event linked to when the compilation process
    returns a (some) error(s) and the user want to be placed where the
    error has been located.
    - trigged by the [project] compiled and a type for formatting a
      compilation error [err_format]
    - provides (file, row, start_col, end_col) the [file], [row],
      the [start_col] and [end_col] if exists, where the error is located **)
val goto_next_error : (file * int * (int * int) option,
                       project * Errors_format.err_format) event

(** [login] is the action for login the user, linked to Login.onclick_signin
    when the module Login has finished to be loaded
    - requires [()]
    - provides [()] **)
val login : (unit, unit) mutable_action

(** [login] is the action for logout the user, linked to Login.onclick_signout
    when the module Login has finished to be loaded
    - requires [()]
    - provides [()] **)
val logout : (unit, unit) mutable_action

(** [save_all callback] saves all unsaved file of the workspace and
    executes [callback] at the end **)
val save_all : (unit -> unit) -> unit

(** [export_directory] is the action linked to the exportation of directories
    or project
    - requires the [dirtree] to export
    - provides [()] **)
val export_directory : (dirtree, unit) action

(** [export_file] is the action linked to the exportation of files *)
val export_file : (file, unit) action

val import_library: (file, project * string) event