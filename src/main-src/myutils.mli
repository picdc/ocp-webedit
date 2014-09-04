(** Utility functions *)

(* Utils for Blob Objects *)
(** Type containing a binary file *)
type blob = File.blob

(** [string_to_blob s] writes [s] into a blob to be downloaded a a binary file
    later *)
val string_to_blob : string -> blob

(** [save as name content] sends a file to user as [name] with [content] *)
val save_as : string -> blob -> unit

(** [my_decode s] decodes a string [s] encoded by [my_encode] (generally sent by
    the server. Same function as Request.my_decode *)
val my_decode : string -> string

(* Utils for Debug *)

(** [alert s] creates an alert with the message [s] *)
val alert : string -> unit

(** [console v] outputs the value [v] in the Javascript's console *)
val console : 'a -> unit

exception Element_missing of string

(* Utils for Dom's operation *)

(** [get_element_by_id s] returns the element with id [s].

    Raise [Element_missing s] if doesn't exist *)
val get_element_by_id : string -> Dom_html.element Js.t

(** [query_selector cont s] finds recursively the child element that matches the
    argument [s] from the element [cont].

    Raise [Element_missing s] if nothing has been found *)
val query_selector : Dom_html.element Js.t -> string -> Dom_html.element Js.t

(** [query_selector_all cont s] returns all the childs that matches [s] from
    [cont] *)
val query_selector_all : Dom_html.element Js.t -> string -> Dom_html.element Js.t list

(** [insert_first c el] inserts [el] as the first child of [c] *)
val insert_first : #Dom.node Js.t -> #Dom.node Js.t -> unit

(** [remove_node n] removes [n] from its parent *)
val remove_node : #Dom.node Js.t -> unit

(** [remove_childs n] removes every child of n *)
val remove_childs: #Dom.node Js.t -> unit

(** [hide_element n] sets the "display" attribute of node [n] to "none" *)
val hide_element: Dom_html.element Js.t -> unit

(** [display_element n] sets the "display" attribute of node [n] to "" *)
val display_element: Dom_html.element Js.t -> unit

(** [hide_elements nl] sets the "display" attribute of all nodes [nl]
    to "none" *)
val hide_elements: Dom_html.element Js.t list -> unit

(** [display_elements nl] sets the "display" attribute of all nodes [nl] to "" *)
val display_elements: Dom_html.element Js.t list -> unit

(** [hide_childs n] sets the "display" attribute of every child of [n] to
    "none" *)
val hide_childs: #Dom.node Js.t -> unit

(** [display_childs n] sets the "display" attribute of every child of [n] to
    "" *)
val display_childs: #Dom.node Js.t -> unit

(** [appendChilds n ln] appends every node in [ln] to [n] *)
val append_childs : #Dom.node Js.t -> #Dom.node Js.t list -> unit

(** [apply_on_optnode optnode f] applies the function [f] if [optnode]
    exists **)
val apply_on_optnode : unit Js.Opt.t -> (unit -> unit)
    -> unit



(* Unsafe coerce of Dom's nodes *)

(** [coerceTo_input el] casts [el] to an inputElement *)
val coerceTo_input : Dom_html.element Js.t -> Dom_html.inputElement Js.t

(** [coerceTo_button el] casts [el] to an buttonElement *)
val coerceTo_button : Dom_html.element Js.t -> Dom_html.buttonElement Js.t

(** [coerceTo_textarea el] casts [el] to an textAreaElement *)
val coerceTo_textarea : Dom_html.element Js.t -> Dom_html.textAreaElement Js.t

(** [coerceTo_img el] casts [el] to an imageElement *)
val coerceTo_img : Dom_html.element Js.t -> Dom_html.imageElement Js.t

(* Utils for Ace Editor *)

(** [get_lines s e] returns the text between line [s] and line [e] *)
val get_lines : int -> int -> string

(* Utils for Dom's events (not referenced) *)

type handler = (Dom_html.element Js.t, Dom_html.event Js.t)
           Dom_html.event_listener

(** [make_event_oncontextmenu el f] adds a onContextMenu event to the element
    [el] that will trigger [f] *)
val make_event_oncontextmenu : Dom_html.element Js.t -> handler -> unit

(** [make_event_onblur el f] adds a onBlur event to the element
    [el] that will trigger [f] *)
val make_event_onblur : Dom_html.element Js.t -> handler -> unit
