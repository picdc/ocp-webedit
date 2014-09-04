
type menuItem
type menuButton

(** A menu bar is made up of a list of [menuButton].
    And a [menuButton] is made of a list of [menuItem] shown when
    [menuButton] is pressed **)

(** [create_menu_item str ?activate callback] creates a [menuItem] with the
    string [str] as content and trigger [callback] when the user clicks on it
    only if [activate] is set to true.
    Show the [shortcut] associated to the callback's action if exists. **)
val create_menu_item : string -> ?activate:bool -> ?shortcut:string ->
  (unit -> unit) -> menuItem

(** [create_memu_button name menuItems] creates a [menuButton] with a list of
    [menuItem] and the button's content is set to a string [name] **)
val create_menu_button : string -> menuItem list -> menuButton    

(** [create_menu_bar container menuButtons] : create a menu bar with a list of
    [menuButton] in the [container] **)
val create_menu_bar : #Dom_html.element Js.t -> menuButton list -> unit



(** The Menu's eventlistener, ready for [Widget.make] **)
val t : Global.eventlistener
