
open Global


(** [split str c] splits [str] into a string list, using c as divider.
    Should behave like Stdlib's String.split.
*)
val split : string -> char -> string list

exception Bad_conf_file
exception Unknown_conf_var of string

(** [parse_to_conf str] parses [str] and returns the coorresponding conf *)
val parse_to_conf : string -> conf

(** [parse_to_compile_conf c] returns the correct compile_conf from [c] *)
val parse_to_compile_conf : conf -> compile_conf

(** [parse_to_edit_conf c] returns the correct edit_conf from [c] *)
val parse_to_edit_conf : conf -> edit_conf

(** [generate_of_conf c] generates the string representation of [c] *)
val generate_of_conf : conf -> string

(** [generate_of_compile_conf cc] generates the conf representation of [cc] *)
val generate_of_compile_conf : compile_conf -> conf

(** [generate_of_edit_conf ec] generates the conf representation of [ec] *)
val generate_of_edit_conf : edit_conf -> conf
