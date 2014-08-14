(**
 ocamlc -I ~/.opam/4.00.1/lib/re re.cma re_emacs.cma re_str.cma -o autocomplete autocomplete.ml && ./autocomplete
**)

(**
    Autocomplete is a really small module to parse a a file/string and add
    possible words that could be used as completions. It only uses the words
    from "let" declarations.
**)


open Completion_data

(** Creation functions **)

(** [add_word w] adds the word [w] in the dictionnary *)
let add_word = new_word

(** [create_from_channel str] parses [str] to find all variable declarations and
    adds them in the dictionnary *)
let create_from_string str =
  Completion_lexer.parse_string str

(** [create_from_channel ch] parses [ch] to find all variable declarations and
    adds them in the dictionnary *)
let create_from_channel ch =
  Completion_lexer.parse_channel ch

(** Utils functions **)

(** [set_to_list s] return a list corresponding to the set [s] *)
let set_to_list s =
  List.rev (Words.fold (fun elt l -> elt :: l) s [])

(** [set_to_array s] return an array corresponding to the set [s] *)
let set_to_array s =
  let a = Array.make (Words.cardinal s) "" in
  ignore (Words.fold (fun elt i -> a.(i) <- elt; i+1) s 0);
  a

(** [print_word_from_set s] will print every word from the set [s] *)
let print_word_from_set s =
  let rec print = function
    | [] -> []
    | w :: l -> Format.printf "%s@." w; print l
  in
  print (set_to_list s)

(** Functions to compute completion **)

(** [find_completion w] will find all the word that can be completed from [w] *)
let find_completion w =
  origin := w;
  let re = "^" ^ w ^ ".*" in
  let re = Re_str.regexp re in
  (* let re = Re.compile re in *)

  let rec step env acc =
    Words.fold
      (fun s acc ->
        (* Format.printf "%s@." s; *)
        if  Re_str.string_match re s 0 then
          begin
            Words.add s acc
          end
        else acc)
      env
      acc
  in
  step !words Words.empty

(** [compute_completions w] will finds the possible completions for [w] and
    sets the environnement to iterate over words found*)
let compute_completions w =
  let words = find_completion w in
  let words = set_to_array words in
  completions := words;
  actual_index := 0

exception No_completion

(** [next_completion ()] will return the next possible completion found by
    compute_completions, or the actual word if there is no match *)
let next_completion () =
  if Array.length !completions = 0 then !origin
  else
    let n = !completions.(!actual_index) in
    actual_index := (!actual_index + 1) mod Array.length !completions;
    n
