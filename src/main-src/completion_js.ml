(**
   [print_words l] print all words in the console from a Js.string list
**)
let rec print_words = function
  | [] -> ()
  | w :: l -> Myutils.console w; print_words l


(** Js bindings of OCaml functions **)

(** [compute_completions w] finds all the completions for [w] and keep them in
    memory (to use them a-la-Emacs) **)
let compute_completions w =
  let w = Js.to_string w in
  Autocomplete.compute_completions w

(** [next_completion ()] returns the next completion in the list if completions
    have been computed **)
let next_completion () =
  try
    Js.string (Autocomplete.next_completion () )
  with
    _ -> Js.string ""

(** [add_words_from_string str] reads [str] and adds all the words that could be
    useful for completion **)
let add_words_from_string str =
  let str = Js.to_string str in
  Autocomplete.create_from_string str


(**
   [list_to_js_array l] converts an OCaml list [l] into a Js.Array
**)
let list_to_js_array l =
  let rec convert acc = function
    | [] -> acc
    | w :: l -> convert ((Js.string w) :: acc) l
  in
  let a = Array.of_list (convert [] l) in
  Js.array a

(**
   [find_completion_js w] computes the possible completions for the word [w]
   given
**)
let find_completion_js w =
  let w = Js.to_string w in
  let l = Autocomplete.set_to_list (Autocomplete.find_completion w) in
  (* print_words l *)
  list_to_js_array l

(**
   [new_word_from_js w] adds a new word for future completion
**)
let new_word_from_js w =
  Autocomplete.add_word (Js.to_string w)



let _ =
  (Js.Unsafe.coerce Dom_html.window)##complete <- find_completion_js;
  (Js.Unsafe.coerce Dom_html.window)##newWord <- new_word_from_js;
  (Js.Unsafe.coerce Dom_html.window)##computeCompletions <- compute_completions;
  (Js.Unsafe.coerce Dom_html.window)##nextCompletion <- next_completion
