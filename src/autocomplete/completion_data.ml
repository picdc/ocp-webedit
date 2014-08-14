(**
   Contains all the words for autocompletion
**)

module Words = Set.Make(String)

(** Dictionnary used by the autocompletion module *)
let words = ref Words.empty

(** Word that is going to be completed *)
let origin = ref ""

(** Words that matches to complete [origin] *)
let completions = ref (Array.make 0 "")

(** Index from the next word to be seen in [completions] *)
let actual_index = ref 0

(** [new_word w] adds the word [w] in the dictionnary *)
let new_word w =
  words :=  Words.add w !words
