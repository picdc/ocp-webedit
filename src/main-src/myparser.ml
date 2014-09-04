
open Global

exception Bad_conf_file


(** [split_first str c] splits [str] into two substrings, using [c] as
    divider. *)
let split_first str c =
  let pos = String.index str c in
  let s1 = String.sub str 0 pos in
  let s2 = String.sub str (pos+1) (String.length str - pos-1) in
  s1, s2

let split str c =
  let rec aux acc str =
    let pos =
      try String.index str c
      with Not_found -> -1 in
    if pos <> -1 then
      let el = String.sub str 0 pos in
      let next = String.sub str (pos+1) (String.length str - pos-1) in
      aux (el::acc) next
    else str::acc
  in
  List.rev (aux [] str)

let parse_to_conf str =
  let get_var line =
    try split_first line '='
    with Not_found -> raise Bad_conf_file
  in
  List.rev (List.fold_left
              (fun acc line -> if line <> "" then (get_var line)::acc
                else acc) [] (split str '\n'))


exception Unknown_conf_var of string

let parse_to_compile_conf conf =
  let files = ref [] in
  let output = ref "" in
  let step (key, value) =
    match key with
      | "output" -> output := value
      | "files" -> files := split value ','
      | "depend" -> if not (value = "") then files := !files @ (split value ',') else ()
      | _ -> raise (Unknown_conf_var key)
  in
  List.iter step conf;
  { cc_files = !files; cc_output = !output }

let parse_to_edit_conf conf =
  let theme = ref "" in
  let step (key, value) =
    match key with
      | "theme" -> theme := value
      | _ -> raise (Unknown_conf_var key)
  in
  List.iter step conf;
  { ec_theme = !theme }
  

let generate_of_conf conf =
  let buf = Buffer.create 503 in
  List.iter (fun (key,value) ->
    let s = Format.sprintf "%s=%s\n" key value in
    Buffer.add_string buf s) conf;
  Buffer.contents buf


let generate_of_compile_conf cconf =
  let files = List.filter (fun s -> s <> "") cconf.cc_files in
  let sources = List.filter (fun file -> not (Filename.check_suffix file ".cma")) files in
  let libraries = List.filter (fun file -> Filename.check_suffix file ".cma") files in
  let concat = String.concat "," in
    ["files", concat sources ; "output", cconf.cc_output; "depend", concat libraries]

let generate_of_edit_conf econf =
  [ "theme", econf.ec_theme ]
