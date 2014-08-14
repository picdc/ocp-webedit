
let to_js = ref false
let hex = ref false

let out = ref None

let speclist =
  [ "-js", Arg.String (fun s ->
      out := Some (open_out s);
      to_js := true),
           "Encode the file to be used as a javascript variable named \
<filename>_<extension> in a .js file with the same name. Adds also a\
 \"Provides\" field to be used with js_of_ocaml";
    "-hex", Arg.Unit (fun _ -> hex := true),
           "Encode in hexadecimal instead of decimal"
  ]

(** Encode an integer value into its hexadecimal representation *)
let byte_to_hex i =
  let x1 = i / 16 in
  let x2 = i mod 16 in
  let aux i =
    match i with
    | i when i < 10 -> string_of_int i
    | 10 -> "a" | 11 -> "b" | 12 -> "c" | 13 -> "d" | 14 -> "e" | 15 -> "f"
    | _ -> assert false
  in
  (aux x1)^(aux x2)

(** Encode each char/byte from a file to its (hexa)decimal representation *)
let mlstrdebug filename =
  let to_js = !to_js in
  let fbase = Filename.basename filename in
  let f = Filename.chop_extension fbase in
  let i = ((String.rindex fbase '.')+1) in
  let ext = String.sub fbase i (String.length fbase - i) in
  let js_name = f ^ "_" ^ ext in
  let inc = open_in filename in
  match !out with
  None -> assert false
  | Some out ->
  (* let buf = Buffer.create 503 *)
  try
    if to_js then
      begin
        let header = "//Provides: " ^ js_name ^"\n" in
        let header = header ^ "var " ^ js_name ^ " = \"" in
        output_string out header
      end;
    while true do
      let c = input_byte inc in
      begin
	match c with
	    (* | i when i >= 33 && i <= 126 -> *)
	    (*   Buffer.add_char buf (char_of_int i) *)
	  | i ->
            let v = if !hex then
                byte_to_hex i
              else
                let prefix =
	          if i < 10 then "00"
	          else if i < 100 then "0"
                  else ""
                in
                prefix ^ (string_of_int i)
            in
            let slash = if to_js then "\\\\" else "\\" in
	    output_string out (slash ^ v)
      end;
    done
  with _ ->
    if to_js then output_string out "\""

let _ =
  Arg.parse speclist mlstrdebug "Usage : ./progmagic.byte [options] filename";
  match !out with
  None -> ()
  | Some oc -> close_out oc; out := None

