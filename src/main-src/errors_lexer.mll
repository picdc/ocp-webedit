{
  (**
     Parses the output from ocamlc in case of compilation error, in order to
     create links to highlight errors like emacs output
  *)

  open Errors_format

  (** Errors found in the output *)
  let errors = ref []

  (** [error_line_column f l s e] prints a formatted error output and the "a"
      markup for the highligth function *)
  let error_line_column file line start _end =
    let nb = List.length !errors in
    errors := { file; line; chars = Some (start, _end) } :: !errors;
    let file = Format.sprintf "<span \
    style=\"color:red;font-weight:bold\">%s</span>" file in
    let line = Format.sprintf "<span style=\"color:green\">%d</span>" line in
    let res = Format.sprintf "File \"%s\", line %s, characters %d-%d:"
      file line start _end in
    Format.sprintf "<a id=\"error%d\" class=\"error-link\">%s</a>@." nb res

  (** [error_line f l] behave as [error_line_column] in case there is no columns
      indication in the output *)
  let error_line file line =
    let nb = List.length !errors in
    errors := { file; line; chars = None } :: !errors;
    let file = Format.sprintf "<span \
    style=\"color:red;font-weight:bold\">%s</span>" file in
    let line = Format.sprintf "<span style=\"color:green\">%d</span>" line in
    let res =
      Format.sprintf "File \"%s\", line %s:"
        file line in
    Format.sprintf "<a id=\"error%d\" class=\"error-link\">%s</a>@." nb res

}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = digit+
let number = digit+ ('.')? digit*
let char = (alpha | digit | '_' | '-' | '/' | '.')
let ident = (alpha | '_') (alpha | '_' | digit)*
let space = (' ' | '\t' | '\r' | '\n')
let ext = ('i'|'l'|'y')

rule line = parse
  | "File \"" (ident".ml"ext? as file)
      "\", line " (integer as line)
      ", characters " (integer as start) "-" (integer as _end) ":\n"
      { error_line_column
          file (int_of_string line)
          (int_of_string start)
          (int_of_string _end) }
  | "File \"" (ident".ml"ext? as file)
      "\", line " (integer as line)  ":" space
      { error_line file (int_of_string line) }
  | eof { raise End_of_file }
  | "\n" { "\n" }
  | _ as s { Char.escaped s }

{

  (** [parse_compile_ouput output] parses the [output] given and formats all the
      error messages *)
  let parse_compile_output output =
    errors := [];
    let lexbuf = Lexing.from_string output in
    let buff = Buffer.create 501 in
    try
      while true do
        let read = line lexbuf in
        Buffer.add_string buff read
      done;
      Buffer.contents buff, !errors (* Unreachable *)
    with
        End_of_file -> Buffer.contents buff, !errors

}
