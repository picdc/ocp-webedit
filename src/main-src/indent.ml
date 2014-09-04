
open Global
open Dom_html

(** [all_breakpoints] is the storage for all file's breakpoints **)
let all_breakpoints = Hashtbl.create 19

(** [breakpoints] is the list of the current file's breakpoints **)
let breakpoints = ref []

let forceIndent = ref false

(** [get_best_breakpoint row] returns the best breakpoints above [row] **)
let get_best_breakpoint row =
  List.fold_left (fun acc l ->
    if l <= row && l > acc then l else acc) 0 !breakpoints


(** [kind_ext] is the kind that ocp-indent needs to parse the code.
    In the function [fun block elt (line, acc)], [acc] is the integer list
    corresponding to the indentation of each line of the code (with decreasing
    order of line numbers).
    This function also add new breakpoints if it finds them **) 
let kind_ext = IndentPrinter.Extended (fun block elt (line, acc) ->
  let line = match elt with
      IndentPrinter.Newline -> line + 1 | _ -> line in
  (* [breakpoints] update *)
  if IndentBlock.is_at_top block then
    (let bkpts = !breakpoints in
     if not (List.mem line bkpts) then
	 breakpoints := line::bkpts);
  (* [acc] update *)
  match elt with
  | IndentPrinter.Newline -> line, (IndentBlock.indent block)::acc
  | _-> line, acc
)


(** [get_minimum_text rowstart rowend] returns the minimal text needed for
    doing a good indentation for lines [rowstart] to [rowend], thanks to
    [breakpoints] **)
let get_minimum_text rowstart rowend =
  let doc = (Global.editor())##getSession()##getDocument() in
  let start = get_best_breakpoint rowstart in
  let col = String.length (Js.to_string (doc##getLine(rowend))) in
  let range =  Ace.range start 0 rowend col in
    Js.to_string (doc##getTextRange(range))



(** [call_ocp_indent str offset] calls ocp-indent with the code [str] and
    [offset] which is the line number of the first line of [str] in the whole
    text.
    Return the indentation for each line of [str] in a reversal order
    (ie the first line's indentation is at the end of the list) **)
let call_ocp_indent str offset =
  let output = {
    IndentPrinter.
    debug = false;
    config = IndentConfig.default;
    in_lines = (fun _ -> true);
    adaptive = false;
    indent_empty = true;
    kind = kind_ext; } in
  let stream = Nstream.of_string str in
  snd (IndentPrinter.proceed output stream IndentBlock.empty (offset, [0]))


(** [get_indent_size line line] returns the number of white-spaces making up
    the indentation of the [line] **)
let get_indent_size line =
  let size = String.length line in
  let tab_size = (Global.editor())##getSession()##getTabSize() in
  let rec aux i =
    if i >= size then i
    else
      (let c = String.get line i in
       if c = ' ' then aux (i+1)
       else if c = '\t' then aux (i+tab_size)
       else i) in
  aux 0


(** [replace_indent row n] replaces the indentation's white-spaces for the line
    [row] with [n] white-spaces (for the current editSession) **)
let replace_indent row n =
  let doc = (Global.editor())##getSession()##getDocument() in
  let size = get_indent_size (Js.to_string doc##getLine(row)) in
  let range = Ace.range row 0 row size in
  let new_indent = String.make n ' ' in
  doc##replace(range, Js.string new_indent)


(** [indent_line row] indents the line [row] of the current editSession **)
let indent_line row =
  let text = get_minimum_text row row in
  let offset = get_best_breakpoint row in
  let res = List.hd (call_ocp_indent text offset) in
  replace_indent row res
      
(** [indent_region now_start row_end] indents lines until [row_start] to
    [row_end] of the current editSession **)
let indent_region row_start row_end  =
  if row_start = row_end then indent_line row_start
  else
    (let text = get_minimum_text row_start row_end in
     let offset = get_best_breakpoint row_start in
     let res = call_ocp_indent text offset in
     ignore (List.fold_left (fun row n ->
       if row >= row_start then replace_indent row n;
       row - 1) row_end res))


(** [get_indent_next_line row] returns the js_string corresponding to the
    good indentation for the next line of the line [row] **)
let get_indent_next_line row : Js.js_string Js.t =
  let _ = if row > 0 && !forceIndent then indent_region 0 (row - 1) in
  let text = get_minimum_text row row in
  let offset = get_best_breakpoint row in
  let res = List.hd (call_ocp_indent text offset) in
  Js.string (String.make res ' ')


(** [notify_change_on_row row] notifies that the [row] has been changed,
    so all breakpoints after this line are no longer valid **)
let notify_change_on_row row =
  breakpoints := List.filter (fun i -> i < row) !breakpoints


let main () =
  (* Events management *)
  let callback_create_file file =
    let _ = forceIndent :=
      if Filename.check_suffix file.f_name ".ml" || Filename.check_suffix file.f_name ".mli" then true
      else false
    in
    let id = file.f_id in
    if Hashtbl.mem all_breakpoints id then
      Hashtbl.remove all_breakpoints id;
    Hashtbl.add all_breakpoints id [] in
  let callback_open_file (file, _) =
    callback_create_file file in
  let callback_switch_file (old_file, file) =
    let _ = forceIndent :=
      if Filename.check_suffix file.f_name ".ml" || Filename.check_suffix file.f_name ".mli" then true
      else false in 
    begin
      match old_file with
      | None -> ()
      | Some old_file ->
	       Hashtbl.replace all_breakpoints old_file.f_id !breakpoints
    end;
    try breakpoints := Hashtbl.find all_breakpoints file.f_id
    with _ -> failwith "Not_found in callback_switch_file in indent.ml"
  in
  Eventmanager.import_file#add_event callback_create_file;
  Eventmanager.switch_file#add_event callback_switch_file;
  Eventmanager.open_file#add_event callback_open_file;
  Eventmanager.create_file#add_event callback_create_file;

  (* Functions which can be called by Javascript *)
  (Js.Unsafe.coerce Dom_html.window)##getIndentLine <- Js.wrap_callback
    get_indent_next_line;
  (Js.Unsafe.coerce Dom_html.window)##indentLine <- Js.wrap_callback
    indent_line;
  (Js.Unsafe.coerce Dom_html.window)##indentRegion <- Js.wrap_callback
    indent_region;
  (Js.Unsafe.coerce Dom_html.window)##indentNotifyChange <-
    Js.wrap_callback notify_change_on_row
