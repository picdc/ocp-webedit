open Myutils
open Dom_html

(** [textarea_line_size] is the height of a text line for the input (in px) **)
let textarea_line_size = 14

(** [new_prompt ()] returns a new prompt DOM element **)
let new_prompt () =
  let el = createDiv document in
  el##className <- Js.string "toplvl_prompt";
  el##innerHTML <- Js.string "#";
  el

(** [print_input str] prints the input text [str] into the matched area **)
let print_input str =
  let output = Myutils.get_element_by_id "toplvl_area_output" in
  let div = createDiv document in
  let prompt = new_prompt () in
  let text = createDiv document in
  text##className <- Js.string "toplvl_input_text";
  text##innerHTML <- Js.string str;
  Dom.appendChild div prompt;
  Dom.appendChild div text;
  Dom.appendChild output div

(** [print_eval str] prints the eval text [str] into the matched area **)
let print_eval str =
  let output = Myutils.get_element_by_id "toplvl_area_output" in
  let div = createDiv document in
  div##className <- Js.string "toplvl_output_text";
  div##innerHTML <- Js.string str;
  Dom.appendChild output div

(** [print_ouptut str] prints the output text [str] into the matched area **)
let print_output str =
  let text = Dom_html.document##createTextNode(Js.string str) in
  let container = Myutils.get_element_by_id "output" in
  Dom.appendChild container text



open Webworker

(** [worker] is the WebWorker for the toplevel **)
let worker = jsnew webWorker(Js.string "toplevel.js")

(** The input message type to send to the toplevel WebWorker **)
type toplevel_worker_msg = Reset | Eval of string

(** The result type for the output message of the toplevel WebWorker **)
type toplevel_worker_res = { eval : string ; output : string }



let execute str callback =
  worker##onmessage <- (fun ev -> callback (Json.unsafe_input ev##data));
  worker##postMessage(Json.output (Eval (str)))

(** [eval_printer result] prints the webworker's [result] **)
let eval_printer res =
  print_eval res.eval;
  print_output res.output


let evaluate_input () =
  let input = Myutils.coerceTo_textarea
    (Myutils.get_element_by_id "toplvl_area_input_textarea") in
  let text = Js.to_string input##value in
  print_input text;
  let callback =
    input##value <- Js.string "";
    input##style##height <- Js.string
      (Format.sprintf "%dpx" textarea_line_size);
    eval_printer
  in
  execute text callback


let evaluate_selection () =
  let editor = Global.editor () in
  let doc = editor##getSession()##getDocument() in
  let range = editor##getSelectionRange() in
  let text = Js.to_string doc##getTextRange(range) in
  print_input text;
  execute text eval_printer


let reset_toplevel () =
  let input = Myutils.coerceTo_textarea
    (Myutils.get_element_by_id "toplvl_area_input_textarea") in
  let output = Myutils.get_element_by_id "toplvl_area_output" in
  input##value <- Js.string "";
  input##style##height <- Js.string
    (Format.sprintf "%dpx" textarea_line_size);
  output##innerHTML <- Js.string "";
  worker##postMessage(Json.output (Reset))


let make_output () =  get_element_by_id "output"
let make_toplevel () =
  let toplevel = get_element_by_id "toplevel" in
  let toplvl_area = get_element_by_id "toplvl_area" in
  let input_textarea =
    let ele = get_element_by_id "toplvl_area_input_textarea" in
    Js.Opt.get (CoerceTo.textarea ele)  (fun _ -> assert false)
in
(*
  let toplevel = createDiv document in
  let toplvl_area = createDiv document in
  let toplvl_buttons = createDiv document in
  let area_output = createPre document in
  let area_input = createDiv document in
*)
  let button_reset = get_element_by_id "toplvl_button_reset" in
  let button_eval = get_element_by_id "toplvl_button_eval" in
  let button_eval_select = get_element_by_id "toplvl_button_eval_select" in
(*
  let input_prompt = new_prompt () in
  let input_textarea = createTextarea document in

  toplevel##id <- Js.string "toplevel";
  toplvl_area##id <- Js.string "toplvl_area";
*)
  toplvl_area##onclick <- handler (fun _ ->
    input_textarea##focus(); Js._true);
(*
  toplvl_buttons##id <- Js.string "toplvl_buttons";
  area_output##id <- Js.string "toplvl_area_output";
  area_input##id <- Js.string "toplvl_area_input";
  button_eval##id <- Js.string "toplvl_button_eval";
  button_eval_select##id <- Js.string "toplvl_button_eval_select";
  button_reset##id <- Js.string "toplvl_button_reset";
  input_textarea##id <- Js.string "toplvl_area_input_textarea";
  input_textarea##className <- Js.string "toplvl_input_text";
  input_textarea##style##height <-
    Js.string (Format.sprintf "%dpx" textarea_line_size);
*)
  input_textarea##onkeypress <- handler (fun kev ->
    if kev##keyCode == 13 then
      (let hpx = Js.to_string (input_textarea##style##height) in
       let h = String.sub hpx 0 (String.length hpx - 2) in
       let new_h = (int_of_string h) + textarea_line_size in
       let new_hpx = Format.sprintf "%dpx" new_h in
       input_textarea##style##height <- Js.string new_hpx);
    Js._true);
(*
  button_eval##innerHTML <- Js.string "Evaluate";
  button_eval_select##innerHTML <- Js.string "Evaluate Selection";
  button_reset##innerHTML <- Js.string "Reset env.";
*)
  button_eval##onclick <- handler (fun _ ->
    evaluate_input (); Js._true);
  button_eval_select##onclick <- handler (fun _ ->
    evaluate_selection (); Js._true);
  button_reset##onclick <- handler (fun _ ->
    reset_toplevel (); Js._true);

(*
  Dom.appendChild area_input input_prompt;
  Dom.appendChild area_input input_textarea;
  Dom.appendChild toplvl_area area_output;
  Dom.appendChild toplvl_area area_input;
  Dom.appendChild toplvl_buttons button_eval;
  Dom.appendChild toplvl_buttons button_eval_select;
  Dom.appendChild toplvl_buttons button_reset;
  Dom.appendChild toplevel toplvl_area;
  Dom.appendChild toplevel toplvl_buttons;
*)
  worker##postMessage(Json.output (Reset));

  (* For the shortcut *)
  (Js.Unsafe.coerce window)##evaluateSelection <-
    Js.wrap_callback evaluate_selection;

  toplevel
