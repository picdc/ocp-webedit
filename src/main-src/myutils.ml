
open Dom_html
open Js
open Unsafe


(* Utils for Blob Objects *)

open Typed_array

type blob = File.blob

let string_to_blob s =
  let uint8Array : (int -> uint8Array Js.t) Js.constr =
    variable "Uint8Array" in
  let a = jsnew uint8Array(String.length s) in
  for i = 0 to (String.length s) - 1 do set a i (Char.code s.[i]) done;
  
  (* let a = [| a##buffer |] in *)
  let myArray = jsnew Js.array_empty () in
  ignore(myArray##push(a##buffer));
  fun_call (variable "new Blob")
    [| inject myArray ;
       inject (variable "{type: \"application/octet-stream;charset=utf-8\"}") |]

let save_as name content =
  ignore (Js.Unsafe.fun_call (Js.Unsafe.variable "saveAs")
                  [| Js.Unsafe.inject content;
                     Js.Unsafe.inject (Js.string name) |])


(* Utils for Debug *)

let alert str = window##alert(string str)

let console o = Firebug.console##log(o)


(** [my_decode str] :
    Decode a string message [str] sent by the "my_encode" function
    of the server **)
let my_decode str =
  assert ((String.length str mod 3) = 0);
  let max = String.length str / 3 in
  let buf = Buffer.create 503 in
  for i=0 to max-1 do
    let number = int_of_string (String.sub str (i*3) 3) in
    Buffer.add_char buf (Char.chr number)
  done;
  Buffer.contents buf




(* Utils for Dom's operation *)

exception Element_missing of string

let get_element_by_id id =
  Opt.get (document##getElementById (string id))
    (fun () -> raise (Element_missing ("fail get_element_by_id : "^id)))

let query_selector el query =
  Opt.get el##querySelector(string query)
    (fun () ->
      console (string "Failure QuerySelector on #Dom.element :");
      console el;
      failwith ("QuerySelector fail with : "^query))

let query_selector_all el query =
  Dom.list_of_nodeList el##querySelectorAll(string query)

let insert_first n c =
  let p = n##firstChild in
  Dom.insertBefore n c p

let remove_node c =
  let p = c##parentNode in
  let p = Js.Opt.get p (fun _ -> failwith "Echec remove_node") in
  Dom.removeChild p c

let remove_childs p =
  let cl = Dom.list_of_nodeList p##childNodes in
  List.iter (fun c -> Dom.removeChild p c) cl

let hide_element n =
  n##style##display <- Js.string "none"

let display_element n =
  n##style##display <- Js.string ""

let hide_elements nl =
  List.iter (fun n -> hide_element n) nl

let display_elements nl =
  List.iter (fun n -> display_element n) nl

let hide_childs p =
  let cl = Dom.list_of_nodeList p##childNodes in
  List.iter (fun c ->
    match Opt.to_option (Dom_html.CoerceTo.element c) with
      | Some s -> hide_element s
      | None -> ()) cl

let display_childs p =
  let cl = Dom.list_of_nodeList p##childNodes in
  List.iter (fun c ->
    match Opt.to_option (Dom_html.CoerceTo.element c) with
      | Some s -> display_element s
      | None -> ()) cl

let append_childs n cl =
  List.iter (fun c -> Dom.appendChild n c) cl

let apply_on_optnode n f =
  match Js.Opt.to_option n with
  | None -> ()
  | Some e -> f e


(* Unsafe coerce of Dom's nodes *)

let coerceTo_input el =
  match Opt.to_option (Dom_html.CoerceTo.input el) with
  | Some s -> s
  | None -> failwith "coerceTo_input failed"

let coerceTo_button el =
  match Opt.to_option (Dom_html.CoerceTo.button el) with
  | Some s -> s
  | None -> failwith "coerceTo_button failed"

let coerceTo_textarea el =
  match Opt.to_option (Dom_html.CoerceTo.textarea el) with
  | Some s -> s
  | None -> failwith "coerceTo_textarea failed"

let coerceTo_img el =
  match Opt.to_option (Dom_html.CoerceTo.img el) with
  | Some s -> s
  | None -> failwith "coerceTo_img failed"





(* Utils for Ace Editor *)

let get_lines row_start row_end =
  let doc = (Global.editor())##getSession()##getDocument() in
  let res = to_array (str_array (doc##getLines(row_start, row_end))) in
  let res = List.fold_right (fun str acc -> (to_string str)::acc)
    (Array.to_list res) [] in
  String.concat "\n" res



(* Utils for Dom's events (not referenced) *)

type handler = (element Js.t, event Js.t) event_listener

let make_event_oncontextmenu el handler =
  ignore (addEventListener el (Event.make "contextmenu") handler _true)

let make_event_onblur el handler =
  ignore (addEventListener el (Event.make "blur") handler _true)
