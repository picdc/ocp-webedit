
open Global
open Dom_html
open Myutils

module H = Hashtbl (* we're lazy *)

(** [tab_width] is the width of the tab in DOM (in pixel) **)
let tab_width = 100

(** [is_list_show] is true if the list of hidden tabs is shown **)
let is_list_shown = ref false

(** [offset] is the number of tabs which are hidden on the left of the
    most left-positionned visible one **)
let offset = ref 0

(** [id_list] is the ordered list of tabs' id **)
let id_list = ref []

(** [update_classes_for_file file] updates the className property of 
    [file]'s tab and hidden tab depends on if [file] is active, or unsaved,
    or not **)
let update_classes_for_file file =
  let t = get_element_by_id (Format.sprintf "tabs_num%d" file.f_id) in
  let h = get_element_by_id (Format.sprintf "hiddentabs_num%d" file.f_id) in
  let st, sh = match Filemanager.get_current_file () with
    | Some f when f.f_id = file.f_id ->
      "tabs tabs_active", "hiddentabs hiddentabs_active"
    | _ -> "tabs", "hiddentabs" in
  let st, sh =
    if file.f_is_unsaved then (st^" tabs_unsaved"), (sh^" hiddentabs_unsaved")
    else st, sh in
  t##className <- Js.string st;
  h##className <- Js.string sh


(** [get_line_max_width ()] returns the maximum number of pixel you can use
    for showing tabs (depends of the user's window's size) **)
let get_line_max_width () =
  let w_sc_left = (get_element_by_id "tabs_scleft")##clientWidth in
  let w_sc_right = (get_element_by_id "tabs_scright")##clientWidth in
  let w_show_all = (get_element_by_id "tabs_showall")##clientWidth in
  let w_container = (get_element_by_id "tabs")##clientWidth in
  w_container - w_sc_left - w_sc_right - w_show_all

(** [get_max_shown_tabs ()] returns the maximum number of tabs which can be
    shown **)
let get_max_shown_tabs () =
  get_line_max_width () / (tab_width + 4) (* 4 = for borders *)



(** [display_navigation_buttons b] enables tabs' navigation buttons if 
    necessary, otherwises disable those buttons **)
let display_navigation_buttons () =
  let c1 = coerceTo_input (get_element_by_id "tabs_scleft_button") in
  let c2 = coerceTo_input (get_element_by_id "tabs_scright_button") in
  let c3 = coerceTo_input (get_element_by_id "tabs_showall_button") in
  let n = Filemanager.count_opened_files () - !offset in
  let b1 = !offset <> 0 in
  let b2 = n > 1 in
  let b3 = b1 || n > get_max_shown_tabs () in
  c1##disabled <- Js.bool (not b1);
  c2##disabled <- Js.bool (not b2);
  c3##disabled <- Js.bool (not b3)


(** [must_be_shown_tab i] return true if the i'th tab must be shown **)
let must_be_shown_tab i =
  i >= !offset && i < !offset + (get_max_shown_tabs ())

(** [is_shown_tab file] returns true if the [file]'s tab is actually shown 
    or must be shown **)
let is_shown_tab file =
  let _,b = List.fold_left (fun (i, b) id ->
    if file.f_id = id then 
      i+1, must_be_shown_tab i
    else i+1, b) (0, false) (List.rev !id_list) in
  b

(** [get_offset_of_tab file] returns the offset of [file]'s tab **)
let get_offset_of_tab file =
  let _,o = List.fold_left (fun (i, o) id ->
    if file.f_id = id then 
      i+1, i
    else i+1, o) (0, -1) (List.rev !id_list) in
  if o = -1 then failwith "Tabs : get_offset_of_tab failed"
  else o

(** [display_tabs nb] displays the maximum of tabs from [offset] and hide
    the others.
    Also update the list of hidden tabs **)
let display_tabs () =
  display_navigation_buttons ();
  hide_childs (get_element_by_id "tabs_line");
  display_childs (get_element_by_id "hiddentabs_list");
  List.iteri (fun i id ->
    if must_be_shown_tab i then begin
      let t = get_element_by_id (Format.sprintf "tabs_num%d" id) in
      let l = get_element_by_id (Format.sprintf "hiddentabs_num%d" id) in
      t##style##display <- Js.string "";
      l##style##display <- Js.string "none" end
    else ()) (List.rev !id_list)


  

let add_tab file content =
  (* Tab creation *)
  id_list := file.f_id :: !id_list;
  let container = get_element_by_id "tabs_line" in
  let tab = createTd document in
  let title = createInput ~_type:(Js.string "text") document in
  let close = createSpan document in
  tab##id <- Js.string (Format.sprintf "tabs_num%d" file.f_id);
  tab##className <- Js.string "tabs";
  title##value <- Js.string file.f_name;
  title##className <- Js.string "tabs_title";
  close##innerHTML <- Js.string "x";
  close##className <- Js.string "tabs_close";
  let handler_rename () = 
    title##readOnly <- Js._true;
    let newname = Js.to_string title##value in
    if String.length newname = 0 then
      title##value <- Js.string file.f_name
    else Eventmanager.rename_file#trigger (file, newname)  in
  title##onclick <- handler (fun _ ->
    Eventmanager.switch_file#trigger file; Js._true);
  title##ondblclick <- handler (fun _ ->
    title##readOnly <- Js._false; Js._true);
  ignore (addEventListener title (Event.make "blur")
            (handler (fun _ -> handler_rename (); Js._true)) Js._true);
  title##onkeypress <- handler (fun kev ->
    if kev##keyCode == 13 then handler_rename (); Js._true);
  close##onclick <- handler ( fun _ ->
    let after _ =
      match Filemanager.get_prev_opened_file () with
      | None -> ()
      | Some f -> Eventmanager.switch_file#trigger f in
    Eventmanager.close_file#trigger ~after file;
    Js._true);
  Dom.appendChild tab title;
  Dom.appendChild tab close;
  Dom.appendChild container tab;

  (* Item for hidden tab creation *)
  let container = get_element_by_id "hiddentabs_list" in
  let itab = createLi document in
  itab##id <- Js.string (Format.sprintf "hiddentabs_num%d" file.f_id);
  itab##innerHTML <- Js.string file.f_name;
  itab##onclick <- handler (fun _ ->
    Eventmanager.switch_file#trigger file; Js._true);
  Dom.appendChild container itab;

  (* Refresh display *)
  let max = get_max_shown_tabs () in
  if Filemanager.count_opened_files () - !offset > max then
    incr offset;
  display_tabs ()


(** [create_button text name f] creates a button that suits for the tab
    widget with [text] as content, [name] as a part of its Id, and [f] as
    the handler when you click on this button **)
let create_button text name f =
  let baseid = "tabs_"^name in
  let c = createSpan document in
  let b = createInput ~_type:(Js.string "button") document in
  c##id <- Js.string baseid;
  b##id <- Js.string (baseid^"_button");
  c##className <- Js.string "tabs_widget";
  b##value <- Js.string text;
  b##onclick <- handler (fun _ -> f (); Js._true);
  Dom.appendChild c b;
  c

(** [init_tabs container] embeds the tab widget's main content into 
    [container] **)
let init_tabs container =
  let table = createTable document in
  let line = createTr document in
  let scleft = create_button "<" "scleft" (fun () -> 
    offset := max 0 (!offset-1);
    display_tabs ()) in
  let scright = create_button ">" "scright" (fun () ->
    let nbmax = Filemanager.count_opened_files () in
    offset := min (nbmax-1) (!offset+1);
    display_tabs ()) in
  let showall = create_button "..." "showall" (fun () ->
    let container = get_element_by_id "hiddentabs" in
    let s = if !is_list_shown then "none" else "" in
    container##style##display <- Js.string s;
    let left = Format.sprintf "%dpx"
      (scright##clientLeft + scright##offsetLeft - container##clientWidth) in
    let top = Format.sprintf "%dpx"
      (scright##clientTop + scright##offsetTop + scright##clientHeight) in
    container##style##left <- Js.string left;
    container##style##top <- Js.string top;
    is_list_shown := not !is_list_shown) in
  line##id <- Js.string "tabs_line";
  table##id <- Js.string "tabs_table";
  table##className <- Js.string "tabs_widget";
  Dom.appendChild table line;
  Dom.appendChild container scleft;
  Dom.appendChild container table;
  Dom.appendChild container scright;
  Dom.appendChild container showall


(** [init_hidden_tab_list container] embeds the widget for showing the hidden
    tabs into [container] **)
let init_hidden_tab_list container =
  let ul = createUl document in
  ul##id <- Js.string "hiddentabs_list";
  container##style##display <- Js.string "none";
  container##style##position <- Js.string "absolute";
  container##style##zIndex <- Js.string "4";
  Dom.appendChild container ul



(** [event_change_current_tab ()] notifies that the current tab has been
    changed to the Eventmanager
    *Warning* : This function can be called since Javascript **)
let event_change_current_tab () =
  match Filemanager.get_current_file () with
  | None -> ()
  | Some f -> Eventmanager.unsave_file#trigger f


(** [event_save_current_tab ()] notifies that the current tab has been
    saved to the Eventmanager
    *Warning* : This function can be called since Javascript **)
let event_save_current_tab () =
  match Filemanager.get_current_file () with
  | None -> ()
  | Some f -> Eventmanager.save_file#trigger f




(** Functions below are the ones of the eventlistener for the tab
    widget **)


let init tabs =
  let hiddentabs = createDiv document in
  hiddentabs##id <- Js.string "hiddentabs";
  init_tabs tabs;
  init_hidden_tab_list hiddentabs;
  Dom.appendChild tabs hiddentabs;
  Dom_html.window##onresize <- Dom_html.handler (fun _ -> 
    display_tabs ();
    Js._true);
  (Js.Unsafe.coerce Dom_html.window)##saveCurrentTab <- Js.wrap_callback
    event_save_current_tab;
  (Js.Unsafe.coerce Dom_html.window)##currentTabChanged <- Js.wrap_callback
    event_change_current_tab

let open_workspace _ =
  display_tabs ()

let close_workspace () =
  is_list_shown := false;
  offset := 0;
  id_list := [];
  let t = get_element_by_id "tabs_line" in
  let ht = get_element_by_id "hiddentabs_list" in
  remove_childs ht;
  remove_childs t

let open_file (file, content) =
  add_tab file content

let close_file f =
  let id = f.f_id in
  id_list := List.filter (fun i -> i <> id) !id_list;
  remove_node (get_element_by_id (Format.sprintf "tabs_num%d" id));
  remove_node (get_element_by_id (Format.sprintf "hiddentabs_num%d" id));
  display_tabs ()

let rename_file f =
  if f.f_is_open then begin
    let id, name = f.f_id, Js.string f.f_name in
    let t = query_selector 
      (get_element_by_id (Format.sprintf "tabs_num%d" id))
      ".tabs_title" in
    let ht = get_element_by_id (Format.sprintf "hiddentabs_num%d" id) in
    (coerceTo_input t)##value <- name;
    ht##innerHTML <- name end

let create_file file =
  add_tab file ""

let delete_file f =
  if f.f_is_open then close_file f

let delete_project p =
  List.iter close_file p.p_files

let delete_directory d =
  let rec aux = function
    | Directory d -> List.iter aux d.dir_dirs
    | Project p -> List.iter close_file p.p_files in
  aux (Directory d)

let save_and_unsaved_file file =
  update_classes_for_file file

let switch_file (old_file, file) =
  let ht = get_element_by_id "hiddentabs" in
  ht##style##display <- Js.string "none";
  is_list_shown := false;
  if not (is_shown_tab file) then
    offset := get_offset_of_tab file;
  display_tabs ();
  update_classes_for_file file;
  match old_file with
  | None -> ()
  | Some old_file -> update_classes_for_file old_file



let t = { empty_eventlistener with
  init;
  open_workspace;
  close_workspace;
  open_file;
  close_file;
  rename_file;
  create_file;
  delete_file;
  delete_project;
  delete_directory;
  save_file = save_and_unsaved_file;
  unsaved_file = save_and_unsaved_file;
  switch_file }
