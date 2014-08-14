
open Myutils
open Dom_html
open Global
open Filemanager

(** [print_exn exn] returns the string associated with the exception [exn] **)
let print_exn = function
  | Invalid_argument s -> "Invalid_argument\n"^s
  | exn -> raise exn

(** [intern_create_popup ~priority ~title ~content ~callback ?can_cancel
    ?submit_text ()] creates and displays a popup like element (used by prompt,
    alert, and confirm dialogs) with [title] and [content] as content.
    The highest [priority] determines which popup will be on top of foreground
    if several popups are shown at the same moment.
    [callback] is the action which be called when the user submit.
    The cancel button is added to the popup if [can_cancel] is set to true.
    [cancel] is the optionnal callback to execute when the cancel button
    is pressed, unused when [can_cancel] is set to false.
    You can also choose the text in the submit button with [submit_text] **)
let rec intern_create_popup ~priority ~title ~content
    ~callback ?(can_cancel=true) ?(cancel=(fun () -> ()))
    ?(submit_text="Submit") () =
  let div = createDiv document in
  let container = createDiv document in
  let background = createDiv document in
  let c_title = createDiv document in
  let b_submit = createButton document in
  div##className <- Js.string "dialog_popup";
  div##style##zIndex <- Js.string (string_of_int (priority * 2));
  container##className <- Js.string "dialog_popup_container";
  background##className <- Js.string "dialog_popup_background";
  background##style##zIndex <- Js.string (string_of_int (priority * 2 - 1));
  c_title##className <- Js.string "dialog_popup_title";
  c_title##innerHTML <- Js.string title;
  content##className <- Js.string "dialog_popup_content";
  let hide () =
    Dom.removeChild document##body div;
    Dom.removeChild document##body background in
  let submit () =
    try callback (); hide ()
    with exn -> alert ~title:"Error" ~text:(print_exn exn) () in
  b_submit##innerHTML <- Js.string submit_text;
  b_submit##onclick <- handler (fun _ -> submit (); Js._true);
  b_submit##className <- Js.string "dialog_popup_button_submit";
  Dom.appendChild container c_title;
  Dom.appendChild container content;
  if can_cancel then begin
    b_submit##style##cssFloat <- Js.string "right";
    let b_cancel = createButton document in
    b_cancel##innerHTML <- Js.string "Cancel";
    b_cancel##onclick <- handler (fun _ -> cancel (); hide (); Js._true);
    b_cancel##className <- Js.string "dialog_popup_button_cancel";
    Dom.appendChild container b_cancel;
    Dom.appendChild container b_submit; end
  else begin
    b_submit##style##display <- Js.string "block";
    Dom.appendChild content b_submit end;
  Dom.appendChild div container;
  Dom.appendChild document##body background;
  Dom.appendChild document##body div


and alert ~title ~text ?(callback=(fun () -> ())) () =
  let content = createDiv document in
  let c_text = createPre document in
  c_text##innerHTML <- Js.string text;
  Dom.appendChild content c_text;
  intern_create_popup ~priority:50 ~title ~content ~callback
    ~can_cancel:false ~submit_text:"Continue" ()


let confirm ~title ~text ~callback =
  let content = createDiv document in
  let c_text = createPre document in
  c_text##innerHTML <- Js.string text;
  Dom.appendChild content c_text;
  intern_create_popup ~priority:45 ~title ~content ~callback
    ~submit_text:"Confirm" ()

(** The several types of dialog prompt's item which notify the DOM
    element where the value is stored **)
type dialogInput_type =
  Text of Dom_html.inputElement Js.t
| Select of Dom_html.selectElement Js.t
| InputFile of Dom_html.inputElement Js.t * Dom_html.inputElement Js.t
| SelectDirtree of Dom_html.inputElement Js.t

(** Items for prompt : the item itself and the [dialogInput_type] associated **)
type dialogInput = Dom_html.element Js.t * dialogInput_type

(** [get_input_data i] returns the name and value datas list
    of the [dialogItem] [i] **)
let get_input_data i =
  match snd i with
  | Text t -> [ Js.to_string t##name, Js.to_string t##value ]
  | Select s -> [ Js.to_string s##name, Js.to_string s##value ]
  | InputFile (f1,f2) -> [ Js.to_string f1##name, Js.to_string f1##value ;
                      Js.to_string f2##name, Js.to_string f2##value ]
  | SelectDirtree dt -> [ Js.to_string dt##name, Js.to_string dt##value ]

(** [get_input_element i] returns the DOM element of the [dialogInput] [i] **)
let get_input_element i = fst i


  let read_arrayBuffer_as_string ab =
    let open Typed_array in
    let uint8Array : (arrayBuffer Js.t -> uint8Array Js.t) Js.constr =
      Js.Unsafe.variable "this.Uint8Array" in
    let a = jsnew uint8Array(ab) in
    let l = a##length in
    let buff = Buffer.create 501 in
    for i = 0 to l - 1 do
      let ch = Js.Optdef.get (get a i) (fun _ -> assert false) in
      (* Must be decoded with my_decode absolutely ! *)
      let add =
        if ch < 10 then Format.sprintf "00%d" ch
        else if ch < 100 then Format.sprintf "0%d" ch
        else (string_of_int ch) in
      Buffer.add_string buff add
    done;
    Js.string (Buffer.contents buff)








let dialogInput_text ~name ~label ?(default="") () =
  let _type = Js.string "text" in
  let name = Js.string name in
  let c_label = createDiv document in
  let c_input = createInput ~_type ~name document in
  c_label##className <- Js.string "dialogInput_label";
  c_label##innerHTML <- Js.string label;
  c_input##className <- Js.string "dialogInput_input dialogInput_text";
  c_input##defaultValue <- Js.string default;
  let div = createDiv document in
  div##className <- Js.string "dialogInput_item";
  Dom.appendChild div c_label;
  Dom.appendChild div c_input;
  div, Text c_input


let dialogInput_select ~name ~label ~values ?(default=None)
    ?(onchange=(fun _ -> ())) () =
  let name = Js.string name in
  let default = match default with None -> "" | Some s -> s in
  let c_label = createDiv document in
  let c_input = createSelect ~name document in
  c_label##className <- Js.string "dialogInput_label";
  c_label##innerHTML <- Js.string label;
  c_input##className <- Js.string "dialogInput_input dialogInput_select";
  c_input##onchange <- handler (fun _ ->
    onchange (Js.to_string c_input##value); Js._true);
  List.iter (fun value ->
    let opt = createOption document in
    opt##value <- Js.string value;
    opt##innerHTML <- Js.string value;
    if value = default then
      opt##defaultSelected <- Js._true;
    Dom.appendChild c_input opt) values;
  let div = createDiv document in
  div##className <- Js.string "dialogInput_item";
  Dom.appendChild div c_label;
  Dom.appendChild div c_input;
  div, Select c_input


exception Input_no_imported_file
let dialogInput_import ~name ~label =
  (** Avoids problems when reading binary file *)
  let _type = Js.string "file" in
  let hidden = Js.string "hidden" in
  let name_name = Js.string (name^"_name") in
  let name_content = Js.string (name^"_content") in
  let c_label = createDiv document in
  let c_input = createInput ~_type document in
  let c_inputh_name = createInput ~_type:hidden ~name:name_name document in
  let c_inputh_content = createInput ~_type:hidden ~name:name_content document in
  c_label##className <- Js.string "dialogInput_label";
  c_label##innerHTML <- Js.string label;
  c_input##className <- Js.string "dialogInput_input dialogInput_file";
  c_input##onchange <- handler (fun _ ->
    try
      let fl = Js.Optdef.get (c_input##files)
        (fun _ -> raise Input_no_imported_file) in
      let f = Js.Opt.get (fl##item(0))
        (fun _ -> raise Input_no_imported_file) in
      let reader = jsnew File.fileReader() in
      reader##onload <- Dom.handler (fun _ ->
        let s = Js.Opt.get (File.CoerceTo.arrayBuffer reader##result)
          (fun _ -> raise Input_no_imported_file) in
        c_inputh_name##value <- f##name;
        c_inputh_content##value <- read_arrayBuffer_as_string s;
        Js._false);
      reader##readAsArrayBuffer (( f :> (File.blob Js.t)));
      Js._true
    with Input_no_imported_file -> Js._true);
  let div = createDiv document in
  div##className <- Js.string "dialogInput_item";
  Dom.appendChild div c_label;
  Dom.appendChild div c_input;
  Dom.appendChild div c_inputh_name;
  Dom.appendChild div c_inputh_content;
  div, InputFile (c_inputh_name, c_inputh_content)




(** Generic type to describe a workspace item
    (sgt : s = super ? (I don't remember), g = generalized, t = type) **)
type sgt = SG_Root | SG_Dir | SG_Pro | SG_File

(** [set_workspace_panel container rescontainer allow] builds the
    graphic representation of the workspace, in [container] and
    intends to put the result in [rescontainer].
    [allow] is the [sgt] list of allowed items that the user can select
    in this panel.
    Note: it don't show useless items **)
let set_workspace_panel container rescontainer allow =
  let create_item img text path =
    let item = createLi document in
    let i = createImg document in
    let t = createSpan document in
    i##className <- Js.string "dialog_select_dirtree_panel_item_img";
    t##className <- Js.string "dialog_select_dirtree_panel_item_text";
    t##innerHTML <- Js.string text;
    i##src <- Js.string img;
    begin match path with
    | None -> ()
    | Some path -> t##onclick <- handler (fun _ ->
      rescontainer##innerHTML <- Js.string path; Js._true) end;
    Dom.appendChild item i;
    Dom.appendChild item t;
    item in
  let rec aux c dt =
    match dt with
    | Directory d ->
      let click, img =
        if d.dir_is_root then
          let img = "./icons/dir_root_opt.png" in
          if List.mem SG_Root allow then
            Some (Filemanager.get_path (Directory d)), img
          else None, img
        else
          let img = "./icons/dir_open_opt.png" in
          if List.mem SG_Dir allow then
            Some (Filemanager.get_path (Directory d)), img
          else None, img
      in
      let item = create_item img d.dir_name click in
      let ul = createUl document in
      Dom.appendChild item ul;
      Dom.appendChild c item;
      List.iter (fun dt -> aux ul dt) d.dir_dirs
    | Project p when List.mem SG_Pro allow || List.mem SG_File allow ->
      let click =
        if List.mem SG_Pro allow then Some (Filemanager.get_path (Project p))
        else None in
      let item = create_item "./icons/project_open_opt.png" p.p_name click in
      Dom.appendChild c item;
      if List.mem SG_File allow then begin
        let ul = createUl document in
        Dom.appendChild item ul;
        List.iter (fun f ->
          let item = create_item "./icons/file_ml_opt.png" f.f_name
            (Some (Filemanager.get_file_path f)) in
          Dom.appendChild ul item) p.p_files;
      end
    | _ -> ()
  in
  aux container (Directory (Filemanager.get_workspace ()))

type workspace_selector = {
  wksel_root : bool;
  wksel_directory : bool;
  wksel_project : bool;
  wksel_file : bool;
}

let set_workspace_dropdown ul wksel callback =
  let tree = Directory (Filemanager.get_workspace ()) in
  let rec map indent tree =
    match tree with
    | Directory d ->

      let li = createLi document in
      let name = Filemanager.get_path (Directory d) in
      begin match d.dir_is_root,
        wksel.wksel_root, wksel.wksel_directory with
      | true, true, _
      | false, _, true ->
        let a = createA document in
        Dom.appendChild li a;
        a##innerHTML <- Js.string (Filename.basename name);
        a##onclick <- handler (fun _ -> callback name);
        li##className <- Js.string (Printf.sprintf "align-left%d" indent);
      | _ ->

        li##innerHTML <- Js.string (Filename.basename name);
        li##className <- Js.string
            (Printf.sprintf "dropdown-header align-left%d" indent)
      end;
      (* TODO:      else desactivate *)
      Dom.appendChild ul  li;
      begin
        match d.dir_dirs with
          [] -> ()
        | dirs ->
          List.iter (fun dir ->
            match dir with
            | Directory _ -> ()
            | _ -> map (indent+1) dir) dirs;
          List.iter (fun dir ->
            match dir with
            | Project _ -> ()
            | _ -> map (indent+1) dir) dirs;
      end

    | Project p ->
      if wksel.wksel_project || wksel.wksel_file then begin

        let li = createLi document in
(*
        let span = createSpan document in
        span##className <- Js.string "badge";
        span##innerHTML <- Js.string (string_of_int (List.length p.p_files));
        Dom.appendChild li span;
*)
        let name = Filemanager.get_path (Project p) in
        if wksel.wksel_project then begin
          let a = createA document in
          Dom.appendChild li a;
          a##innerHTML <- Js.string ("Project " ^ Filename.basename name);
          a##onclick <- handler (fun _ -> callback name);
          li##className <- Js.string  (Printf.sprintf "align-left%d" indent);
        end else begin
          li##innerHTML <- Js.string (Filename.basename name);
          li##className <- Js.string
            (Printf.sprintf "dropdown-header align-left%d" indent)
        end;

        (* TODO:      else desactivate *)
        Dom.appendChild ul  li;
        if wksel.wksel_file then begin
          match p.p_files with
            [] -> ()
          | files ->
            List.iter (fun file -> map (indent+1) (File file)) files
        end

      end

    | File f ->
        let li = createLi document in
        let a = createA document in
        li##className <- Js.string (Printf.sprintf "align-left%d" indent);
        Dom.appendChild li a;
        let name = Filemanager.get_path (File f) in
        a##innerHTML <- Js.string (Filename.basename name);
        a##onclick <- handler (fun _ -> callback name);
        Dom.appendChild ul  li;
  in
  map 0 tree

(** [dialog_select_generic typename typeallowed callback] creates and displays
    the popup in order to let the user to select a workspace item.
    [typename] is the name of the item type, and will be shown in the
    popup's title.
    [typeallowed] is the [sgt] list of allowed item the user can select.
    And the [callback] will be called at the end of this selecting popup. **)
let dialog_select_generic typename typeallowed callback =
  let content = createDiv document in
  let c_selected = createDiv document in
  let c_panel = createUl document in
  c_selected##className <- Js.string "dialog_select_dirtree_selected";
  c_panel##className <- Js.string "dialog_select_dirtree_panel";
  c_selected##innerHTML <- Js.string "";
  set_workspace_panel c_panel c_selected typeallowed;
  Dom.appendChild content c_selected;
  Dom.appendChild content c_panel;
  let title = "Select a "^typename in
  let callback () =
    let value = c_selected##innerHTML in
    if value##length = 0 then
      raise (Invalid_argument ("No "^typename^" selected"));
    callback value in
  intern_create_popup ~priority:30 ~title ~content ~callback ()


(** [dialogInput_dirtree_generic button result name] creates the
    [dialogInput] item for selecting a workspace item, according a
    [button], a [result] container.

    See [dialogInput_project] below to see how to use as example **)
let dialogInput_dirtree_generic button result =
  let div = createDiv document in
  let c_input = createDiv document in
  c_input##className <- Js.string "dialogInput_input dialogInput_dirtree";
  result##className <- Js.string "dialogInput_dirtree_result";
  result##readOnly <- Js._true;
  Dom.appendChild c_input button;
  Dom.appendChild c_input result;
  Dom.appendChild div c_input;
  div

let dialogInput_file ~name ?(default=None) () =
  let name = Js.string name in
  let def_path = match default with
    | None -> ""
    | Some f -> Filemanager.get_file_location f in
  let result = createInput ~_type:(Js.string "text") ~name document in
  let button = createButton document in
  result##value <- Js.string def_path;
  button##innerHTML <- Js.string "Select a file";
  button##onclick <- handler (fun _ ->
    let callback str = result##value <- str in
    dialog_select_generic "file" [SG_File] callback;
    Js._true);
  let div = dialogInput_dirtree_generic button result in
  div, SelectDirtree (result)

let dialogInput_project ~name ?(default=None) () =
  let name = Js.string name in
  let def_path = match default with
    | None -> ""
    | Some p -> Filemanager.get_path (Project p) in
  let result = createInput ~_type:(Js.string "text") ~name document in
  let button = createButton document in
  result##value <- Js.string def_path;
  button##innerHTML <- Js.string "Select a project";
  button##onclick <- handler (fun _ ->
    let callback str = result##value <- str in
    dialog_select_generic "project" [SG_Pro] callback;
    Js._true);
  let div = dialogInput_dirtree_generic button result in
  div, SelectDirtree (result)

let dialogInput_directory ~name ?(with_root=true) ?(default=None) () =
  let name = Js.string name in
  let def_path = match default with
    | None -> ""
    | Some d -> Filemanager.get_path (Directory d) in
  let result = createInput ~_type:(Js.string "text") ~name document in
  let button = createButton document in
  result##value <- Js.string def_path;
  button##innerHTML <- Js.string "Select a directory";
  button##onclick <- handler (fun _ ->
    let callback str = result##value <- str in
    let flags = if with_root then [SG_Root; SG_Dir] else [ SG_Dir ] in
    dialog_select_generic "directory" flags callback;
    Js._true);
  let div = dialogInput_dirtree_generic button result in
  div, SelectDirtree (result)

let dialogInput_dirtree ~name ?(default=None) () =
  let name = Js.string name in
  let def_path = match default with
    | None -> ""
    | Some dt -> Filemanager.get_path dt in
  let result = createInput ~_type:(Js.string "text") ~name document in
  let button = createButton document in
  result##value <- Js.string def_path;
  button##innerHTML <- Js.string "Select a directory";
  button##onclick <- handler (fun _ ->
    let callback str = result##value <- str in
    dialog_select_generic "directory/project" [SG_Root; SG_Dir; SG_Pro] callback;
    Js._true);
  let div = dialogInput_dirtree_generic button result in
  div, SelectDirtree (result)


module Right_click_dialog = struct

  type t = Dom_html.divElement Js.t

  let elements = ref []

  let hide dialog =
    hide_element dialog

  let create id actions =
    let fullid = "rcdialog-" ^ id in
    let dialog = get_element_by_id fullid in
    List.iter (fun (id, action) ->
      let span = get_element_by_id (fullid ^ "-item-" ^ id) in
      span##onclick <- handler (fun _ -> action (); Js._true);
    ) actions;
    elements := dialog::(!elements);
    dialog


  let hide_all () =
    List.iter (fun el -> hide el) !elements


  let show dialog x y =
    hide_all ();
    let l = Format.sprintf "%dpx" x in
    let t = Format.sprintf "%dpx" y in
    display_element dialog;
    dialog##style##left <- Js.string l;
    dialog##style##top <- Js.string t


end


let prompt ~title ~inputs ~callback ?(cancel=(fun () -> ())) () =
  let content = createDiv document in
  List.iter (fun item ->
    Dom.appendChild content (get_input_element item)) inputs;
  let callback () =
    let args = List.fold_left (fun acc item ->
      (get_input_data item)@acc) [] inputs in
    callback args in
  intern_create_popup ~priority:20 ~title ~content ~callback ~cancel ()




(** [parse_input_result res args ?debug ()] parses the prompt dialog result
    [res] with the list of dialogInput's names [args] and returns the
    list of value in the same order of [args].
    (ie if [args] is [ "toto" ; "titi" ] then the result will be
      [ "value_of_toto" ; "value_of_titi" ]).
    [debug] is here to show a customised message when the parse fails **)
let parse_input_result res args ?(debug="unknown") () =
  List.map (fun name ->
    try List.assoc name res
    with Not_found -> failwith
      (Format.sprintf
         "Dialog: parse_input_result failed (arg: %s) (location: %s)"
         name debug)
  ) args


(** [check_no_empty n v] checks if [v] is not a empty string. Otherwise, raises
    Invalid_argument with [n] in the error message **)
let check_no_empty n v =
  if String.length v = 0 then
    raise (Invalid_argument (Format.sprintf "The field '%s' can't be empty" n))

let is_letter c = (c > 64 && c < 91) || (c > 96 && c < 123)
let is_number c = c > 47 && c < 58
let is_underscore c = c = 95



let prompt_select_file callback =
  let callback res =
    let values = parse_input_result res ["path"] ~debug:"select_file" () in
    let path = List.nth values 0 in
    check_no_empty "file" path;
    let file = get_file_from_path path in
    callback file in
  let inputs =
    [ dialogInput_file ~name:"path" () ] in
  prompt ~title:"Select a file" ~inputs ~callback ()

let prompt_select_project callback =
  let callback res =
    let values = parse_input_result res ["path"] ~debug:"select_project" () in
    let path = List.nth values 0 in
    check_no_empty "project" path;
    let project = get_project_from_path path in
    callback project in
  let inputs =
    [ dialogInput_project ~name:"path" () ] in
  prompt ~title:"Select a project" ~inputs ~callback ()

let prompt_select_directory callback =
  let callback res =
    let values = parse_input_result res ["path"] ~debug:"select_directory" () in
    let path = List.nth values 0 in
    check_no_empty "directory" path;
    let dir = get_directory_from_path path in
    callback dir in
  let inputs =
    [ dialogInput_directory ~name:"path" () ] in
  prompt ~title:"Select a directory" ~inputs ~callback ()

(*
let prompt_new_module ?(project=None) () =
  let callback res =
    let values = parse_input_result res ["path";"name"]
      ~debug:"new_module" () in
    let path = List.nth values 0 in
    let name = List.nth values 1 in
    check_no_empty "project" path;
    Filemanager.verify_module_name name;
    let filename = name^".ml" in
    let project = get_project_from_path path in
    let after f = Eventmanager.switch_file#trigger f in
    Eventmanager.create_file#trigger ~after (project, filename) in
  let inputs =
    [ dialogInput_project ~name:"path" ~default:project () ;
      dialogInput_text ~name:"name"
        ~label:"Choose module name (ex: Foobar)" () ] in
  prompt ~title:"Create module" ~inputs ~callback ()
*)

let prompt_new_interface ?(project=None) () =
  let callback res =
    let values = parse_input_result res ["path";"name"]
      ~debug:"new_interface" () in
    let path = List.nth values 0 in
    let name = List.nth values 1 in
    check_no_empty "project" path;
    Filemanager.verify_module_name name;
    let filename = name^".mli" in
    let project = get_project_from_path path in
    let after f = Eventmanager.switch_file#trigger f in
    Eventmanager.create_file#trigger ~after (project, filename) in
  let inputs =
    [ dialogInput_project ~name:"path" ~default:project () ;
      dialogInput_text ~name:"name"
        ~label:"Choose interface name (ex: Foobar)" () ] in
  prompt ~title:"Create interface" ~inputs ~callback ()

(*
let prompt_compile_settings ?(project=None) () =
  let default_output = match project with
    | None -> ""
    | Some p -> p.p_compile_opts.cc_output in
  let callback res =
    let values = parse_input_result res ["path";"output"]
      ~debug:"compile_settings" () in
    let path = List.nth values 0 in
    let cc_output = List.nth values 1 in
    check_no_empty "project" path;
    check_no_empty "output" cc_output;
    let project = get_project_from_path path in
    let prev_conf = project.p_compile_opts in
    let cc_files = prev_conf.cc_files in
    let compile_conf = { cc_files ; cc_output } in
    let conf = Myparser.generate_of_compile_conf compile_conf in
    Eventmanager.save_conf#trigger (Global.Compile(project), conf) in
  let inputs =
    [ dialogInput_project ~name:"path" ~default:project ();
      dialogInput_text ~name:"output"
        ~label:"Output's name (ex: out.byte)"
        ~default:default_output () ] in
  prompt ~title:"Change compiling settings" ~inputs ~callback ()
*)

let prompt_rename_project ?(project=None) () =
  let callback res =
    let values = parse_input_result res ["path";"newname"]
      ~debug:"rename_project" () in
    let path = List.nth values 0 in
    let newname = List.nth values 1 in
    check_no_empty "path" path;
    check_no_empty "new name" newname;
    let project = get_project_from_path path in
    Eventmanager.rename_project#trigger (project, newname) in
  let inputs =
    [ dialogInput_project ~name:"path" ~default:project () ;
      dialogInput_text ~name:"newname"
        ~label:"Choose the new project's name (ex: project_rebirth)" () ] in
  prompt ~title:"Rename project" ~inputs ~callback ()


let prompt_rename_directory ?(dir=None) () =
  let callback res =
    let values = parse_input_result res ["path";"newname"]
      ~debug:"rename_directory" () in
    let path = List.nth values 0 in
    let newname = List.nth values 1 in
    check_no_empty "path" path;
    check_no_empty "new name" newname;
    let dir = get_directory_from_path path in
    Eventmanager.rename_directory#trigger (dir, newname) in
  let inputs =
    [ dialogInput_directory ~name:"path" ~with_root:false ~default:dir () ;
      dialogInput_text ~name:"newname"
        ~label:"Choose the new project's name (ex: project_rebirth)" () ] in
  prompt ~title:"Rename project" ~inputs ~callback ()

module HTML = struct
  let li items =
    let ele = createLi document in
    List.iter (fun item -> Dom.appendChild ele item) items;
    ( ele :> Dom_html.element Js.t)

end

let get_input_by_id id =
  let ele = get_element_by_id id in
  Js.Opt.get (CoerceTo.input ele) (fun _ -> assert false)

(*let get_span_by_id id =
  let ele = get_element_by_id id in
  Js.Opt.get (CoerceTo.span ele) (fun _ -> assert false)
*)

module CreateNewProject = struct
  let modalname = "create-new-project"
  let result = get_input_by_id
      (Printf.sprintf "modal-%s-path-input" modalname)

  let name =
    get_input_by_id
        (Printf.sprintf "modal-%s-name-input" modalname)

  let dropdown =
    get_element_by_id (Printf.sprintf "modal-%s-dropdown" modalname)

  let modal = JQuery.dollar (Printf.sprintf "#modal-%s" modalname)
  let button = get_element_by_id
      (Printf.sprintf "modal-%s-button" modalname)

  let show () = JQuery.modal modal "show"
  let hide () = JQuery.modal modal "hide"

  let () =
    JQuery.on modal "show.bs.modal" (fun () ->
      dropdown##innerHTML <- Js.string "";
      set_workspace_dropdown dropdown
        { wksel_file = false;
          wksel_root = true;
          wksel_directory = true;
          wksel_project = false;
        }
        (fun s ->
          result##value <- Js.string s;
          Js._true
        )
      ;
    );

    button##onclick <- handler (fun _ ->
        let path = Js.to_string result##value in
        let name = Js.to_string name##value in
        check_no_empty "path" path;
        check_no_empty "project's name" name;
        hide ();
        let dir = get_directory_from_path path in
        Eventmanager.create_project#trigger (dir, name);
        Js._false
    )
end

let prompt_create_project ?(dir=None) () =
  CreateNewProject.result##value <- Js.string (
    match dir with
    | None -> ""
    | Some d ->  Filemanager.get_path (Directory d));
  CreateNewProject.show ()


let load_onchange inputfile name content =
  inputfile##onchange <- handler (fun _ ->
    try
      let fl = Js.Optdef.get (inputfile##files)
        (fun _ -> raise Input_no_imported_file) in
      let f = Js.Opt.get (fl##item(0))
        (fun _ -> raise Input_no_imported_file) in
      let reader = jsnew File.fileReader() in
      reader##onload <- Dom.handler (fun _ ->
        let s = Js.Opt.get (File.CoerceTo.arrayBuffer reader##result)
          (fun _ -> raise Input_no_imported_file) in
        name##value <- f##name;
        content##value <- read_arrayBuffer_as_string s;
        Js._false);
      reader##readAsArrayBuffer (( f :> (File.blob Js.t)));
      Js._true
    with Input_no_imported_file -> Js._true)

module ImportNewProject = struct
  let modalname = "import-new-project"
  let result =
    get_input_by_id
        (Printf.sprintf "modal-%s-path-input" modalname)

  let dropdown =
    get_element_by_id (Printf.sprintf "modal-%s-dropdown" modalname)

  let modal = JQuery.dollar (Printf.sprintf "#modal-%s" modalname)
  let button = get_element_by_id
      (Printf.sprintf "modal-%s-button" modalname)

  let inputfile =
    let inputfile = get_element_by_id
      (Printf.sprintf "modal-%s-file-input" modalname) in
    Js.Opt.get (CoerceTo.input inputfile) (fun _ -> assert false)

  let name =
    let name = get_element_by_id
      (Printf.sprintf "modal-%s-file-name" modalname) in
    Js.Opt.get (CoerceTo.input name) (fun _ -> assert false)

  let content =
    let content = get_element_by_id
      (Printf.sprintf "modal-%s-file-content" modalname) in
    Js.Opt.get (CoerceTo.input content) (fun _ -> assert false)

  let show () = JQuery.modal modal "show"
  let hide () = JQuery.modal modal "hide"

  let () =
    load_onchange inputfile name content;
    JQuery.on modal "show.bs.modal" (fun () ->
      dropdown##innerHTML <- Js.string "";
      name##value <- Js.string "";
      content##value <- Js.string "";
      set_workspace_dropdown dropdown
        { wksel_file = false;
          wksel_root = true;
          wksel_directory = true;
          wksel_project = false;
        }
        (fun s ->
          result##value <- Js.string s;
          Js._true
        )
      ;
    );

    button##onclick <- handler (fun _ ->
        let path = Js.to_string result##value in
        let filename = Js.to_string name##value in
        let content = Js.to_string content##value in
        check_no_empty "path" path;
        check_no_empty "filename" filename;
        Filemanager.verify_archive_name filename;
        check_no_empty "content" content;
        hide ();
        let dir = get_directory_from_path path in
        Eventmanager.import_project#trigger (dir, filename, content);
        Js._false
      )
end

let prompt_import_project ?(dir=None) () =
  ImportNewProject.result##value <- Js.string (
    match dir with
    | None -> ""
    | Some d ->  Filemanager.get_path (Directory d));

  ImportNewProject.show ()


module DownloadProject = struct
  let modalname = "download-project"
  let result =
    let result = get_element_by_id
        (Printf.sprintf "modal-%s-path-input" modalname) in
    Js.Opt.get (CoerceTo.input result) (fun _ -> assert false)

  let dropdown =
    get_element_by_id (Printf.sprintf "modal-%s-dropdown" modalname)

  let modal = JQuery.dollar (Printf.sprintf "#modal-%s" modalname)
  let button = get_element_by_id
      (Printf.sprintf "modal-%s-button" modalname)

  let show () = JQuery.modal modal "show"
  let hide () = JQuery.modal modal "hide"

  let () =
    JQuery.on modal "show.bs.modal" (fun () ->
      dropdown##innerHTML <- Js.string "";
      set_workspace_dropdown dropdown
        { wksel_file = false;
          wksel_root = false;
          wksel_directory = false;
          wksel_project = true;
        }
        (fun s ->
          result##value <- Js.string s;
          Js._true
        )
      ;
    );

    button##onclick <- handler (fun _ ->
        let path = Js.to_string result##value in
        check_no_empty "path" path;
        hide ();
        let project = get_project_from_path path in
        Eventmanager.export_directory#trigger (Project project);
        Js._false
    )
end



module CreateNewFile(M: sig
      val modalname : string
      val ext : string
    end) = struct
  let modalname = M.modalname
  let result =
    let result = get_element_by_id
        (Printf.sprintf "modal-%s-path-input" modalname) in
    Js.Opt.get (CoerceTo.input result) (fun _ -> assert false)

  let name =
    let name = get_element_by_id
        (Printf.sprintf "modal-%s-name-input" modalname) in
    Js.Opt.get (CoerceTo.input name) (fun _ -> assert false)

  let dropdown =
    get_element_by_id (Printf.sprintf "modal-%s-dropdown" modalname)

  let modal = JQuery.dollar (Printf.sprintf "#modal-%s" modalname)
  let button = get_element_by_id
      (Printf.sprintf "modal-%s-button" modalname)

  let show () = JQuery.modal modal "show"
  let hide () = JQuery.modal modal "hide"

  let () =
    JQuery.on modal "show.bs.modal" (fun () ->
      dropdown##innerHTML <- Js.string "";
      set_workspace_dropdown dropdown
        { wksel_file = false;
          wksel_root = false;
          wksel_directory = false;
          wksel_project = true;
        }
        (fun s ->
          result##value <- Js.string s;
          Js._true
        )
      ;
    );

    button##onclick <- handler (fun _ ->
        let path = Js.to_string result##value in
        let name = Js.to_string name##value in
        check_no_empty "path" path;
        Filemanager.verify_module_name name;
        let filename = name^ M.ext in
        let project = get_project_from_path path in
        hide ();
        let after f = Eventmanager.switch_file#trigger f in
        Eventmanager.create_file#trigger ~after (project, filename);
        Js._false
    )
end

module CreateNewModule = CreateNewFile(struct
  let modalname = "create-new-module"
  let ext = ".ml"
  end)

module CreateNewInterface = CreateNewFile(struct
    let modalname = "create-new-interface"
    let ext = ".mli"
  end)

module CreateNewLexer = CreateNewFile(struct
    let modalname = "create-new-lexer"
    let ext = ".mll"
  end)

module CreateNewGrammar = CreateNewFile(struct
    let modalname = "create-new-grammar"
    let ext = ".mly"
  end)

let prompt_new_module ?(project=None) () =
  CreateNewModule.result##value <- Js.string (
    match project with
    | None -> ""
    | Some d ->  Filemanager.get_path (Project d));
  CreateNewModule.show ()

let prompt_new_interface ?(project=None) () =
  CreateNewInterface.result##value <- Js.string (
    match project with
    | None -> ""
    | Some d ->  Filemanager.get_path (Project d));
  CreateNewInterface.show ()

let prompt_new_lexer ?(project=None) () =
  CreateNewLexer.result##value <- Js.string (
    match project with
    | None -> ""
    | Some p -> Filemanager.get_path (Project p));
  CreateNewLexer.show ()

let prompt_new_grammar ?(project=None) () =
  CreateNewGrammar.result##value <- Js.string (
    match project with
    | None -> ""
    | Some p -> Filemanager.get_path (Project p));
  CreateNewGrammar.show ()

(*
let prompt_create_project ?(dir=None) () = ()

  let callback res =
    let values = parse_input_result res ["path"; "name"]
      ~debug:"create_project" () in
    let path = List.nth values 0 in
    let name = List.nth values 1 in
    check_no_empty "path" path;
    check_no_empty "project's name" name;
    let dir = get_directory_from_path path in
    Eventmanager.create_project#trigger (dir, name) in
  let inputs =
    [ dialogInput_directory ~name:"path" ~default:dir () ;
      dialogInput_text ~name:"name"
        ~label:"Choose the new project's name (ex: new_project)" () ] in
  prompt ~title:"Create project" ~inputs ~callback ()
*)


module ImportNewFile = struct
  let modalname = "import-new-file"
  let result =
    get_input_by_id
        (Printf.sprintf "modal-%s-path-input" modalname)

  let dropdown =
    get_element_by_id (Printf.sprintf "modal-%s-dropdown" modalname)

  let modal = JQuery.dollar (Printf.sprintf "#modal-%s" modalname)
  let button = get_element_by_id
      (Printf.sprintf "modal-%s-button" modalname)

  let inputfile =
    let inputfile = get_element_by_id
      (Printf.sprintf "modal-%s-file-input" modalname) in
    Js.Opt.get (CoerceTo.input inputfile) (fun _ -> assert false)

  let name =
    let name = get_element_by_id
      (Printf.sprintf "modal-%s-file-name" modalname) in
    Js.Opt.get (CoerceTo.input name) (fun _ -> assert false)

  let content =
    let content = get_element_by_id
      (Printf.sprintf "modal-%s-file-content" modalname) in
    Js.Opt.get (CoerceTo.input content) (fun _ -> assert false)

  let show () = JQuery.modal modal "show"
  let hide () = JQuery.modal modal "hide"

  let () =
    load_onchange inputfile name content;
    JQuery.on modal "show.bs.modal" (fun () ->
      dropdown##innerHTML <- Js.string "";
      name##value <- Js.string "";
      content##value <- Js.string "";
      set_workspace_dropdown dropdown
        { wksel_file = false;
          wksel_root = false;
          wksel_directory = false;
          wksel_project = true;
        }
        (fun s ->
          result##value <- Js.string s;
          Js._true
        )
      ;
    );

    button##onclick <- handler (fun _ ->
        let path = Js.to_string result##value in
        let filename = Js.to_string name##value in
        let content = Js.to_string content##value in
        check_no_empty "path" path;
        check_no_empty "filename" filename;
        Filemanager.verify_file_name filename;
        hide ();
        let project = get_project_from_path path in
        let after f = Eventmanager.switch_file#trigger f in
        Eventmanager.import_file#trigger ~after (project, filename, content);
        Js._false
      )
end

let prompt_import_file ?(project=None) () =
  ImportNewFile.result##value <- Js.string (
    match project with
    | None -> ""
    | Some d ->  Filemanager.get_path (Project d));
  ImportNewFile.show ()

let prompt_import_library ?(project=None) () =
  let values = Filemanager.get_lib_list () in
  let libraries =
    [ dialogInput_select ~name:"library" ~label:"Choose Library" ~values () ] in
  let values = List.map (fun p -> p.p_name) (Filemanager.get_projects ()) in
  let projects = 
    [ dialogInput_select ~name:"project" ~label:"Choose Project" ~values () ] in
  let inputs = match project with
    | None -> projects @ libraries
    | Some _ -> libraries in
  let callback res =
    let library_to_import = List.assoc "library" res in
    let project_to_import = match project with
      | Some p -> p
      | None ->
        let p_name = List.assoc "project" res in
        let p = List.filter (fun p -> p.p_name = p_name) (Filemanager.get_projects ()) in
          List.nth p 0
    in
      Eventmanager.import_library#trigger (project_to_import, library_to_import)
  in
    prompt ~title:"Import Library" ~inputs ~callback ()

let prompt_create_directory ?(dir=None) () =
  let callback res =
    let values = parse_input_result res ["path"; "name"]
      ~debug:"create_directory" () in
    let path = List.nth values 0 in
    let name = List.nth values 1 in
    check_no_empty "path" path;
    check_no_empty "directory's name" name;
    let dir = get_directory_from_path path in
    Eventmanager.create_directory#trigger (dir, name) in
  let inputs =
    [ dialogInput_directory ~name:"path" ~default:dir () ;
      dialogInput_text ~name:"name"
        ~label:"Choose the directory's name (ex: new_dir)" () ] in
  prompt ~title:"Create Directory" ~inputs ~callback ()

(*
let prompt_import_project ?(dir=None) () =
  let callback res =
    let values = parse_input_result res ["path"; "file_name"; "file_content" ]
      ~debug:"import_project" () in
    let path = List.nth values 0 in
    let filename = List.nth values 1 in
    let content = List.nth values 2 in
    check_no_empty "path" path;
    check_no_empty "filename" filename;
    Filemanager.verify_archive_name filename;
    check_no_empty "content" content;
    let dir = get_directory_from_path path in
    Eventmanager.import_project#trigger (dir, filename, content) in
  let inputs =
    [dialogInput_directory ~name:"path" ~default:dir () ;
     dialogInput_import ~name:"file" ~label:"Choose archived project" ] in
  prompt ~title:"Import project" ~inputs ~callback ()
*)

(*
let prompt_import_file ?(project=None) () =
  let callback res =
    let values = parse_input_result res ["path";"file_name";"file_content"]
      ~debug:"import_file" () in
    let path = List.nth values 0 in
    let filename = List.nth values 1 in
    let content = List.nth values 2 in
    check_no_empty "path" path;
    check_no_empty "filename" filename;
    Filemanager.verify_file_name filename;
    let project = get_project_from_path path in
    let after f = Eventmanager.switch_file#trigger f in
    Eventmanager.import_file#trigger ~after (project, filename, content) in
  let inputs =
    [ dialogInput_project ~name:"path" ~default:project () ;
      dialogInput_import ~name:"file" ~label:"Choose file" ] in
  prompt ~title:"Import file" ~inputs ~callback ()
*)



let prompt_rename_file file ?(default="") () =
  let callback res =
    let values = parse_input_result res ["newname"] ~debug:"rename_file" () in
    let newname = List.nth values 0 in
    Filemanager.verify_module_name newname;
    let ext =
      Filemanager.get_extension_of_type (Filemanager.get_type_of_file file)
    in
    let filename = newname^ext in
    let oldname = (Filename.chop_extension file.f_name) in

    let after _ =
      try
        let ext = if ext = ".mli" then ".ml" else ".mli" in
        let text =
          Format.sprintf "Would you like to rename the %s corresponding ?" ext in

        let filename = newname^ext in
        let old = (oldname ^ ext) in
        let file =
          Filemanager.get_file_from_project
            (Filemanager.get_project file.f_project) old in
        (* Renames the corresponding ml/mli *)
        let callback _ =
          Eventmanager.rename_file#trigger (file, filename) in

        confirm ~title:"Rename corresponding module/interface"
          ~text ~callback;
      with _ -> ()
    in

    Eventmanager.rename_file#trigger ~after (file, filename) in
  let inputs =
    [ dialogInput_text ~name:"newname"
        ~label:"Choose the new module/interface name" ~default () ] in
  prompt ~title:"Rename file" ~inputs ~callback ()




let prompt_edit_settings ?(theme="eclipse") () =
  let settings = Filemanager.get_edit_settings () in
  let change_theme str =
    if str <> settings.ec_theme then begin
      let newconf = Myparser.generate_of_edit_conf { ec_theme = str } in
      Eventmanager.save_conf#trigger (Edit, newconf)
    end in
  let cancel () = change_theme theme in
  let callback res =
    let values = parse_input_result res ["theme"] ~debug:"edit_settings" () in
    let theme = List.nth values 0 in
    check_no_empty "theme" theme;
    change_theme theme in
  let values =
    [ "ambiance" ; "chaos" ; "chrome" ; "clouds" ; "clouds_midnight" ;
      "cobalt" ; "crimson_editor" ; "dawn" ; "dreamweaver" ; "eclipse" ;
      "github" ; "idle_fingers" ; "kr" ; "merbivore" ; "merbivore_soft" ;
      "mono_industrial" ; "monokai" ; "pastel_on_dark" ; "solarized_dark" ;
      "solarized_light" ; "terminal" ; "textmate" ; "tomorrow" ;
      "tomorrow_night_blue"; "tomorrow_night_bright"; "tomorrow_night_eighties";
      "tomorrow_night" ; "twilight" ; "vibrant_ink" ; "xcode" ] in
  let inputs =
    [ dialogInput_select ~name:"theme" ~label:"Choose the editor's theme"
        ~values ~default:(Some theme) ~onchange:change_theme () ] in
  prompt ~title:"Edit settings" ~inputs ~callback ~cancel ()






module MakeModal(M: sig val modalname : string end) = struct
  let modalname = M.modalname

  let callback = ref None

  let span =
    get_element_by_id
        (Printf.sprintf "modal-%s-filename" modalname)

  let modal = JQuery.dollar (Printf.sprintf "#modal-%s" modalname)
  let button = get_element_by_id
      (Printf.sprintf "modal-%s-button" modalname)

  let show () = JQuery.modal modal "show"
  let hide () = JQuery.modal modal "hide"

  let () =
    JQuery.on modal "show.bs.modal" (fun () ->
      ()
    );

    button##onclick <- handler (fun _ ->
        hide ();
        begin
          match !callback with
          | None -> ()
          | Some f -> f ()
        end;
        Js._false
    )
end

module DeleteFile = MakeModal(struct
  let modalname = "delete-file"
  end)

let prompt_delete_file f callback =
  let filename = get_path (File f) in
  DeleteFile.span##innerHTML <- Js.string filename;
  DeleteFile.callback := Some callback;
  DeleteFile.show ()

module DeleteProject = MakeModal(struct
  let modalname = "delete-project"
  end)

let prompt_delete_project f callback =
  let filename = get_path (Project f) in
  DeleteProject.span##innerHTML <- Js.string filename;
  DeleteProject.callback := Some callback;
  DeleteProject.show ()

module DeleteDirectory = MakeModal(struct
  let modalname = "delete-directory"
  end)

let prompt_delete_directory f callback =
  let filename = get_path (Directory f) in
  DeleteDirectory.span##innerHTML <- Js.string filename;
  DeleteDirectory.callback := Some callback;
  DeleteDirectory.show ()



module CompileProject = struct
  let modalname = "compile-project"
  let result =
    let result = get_element_by_id
        (Printf.sprintf "modal-%s-path-input" modalname) in
    Js.Opt.get (CoerceTo.input result) (fun _ -> assert false)

  let dropdown =
    get_element_by_id (Printf.sprintf "modal-%s-dropdown" modalname)

  let modal = JQuery.dollar (Printf.sprintf "#modal-%s" modalname)
  let button = get_element_by_id
      (Printf.sprintf "modal-%s-button" modalname)

  let show () = JQuery.modal modal "show"
  let hide () = JQuery.modal modal "hide"

  let () =
    JQuery.on modal "show.bs.modal" (fun () ->
      dropdown##innerHTML <- Js.string "";
      set_workspace_dropdown dropdown
        { wksel_file = false;
          wksel_root = false;
          wksel_directory = false;
          wksel_project = true;
        }
        (fun s ->
          result##value <- Js.string s;
          Js._true
        )
      ;
    );

    button##onclick <- handler (fun _ ->
        let path = Js.to_string result##value in
        check_no_empty "path" path;
        hide ();
        let project = get_project_from_path path in
        Eventmanager.compile#trigger project;
        Js._false
    )
end


let prompt_compile ?(project=None) () =
  CompileProject.result##value <- Js.string (
    match project with
    | None -> ""
    | Some d ->  Filemanager.get_path (Project d));
  CompileProject.show ()







module CompilationSettings = struct
  let modalname = "compilation-settings"
  let result =
    let result = get_element_by_id
        (Printf.sprintf "modal-%s-path-input" modalname) in
    Js.Opt.get (CoerceTo.input result) (fun _ -> assert false)

  let name =
    let name = get_element_by_id
        (Printf.sprintf "modal-%s-name-input" modalname) in
    Js.Opt.get (CoerceTo.input name) (fun _ -> assert false)

  let dropdown =
    get_element_by_id (Printf.sprintf "modal-%s-dropdown" modalname)

  let modal = JQuery.dollar (Printf.sprintf "#modal-%s" modalname)
  let button = get_element_by_id
      (Printf.sprintf "modal-%s-button" modalname)

  let show () = JQuery.modal modal "show"
  let hide () = JQuery.modal modal "hide"

  let () =
    JQuery.on modal "show.bs.modal" (fun () ->
      dropdown##innerHTML <- Js.string "";
      set_workspace_dropdown dropdown
        { wksel_file = false;
          wksel_root = false;
          wksel_directory = false;
          wksel_project = true;
        }
        (fun s ->
          result##value <- Js.string s;
          Js._true
        )
      ;
    );

    button##onclick <- handler (fun _ ->
        let path = Js.to_string result##value in
        let aout = Js.to_string name##value in
        check_no_empty "path" path;
        check_no_empty "aout" path;
        let project = get_project_from_path path in
        JQuery.modal modal "hide";
        let prev_conf = project.p_compile_opts in
        let cc_files = prev_conf.cc_files in
        let compile_conf = { cc_files ; cc_output = aout } in
        let conf = Myparser.generate_of_compile_conf compile_conf in
        Eventmanager.save_conf#trigger (Global.Compile(project), conf);
        Js._false
    )
end

let prompt_compile_settings ?(project=None) () =
  CompilationSettings.result##value <- Js.string (
    match project with
    | None -> ""
    | Some d ->  Filemanager.get_path (Project d));
  CompilationSettings.show ()



module LoginFail = MakeModal(struct
  let modalname = "login-fail"
  end)

let string_to_sha1 str =
  let crypto = Js.Unsafe.variable "CryptoJS" in
  let arg = Js.Unsafe.inject (Js.string str) in
  let hash = Js.Unsafe.meth_call crypto "SHA1" [| arg |] in
    Js.to_string hash

module Login (Callback: sig
                val success: string -> unit
                val failure: exn -> unit
              end) = struct
  let modalname = "login"
  
  let username =
    let username = get_element_by_id
        (Printf.sprintf "modal-%s-email-input" modalname) in
    Js.Opt.get (CoerceTo.input username) (fun _ -> assert false)
  let psw =
    let psw = get_element_by_id
        (Printf.sprintf "modal-%s-password-input" modalname) in
    Js.Opt.get (CoerceTo.input psw) (fun _ -> assert false)

  let modal = JQuery.dollar (Printf.sprintf "#modal-%s" modalname)
  let button = get_element_by_id (Printf.sprintf "modal-%s-button" modalname)

  let show () = JQuery.modal modal "show"
  let hide () = JQuery.modal modal "hide"
  let success = Callback.success
  let failure = Callback.failure

  let () =
    button##onclick <- handler (fun _ ->
      let username = Js.to_string username##value in
      let psw = string_to_sha1 (Js.to_string psw##value) in
      let failmsg = "Please check your login email address and password.\n
                     If you don't have an account, signup first." in
        check_no_empty "email" username;
        check_no_empty "password" psw;
        let success str = hide (); success str in
        let failure exn =
          hide ();
          LoginFail.show ();
          failure exn in
        let msg = Printf.sprintf "email=%s&psw=%s" username psw in
          Request.pull_request ~success ~failure ~url:"login" ~msg ();
          Js._false
    )
end

let prompt_login ~success ?(failure=(fun _ -> ())) () =
  let module Loginmodule = Login (struct
      let success = success
      let failure = failure
    end) in
  Loginmodule.show ()

(*
module Signup (Callback: sig
  val success: string -> unit
  val failure: exn -> unit
end) = struct
  let modalname = "signup"

  let email =
    let email = get_element_by_id (Printf.sprintf "modal-%s-email-input" modalname) in
      Js.Opt.get (CoerceTo.input email) (fun _ -> assert false)
  let psw =
    let psw = get_element_by_id (Printf.sprintf "modal-%s-password-input" modalname) in
      Js.Opt.get (CoerceTo.input psw) (fun _ -> assert false)
  let name =
    let name = get_element_by_id (Printf.sprintf "modal-%s-name-input" modalname) in
      Js.Opt.get (CoerceTo.input name) (fun _ -> assert false)

  let modal = JQuery.dollar (Printf.sprintf "#modal-%s" modalname)
  let button = get_element_by_id (Printf.sprintf "modal-%s-button" modalname)

  let show () = JQuery.modal modal "show"
  let hide () = JQuery.modal modal "hide"
  let success = Callback.success
  let failure = Callback.failure

  let () =
    button##onclick <- handler (fun _ ->
      let email = Js.to_string email##value in
      let psw = string_to_sha1 (Js.to_string psw##value) in
      let name = Js.to_string name##value in
        check_no_empty "email address" email;
        check_no_empty "password" psw;
        check_no_empty "name" name;
        let success str =
          hide ();
          if str = "wformat" then
            alert "Signup Fail" "Please use a valid email address format." ()
          else if str = "exist" then
            alert "Signup Fail" "The email address has been registered." ()
          else success str in
        let failure exn = hide (); failure exn in
        let msg = Printf.sprintf "email=%s&psw=%s&name=%s" email psw name in
          Request.pull_request ~success ~failure ~url:"signup" ~msg ();
          Js._false
    )
end

let prompt_signup ~success ?(failure=(fun e -> raise e)) () =
  let module Signupmodule = Signup (struct
      let success = success
      let failure = failure
    end) in
    Signupmodule.show()
    *)