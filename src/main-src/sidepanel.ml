
open Dom_html
open Myutils
open Global
open Filemanager

(** Generic type for all types of workspace's item **)
type gen =
  Gen_R of directory | Gen_D of directory
| Gen_P of project | Gen_F of file

module H = Hashtbl.Make(struct
  type t = gen
  let equal t1 t2 = match t1,t2 with
    | (Gen_R r1, Gen_R r2) -> r1.dir_id = r2.dir_id
    | (Gen_D d1, Gen_D d2) -> d1.dir_id = d2.dir_id
    | (Gen_P p1, Gen_P p2) -> p1.p_id = p2.p_id
    | (Gen_F f1, Gen_F f2) -> f1.f_id = f2.f_id
    | _ -> false
  let hash = function
    | Gen_R r -> r.dir_id * 11
    | Gen_D d -> d.dir_id * 13
    | Gen_P p -> p.p_id * 17
    | Gen_F f -> f.f_id * 19
end)

(** List of item which childrens are shown in the sidepanel **)
let childs_shown = H.create 19

(** The focused [gen] (with the right mouse's clic) *)
let focus = ref None

(** [get_cssclassname_of_gen gen is_focused] gives the name which can be used
    for the className method of the DOM element that contains the name of [gen]
    [is_focused] notify that [focus] is in the arborescence of [gen] **)
let get_cssclassname_of_gen gen is_focused =
  let str = match gen with
    | Gen_R r -> "side_name_root"
    | Gen_D d -> "side_name_dir"
    | Gen_P p -> "side_name_project"
    | Gen_F f -> "side_name_file" ^
      (if f.f_is_unsaved then " side_name_file_unsaved" else "") in
  if is_focused then Js.string ("side_name " ^ str ^ " side_name_focus")
  else Js.string ("side_name " ^ str)

(** [get_id_of_gen gen] gives the id which can be used for the id method of
    [gen]'s main container **)
let get_id_of_gen = function
  | Gen_R r -> Format.sprintf "dir%d" r.dir_id
  | Gen_D d -> Format.sprintf "dir%d" d.dir_id
  | Gen_P p -> Format.sprintf "pro%d" p.p_id
  | Gen_F f -> Format.sprintf "fil%d" f.f_id

(** [get_img_of_gen gen is_opened] gives the image/icon matching with [gen]
    and [is_opened] **)
let get_glyph_of_gen gen is_opened =
  match gen, is_opened with
  | Gen_R r, _ -> "align-justify"
  | Gen_D d, true -> "chevron-down"
  | Gen_D d, false -> "chevron-right"
  | Gen_P p, true -> "folder-open"
  | Gen_P p, false -> "folder-close"
  | Gen_F f, _ ->
    match get_type_of_file f with
    | Module -> "briefcase"
    | Interface -> "eye-open"
    | Library -> "shopping-cart"
    | Lexer -> "tags"
    | Grammar -> "book"

let get_glyph_of_gen gen is_opened =
  Js.string (Printf.sprintf "glyphicon glyphicon-%s glyph-space"
    (get_glyph_of_gen gen is_opened))

(** [get_img_of_gen gen is_opened] gives the image/icon matching with [gen]
    and [is_opened] **)
let get_img_of_gen gen is_opened =
  match gen with
  | Gen_R r -> Js.string "./icons/dir_root_opt.png"
  | Gen_D d when is_opened -> Js.string "./icons/dir_open_opt.png"
  | Gen_D d -> Js.string "./icons/dir_close_opt.png"
  | Gen_P p when is_opened -> Js.string "./icons/project_open_opt.png"
  | Gen_P p -> Js.string "./icons/project_close_opt.png"
  | Gen_F f ->
    match get_type_of_file f with
    | Module -> Js.string "./icons/file_ml_opt.png"
    | Interface -> Js.string "./icons/file_mli_opt.png"
    | Library -> Js.string "./icons/file_cma_opt.png"
    | Lexer -> Js.string "./icons/file_ml_opt.png"
    | Grammar -> Js.string "./icons/file_ml_opt.png"

(** [add_generic container gen rcdialog] add [gen] as a child of [container]
    and link [rcdialog] as [gen]'s right clic dialog **)
let add_generic container gen rcdialog =
  H.add childs_shown gen false;
  let c_item = createLi document in
  let c_glyph = createSpan document in
  let c_icon = createImg document in
  let c_name = createSpan document in
  let c_childs = createUl document in
  let name = match gen with
    | Gen_R d | Gen_D d -> d.dir_name
    | Gen_P p -> p.p_name
    | Gen_F f -> f.f_name in
  c_item##id <- Js.string ("side_"^(get_id_of_gen gen));
  c_name##className <- get_cssclassname_of_gen gen false;
  c_icon##className <- Js.string "side_icon";
  c_glyph##className <- get_glyph_of_gen gen false;
  c_glyph##innerHTML <- Js.string "      ";
  c_childs##className <- Js.string "side_childs";
  c_childs##style##display <- Js.string "none";
  c_icon##src <- get_img_of_gen gen false;
  c_name##innerHTML <- Js.string name;
  c_name##onclick <- handler (fun _ ->
    let shown = not (H.find childs_shown gen) in
    H.replace childs_shown gen shown;
    c_childs##style##display <- Js.string (if shown then "" else "none");
    c_icon##src <- get_img_of_gen gen shown;
    c_glyph##className <- get_glyph_of_gen gen shown;
    match gen with
    | Gen_F f ->
      let after _ = Eventmanager.switch_file#trigger f in
      if f.f_is_open then after ()
      else Eventmanager.open_file#trigger ~after f;
      Js._true
    | _ -> Js._true);
  make_event_oncontextmenu c_name (handler (fun ev ->
    let ev = Js.Opt.get (Dom_html.CoerceTo.mouseEvent ev) (fun () ->
      failwith "Sidepanel: fail on coerceTo mouseEvent") in
    let x, y =
      Js.Optdef.get (ev##pageX) (fun _ -> 0),
      Js.Optdef.get (ev##pageY) (fun _ -> 0) in
    focus := Some gen;
    Dialog.Right_click_dialog.show rcdialog x y;
    Js._false));
(*  Dom.appendChild c_item c_icon; *)
  Dom.appendChild c_item c_glyph;
  Dom.appendChild c_item c_name;
  Dom.appendChild c_item c_childs;
  Dom.appendChild container c_item;
  c_childs

(** [get_focused_file ()] returns the focused file of the sidepanel.
    Fail if the focused item is not a file **)
let get_focused_file () = match !focus with
  | None -> failwith "Sidepanel: No item focused"
  | Some gen -> match gen with
    | Gen_F f -> f
    | _ -> failwith "Sidepanel: Focused item is not a file"

(** [get_focused_project ()]
    Like [get_focused_file] but for a project **)
let get_focused_project () = match !focus with
  | None -> failwith "Sidepanel: No item focused"
  | Some gen -> match gen with
    | Gen_P p -> p
    | _ -> failwith "Sidepanel: Focused item is not a project"

 (** [get_focused_directory ()]
    Like [get_focused_file] but for a directory **)
let get_focused_directory () = match !focus with
  | None -> failwith "Sidepanel: No item focused"
  | Some gen -> match gen with
    | Gen_D d -> d
    | Gen_R r -> r
    | _ -> failwith "Sidepanel: Focused item is not a directory"

(** [reload_cssclassname_of_gen gen focused] updates the className of the
    container of [gen] name notifying if [gen] or one of its child is focused **)
let reload_cssclassname_of_gen gen focused =
  let c = query_selector
    (get_element_by_id ("side_" ^ (get_id_of_gen gen))) ".side_name" in
  c##className <- get_cssclassname_of_gen gen focused


(** [show_parent_of_gen gen] shows [gen]'s parent recursively until the root
    if necessary **)
let rec show_parent_of_gen gen =
  let v gen =
    if not (H.find childs_shown gen) then gen
    else assert false in
  try
    let gen_if_root dir =
      if dir.dir_is_root then Gen_R dir
      else Gen_D dir in
    let par = match gen with
      | Gen_F f -> v (Gen_P (get_project f.f_project))
      | Gen_P p -> v (gen_if_root (get_directory p.p_parent))
      | Gen_D d -> v (gen_if_root (get_directory d.dir_parent))
      | Gen_R r -> assert false in
    show_childs_of_gen par true
  with _ -> ()

(** [show_childs_of_gen gen b] display or hide [gen]'s childs according to
    [b] **)
and show_childs_of_gen gen b =
  (* if [gen] itself isn't shown, then we'll remedy it *)
  if b then show_parent_of_gen gen;
  (* now let's show [gen] *)
  H.replace childs_shown gen b;
  let c = get_element_by_id ("side_" ^ (get_id_of_gen gen)) in
  let cc = query_selector c ".side_childs" in
(*  let ci = coerceTo_img (query_selector c ".side_icon") in *)
  let cg = query_selector c ".glyphicon" in
  cc##style##display <- Js.string (if b then "" else "none");
(*  ci##src <- get_img_of_gen gen b; *)
  cg##className <- get_glyph_of_gen gen b


(** [intern_handler_new_ml_or_mli ext] is a generic function handler for
    the creation of a ml or mli (according to [ext]) **)
let intern_handler_new_ml_or_mli ext =
  let f = get_focused_file () in
  let p = get_project (f.f_project) in
  let n = Filename.chop_extension f.f_name in
  let en = n ^ ext in
  try
    let ef = List.find (fun f -> f.f_name = en) p.p_files in
    let after _ = Eventmanager.switch_file#trigger ef in
    if ef.f_is_open then after ()
    else Eventmanager.open_file#trigger ~after ef
  with Not_found ->
      Eventmanager.create_file#trigger (p, en)


(** Functions below are handlers for right clic dialog **)

let handler_new_module () =
  try intern_handler_new_ml_or_mli ".ml"
  with _ ->
    Dialog.prompt_new_module ~project:(Some (get_focused_project ())) ()

let handler_new_interface () =
  try intern_handler_new_ml_or_mli ".mli"
  with _ ->
    Dialog.prompt_new_interface ~project:(Some (get_focused_project ())) ()

let handler_new_lexer () =
  let project = Filemanager.get_current_project () in
    Dialog.prompt_new_lexer ~project ()

let handler_new_grammar () =
  let project = Filemanager.get_current_project () in
    Dialog.prompt_new_grammar ~project ()

let handler_save_file () =
  Eventmanager.save_file#trigger (get_focused_file ())

let handler_delete_file () =
  let f = get_focused_file () in
  let callback () =
    let after _ =
      match get_prev_opened_file () with
      | None -> ()
      | Some f -> Eventmanager.switch_file#trigger f in
    Eventmanager.delete_file#trigger ~after f in
  Dialog.prompt_delete_file f callback

let handler_rename_file () =
  let file = get_focused_file () in
  let default = Filename.chop_extension file.f_name in
  Dialog.prompt_rename_file file ~default ()

let handler_import_project () =
  Dialog.prompt_import_project ~dir:(Some (get_focused_directory ())) ()

let handler_import_file () =
  Dialog.prompt_import_file ~project:(Some (get_focused_project ())) ()

let handler_import_library () =
  let project = Some (get_focused_project ()) in
    Dialog.prompt_import_library ~project ()

let handler_link_after () =
  Eventmanager.link_after#trigger (get_focused_file ())

let handler_link_before () =
  Eventmanager.link_before#trigger (get_focused_file ())

let handler_new_project () =
  Dialog.prompt_create_project ~dir:(Some (get_focused_directory ())) ()

let handler_rename_project () =
  Dialog.prompt_rename_project ~project:(Some (get_focused_project ())) ()

let handler_delete_project () =
  let p = get_focused_project () in
  let callback () = Eventmanager.delete_project#trigger p in
  Dialog.prompt_delete_project p callback

let handler_compile_project () =
  Eventmanager.compile#trigger (get_focused_project ())

let handler_compileopts_project () =
  Dialog.prompt_compile_settings ~project:(Some (get_focused_project ())) ()

let handler_export_project () =
  Eventmanager.export_directory#trigger (Project (get_focused_project ()))

let handler_new_directory () =
  Dialog.prompt_create_directory ~dir:(Some (get_focused_directory ())) ()

let handler_rename_directory () =
  Dialog.prompt_rename_directory ~dir:(Some (get_focused_directory ())) ()

let handler_export_directory () =
  Eventmanager.export_directory#trigger (Directory (get_focused_directory ()))

let handler_delete_directory () =
  let d = get_focused_directory () in
  let callback () = Eventmanager.delete_directory#trigger d in
  Dialog.prompt_delete_directory d callback

let handler_export_file () =
  let file = get_focused_file () in
  Eventmanager.export_file#trigger file

(** The different Right clic dialogs **)

(** Right clic dialog shown on "ml" files **)
let rcdialog_file_ml =
  Dialog.Right_click_dialog.create "ml"
    [ "save-file", handler_save_file ;
      "generate-interface", handler_new_interface ;
      "delete-file", handler_delete_file ;
      "rename-file", handler_rename_file ;
      "export-file", handler_export_file ;
      "link-before", handler_link_before ;
      "link-after", handler_link_after ]

(** Right clic dialog shown on "mli" files **)
let rcdialog_file_mli =
  Dialog.Right_click_dialog.create "mli"
    [ "save-file", handler_save_file ;
      "generate-module", handler_new_module ;
      "delete-file", handler_delete_file ;
      "rename-file", handler_rename_file ;
      "export-file", handler_export_file ;
      "link-before", handler_link_before ;
      "link-after", handler_link_after ]

let rcdialog_file_cma =
  Dialog.Right_click_dialog.create "cma"
    [ "delete-file", handler_delete_file;
      "link-before", handler_link_before;
      "link-after", handler_link_after ]

let rcdialog_file_mll =
  Dialog.Right_click_dialog.create "mll"
    [ "save-file", handler_save_file ;
      "delete-file", handler_delete_file;
      "rename-file", handler_rename_file ;
      "export-file", handler_export_file ;
      "link-before", handler_link_before;
      "link-after", handler_link_after ]

let rcdialog_file_mly =
  Dialog.Right_click_dialog.create "mly"
    [ "save-file", handler_save_file ;
      "delete-file", handler_delete_file;
      "rename-file", handler_rename_file ;
      "export-file", handler_export_file ;
      "link-before", handler_link_before;
      "link-after", handler_link_after ]

(** Right clic dialog shown on projects **)
let rcdialog_project =
  Dialog.Right_click_dialog.create "project" [
    "create-new-module", handler_new_module ;
    "create-new-interface", handler_new_interface ;
    "create-new-lexer", handler_new_lexer;
    "create-new-grammar", handler_new_grammar;
    "compile-project", handler_compile_project ;
    "change-compile-options", handler_compileopts_project ;
    "rename-project", handler_rename_project ;
    "delete-project", handler_delete_project ;
    "export-project", handler_export_project ;
    "import-file", handler_import_file;
    "import-library", handler_import_library]


(** Right clic dialog shown on directories (except roots) **)
let rcdialog_directory =
  Dialog.Right_click_dialog.create "directory" [
    "create-new-directory", handler_new_directory ;
    "create-new-project", handler_new_project ;
    "import-project", handler_import_project ;
    "rename-directory", handler_rename_directory ;
    "delete-directory", handler_delete_directory ;
    "export-directory", handler_export_directory]


(** Right clic dialog shown on roots **)
let rcdialog_root =
  Dialog.Right_click_dialog.create "root" [

    "create-new-directory", handler_new_directory ;
    "create-new-project", handler_new_project ;
    "import-project", handler_import_project ;
    "export-directory", handler_export_directory ]




let intern_add_project p =
  let c_parent = get_id_of_gen (Gen_D (get_directory p.p_parent)) in
  let parent_node = query_selector
    (get_element_by_id ("side_" ^ c_parent))
    ".side_childs"  in
  let c = add_generic parent_node (Gen_P p) rcdialog_project in
  List.iter (fun f ->
    match get_type_of_file f with
    | Module -> ignore (add_generic c (Gen_F f) rcdialog_file_ml)
    | Interface -> ignore (add_generic c (Gen_F f) rcdialog_file_mli)
    | Library -> ignore (add_generic c (Gen_F f) rcdialog_file_cma)
    | Lexer -> ignore (add_generic c (Gen_F f) rcdialog_file_mll)
    | Grammar -> ignore (add_generic c (Gen_F f) rcdialog_file_mly)
  ) p.p_files




(** Functions below are the ones of the eventlistener for the sidepanel
    widget **)


let redir url =
  if Config.redir = "" then url else
    Filename.concat Config.redir url

let init _ =
  (* Form for export projects *)
  let form = createForm document in
  let p = createInput
    ~_type:(Js.string "text")
    ~name:(Js.string "path")
    document in
  p##id <- Js.string "project-path-form";
  let n = createInput
    ~_type:(Js.string "text")
    ~name:(Js.string "name")
    document in
  n##id <- Js.string "project-name-form";
  let submit = createInput
    ~_type:(Js.string "submit")
    ~name:(Js.string "export-submit")
    document in
  submit##id <- Js.string "export-submit-form";
  form##_method <- Js.string "post";
  form##action <- Js.string (redir "/export");
  form##style##display <- Js.string "none";
  Dom.appendChild form p;
  Dom.appendChild form n;
  Dom.appendChild form submit;
  Dom.appendChild document##body form



let open_workspace root =
  let c = get_element_by_id "sidepanel" in
  let rec aux c_parent = function
    | Directory d ->
      let c = add_generic c_parent (Gen_D d) rcdialog_directory in
      List.iter (fun d -> aux c d) d.dir_dirs
    | Project p ->
      let c = add_generic c_parent (Gen_P p) rcdialog_project in
      List.iter (fun f ->
        match get_type_of_file f with
        | Module -> ignore (add_generic c (Gen_F f) rcdialog_file_ml)
        | Interface -> ignore (add_generic c (Gen_F f) rcdialog_file_mli)
        | Library -> ignore (add_generic c (Gen_F f) rcdialog_file_cma)
        | Lexer -> ignore (add_generic c (Gen_F f) rcdialog_file_mll)
        | Grammar -> ignore (add_generic c (Gen_F f) rcdialog_file_mly)
      ) p.p_files
    | File _ -> assert false

  in
  let gen = Gen_R root in
  let c = add_generic c gen rcdialog_root in
  List.iter (fun dt -> aux c dt) root.dir_dirs;
  show_childs_of_gen gen true

let close_workspace () =
  focus := None;
  H.reset childs_shown;
  let c = get_element_by_id "sidepanel" in
  remove_childs c

let close_file f =
  reload_cssclassname_of_gen (Gen_F f) false

let create_file f =
  let p = Gen_P (get_project (f.f_project)) in
  let c = query_selector
    (get_element_by_id ("side_" ^ (get_id_of_gen p)))
    ".side_childs" in
  begin match get_type_of_file f with
  | Module -> ignore (add_generic c (Gen_F f) rcdialog_file_ml)
  | Interface -> ignore (add_generic c (Gen_F f) rcdialog_file_mli) 
  | Library -> ignore (add_generic c (Gen_F f) rcdialog_file_cma)
  | Lexer -> ignore (add_generic c (Gen_F f) rcdialog_file_mll)
  | Grammar -> ignore (add_generic c (Gen_F f) rcdialog_file_mly)
  end;
  show_childs_of_gen p true

let create_project p =
  let d = Gen_D (get_directory p.p_parent) in
  let p = Gen_P p in
  let c = query_selector
    (get_element_by_id ("side_" ^ (get_id_of_gen d)))
    ".side_childs" in
  ignore (add_generic c p rcdialog_project);
  show_childs_of_gen p true

let create_directory d =
  let par = Gen_D (get_directory d.dir_parent) in
  let d = Gen_D d in
  let c = query_selector
    (get_element_by_id ("side_" ^ (get_id_of_gen par)))
    ".side_childs" in
  ignore (add_generic c d rcdialog_directory);
  show_childs_of_gen d true

let import_project (p: Global.project) =
  intern_add_project p;
  show_childs_of_gen (Gen_P p) true

let delete_file f =
  close_file f; (* update hashtbl, usefull if you re-create
                            a file with same location and name *)
  let c = get_element_by_id ("side_" ^ get_id_of_gen (Gen_F f)) in
  remove_node c

let delete_project p =
  let c = get_element_by_id ("side_" ^ get_id_of_gen (Gen_P p)) in
  remove_node c

let delete_directory d =
  let c = get_element_by_id ("side_" ^ get_id_of_gen (Gen_D d)) in
  remove_node c

let rename_file f =
  let c = query_selector
    (get_element_by_id ("side_" ^ get_id_of_gen (Gen_F f)))
    ".side_name" in
  Firebug.console##debug(get_element_by_id ("side_" ^ get_id_of_gen (Gen_F f)));
  c##innerHTML <- Js.string (f.f_name)

let rename_project p =
  let c = query_selector
    (get_element_by_id ("side_" ^ get_id_of_gen (Gen_P p)))
    ".side_name" in
  c##innerHTML <- Js.string (p.p_name)

let rename_directory d =
  let c = query_selector
    (get_element_by_id ("side_" ^ get_id_of_gen (Gen_D d)))
    ".side_name" in
  c##innerHTML <- Js.string (d.dir_name)


let save_and_unsaved_file f =
  let focused =
    match get_current_file () with
      None -> false
    | Some f2 -> f2.f_id = f.f_id in
  reload_cssclassname_of_gen (Gen_F f) focused

let switch_file (oldf, f) =
  reload_cssclassname_of_gen (Gen_F f) true;
  match oldf with
  | None -> ()
  | Some f -> reload_cssclassname_of_gen (Gen_F f) false

let link_before (_, _, file) =
  let c_file = get_element_by_id ("side_" ^ (get_id_of_gen (Gen_F file))) in
  let c_project = Js.Opt.get (c_file##parentNode) (fun _ -> assert false) in
  let c_prevfile = c_file##previousSibling in
  match Js.Opt.to_option c_prevfile with
  | None -> ()
  | Some _ ->
    Dom.removeChild c_project c_file;
    Dom.insertBefore c_project c_file c_prevfile

let link_after (_, _, file) =
  let c_prevfile = get_element_by_id ("side_"^(get_id_of_gen (Gen_F file))) in
  let c_project = Js.Opt.get (c_prevfile##parentNode)
    (fun _ -> assert false) in
  let c_file = c_prevfile##nextSibling in
  match Js.Opt.to_option c_file with
  | None -> ()
  | Some c_file ->
    Dom.removeChild c_project c_file;
    Dom.insertBefore c_project c_file (Js.some c_prevfile)




let t = { empty_eventlistener with
  init;
  open_workspace;
  close_workspace;
  create_file;
  create_project;
  create_directory;
  rename_file;
  rename_project;
  rename_directory;
  close_file;
  delete_file;
  delete_project;
  delete_directory;
  save_file = save_and_unsaved_file;
  unsaved_file = save_and_unsaved_file;
  switch_file;
  link_after;
  link_before;
  import_project;
  import_file = create_file;
  import_library = create_file }
