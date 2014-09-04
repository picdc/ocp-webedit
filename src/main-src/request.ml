
exception Bad_request_url of string (* url *)
exception Request_failed of int * string (* code, message *)

let redir url =
  if Config.redir = "" then url else
    Filename.concat Config.redir url

let pull_request ~success ?(failure=(fun e -> raise e)) ?(meth="POST")
    ~url ?(asyn=true) ~msg () =
  let req = XmlHttpRequest.create () in
  let url = redir url in
  req##_open(Js.string meth, Js.string url, Js.bool asyn);
  req##setRequestHeader(Js.string "Content-Type",
			Js.string "application/x-www-form-urlencoded");
  (* req##setRequestHeader(Js.string "withCredentials", Js.string "true"); *)
  let f () =
    match req##readyState with
    | XmlHttpRequest.DONE ->
      let i = req##status in
      if i = 200 then success (Js.to_string req##responseText)
      else failure (Request_failed (i, Js.to_string req##statusText))
    | _ -> () in
  req##onreadystatechange <- Js.wrap_callback f;
  req##send(Js.some (Js.string msg))


(** [my_encode str] :
    Encodes a string message [str] to send it to the server **)
let my_encode str =
  let buf = Buffer.create 503 in
  String.iter (fun c ->
    let i = Char.code c in
    let add =
      if i < 10 then Format.sprintf "00%d" i
      else if i < 100 then Format.sprintf "0%d" i
      else (string_of_int i) in
    Buffer.add_string buf add) str;
  Buffer.contents buf


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


let get_workspace ~callback =
  let success str =
    let dir : Global.s_dirtree = Marshal.from_string (my_decode str) 0 in
    callback dir in
  pull_request ~success ~url:"/project" ~msg:"" ()


let get_file_content ~callback ~path ~name =
  let success str = callback (my_decode str) in
  let msg = Format.sprintf "path=%s&name=%s" path name in
  pull_request ~success:success  ~url:"/project/load" ~msg ()


let create_project ~callback ~path ~name =
  let msg = Format.sprintf "path=%s&name=%s" path name in
  let success _ = callback () in
  pull_request ~success ~url:"/create" ~msg ()

let create_directory ~callback ~path ~name =
  let msg = Format.sprintf "path=%s&name=%s" path name in
  let success _ = callback () in
  pull_request ~success ~url:"/createdir" ~msg ()

let create_file ~callback ~path ~name =
  let msg = Format.sprintf "path=%s&name=%s" path name in
  let success _ = callback () in
  pull_request ~success ~url:"/project/create" ~msg ()


let save_file ~callback ~path ~name ~content =
  let msg = Format.sprintf "path=%s&name=%s&content=%s"
    path name (Url.urlencode content) in
  let success _ = callback () in
  pull_request ~success ~url:"project/save" ~msg ()

let import_file ~callback ~path ~name ~content =
  let msg = Format.sprintf "path=%s&name=%s&content=%s"
    path name (Url.urlencode content) in
  let success _ = callback () in
  pull_request ~success ~url:"project/import" ~msg ()

let rename_file ~callback ~path ~name ~newname =
  let msg = Format.sprintf "path=%s&name=%s&newname=%s"
    path name newname in
  let success _ = callback () in
  pull_request ~success ~url:"project/rename" ~msg ()


let rename_directory ~callback ~path ~name ~newname =
  let msg = Format.sprintf "path=%s&name=%s&newname=%s" path name newname in
  let success _ = callback () in
  pull_request ~success ~url:"rename" ~msg ()

let delete_directory ~callback ~path ~name =
  let msg = Format.sprintf "path=%s&name=%s" path name in
  let success _ = callback () in
  pull_request ~success ~url:"delete" ~msg ()

let delete_file ~callback ~path ~name =
  let msg = Format.sprintf "path=%s&name=%s" path name in
  let success _ = callback () in
  pull_request ~success  ~url:"project/delete" ~msg ()

let import_project ~callback ~path ~file ~content =
  let msg = Format.sprintf "path=%s&file=%s&content=%s" path file content in
  let success str =
    let p : Global.s_dirtree =  Marshal.from_string (my_decode str) 0 in
    callback p in
  pull_request ~success ~url:"import" ~msg ()

let export_directory ~callback ~path ~name =
  let msg = Format.sprintf "path=%s&name=%s" path name in
  let success _ = callback () in
  pull_request ~success ~url:"export" ~msg ()

let save_conf ~callback ~path ~name ~content =
  let msg = Format.sprintf "path=%s&name=%s&content=%s"
    path name (Url.urlencode content) in
  let success _ = callback () in
  pull_request ~success ~url:"conf/save" ~msg ()


let load_conf ~callback ~failure ~path ~name =
  let msg = Format.sprintf "path=%s&name=%s" path name in
  pull_request ~success:callback ~failure ~url:"conf/load" ~msg ()

let compile_project ~callback ~path ~obj =
  let msg = Format.sprintf "path=%s&obj=%s" path obj in
  let success str =
    let result : Global.compile_result = Marshal.from_string (my_decode str) 0 in
    callback result in
  pull_request ~success ~url:"project/compile" ~msg ()

let load_lib ~callback =
  let success str =
    let result : string list = Marshal.from_string (my_decode str) 0 in
    callback result in
  pull_request ~success ~url:"library" ~msg:"" ()

let install_lib ~callback ~path ~library =
  let msg = Format.sprintf "path=%s&lib=%s" path library in
  let success _ = callback () in
    pull_request ~success ~url:"library/install" ~msg ()