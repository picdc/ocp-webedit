
open Format

exception Invalid_servername of string
exception Invalid_boolean of string
exception Invalid_redir_prefix of string
exception Make_failed
exception Make_install_failed of exn
exception Move_failed of string
exception Conffile_corrupted

let print_exn exn =
  let rec aux exn =
    let str = match exn with
      | Invalid_servername s -> "Invalid server name : "^s
      | Invalid_boolean s -> "Invalid boolean : "^s
      | Invalid_redir_prefix s -> "Invalid redirection prefix : "^s
      | Move_failed s -> "Failed to move "^s
      | Conffile_corrupted -> "Error : Configuration file corrupted"
      | Make_install_failed e -> "Make install failed :\n" ^ aux e
      | _ -> raise exn(* "Error"  *)in
    str in
  prerr_endline (aux exn)

type mode = Unknown | Offline | Online
type server = Nothing | Start | Stop | Restart

let trap = ref false
let all = ref false
let install_only = ref false
let server = ref Nothing
let mode = ref Unknown
let servername = ref "localhost"
let prefport = ref 4444
let installpath = ref
  (Filename.concat (Filename.concat (Sys.getcwd ()) "site") Sys.ocaml_version)
let redir = ref false
let redir_prefix = ref "api"
let redir_port = ref (!prefport)
let offline_mode = ref false
let run_make = ref true
let run_make_install = ref true
let opt_conf = ref false
let ocpwebuildpath = ref ".ocp-we.conf"


let is_empty s = String.length s = 0

let string_to_servername s =
  if String.contains s ' ' then raise (Invalid_servername s);
  s

let string_to_path s =
  if Filename.is_relative s then Filename.concat (Sys.getcwd ()) s
  else s

let string_to_port s =
  int_of_string s

let string_to_bool s =
  let yl = [ "y"; "yes"; "oui"; "true" ] in
  let nl = [ "n"; "no"; "non"; "false" ] in
  if List.mem (String.lowercase s) yl then true
  else if List.mem (String.lowercase s) nl then false
  else raise (Invalid_boolean s)

let string_to_redir_prefix s =
  if String.contains s ' ' then raise (Invalid_redir_prefix s);
  Filename.basename s

let rec ask_string msg value (str_to_val, str_from_val) =
  try
    print_endline (sprintf "%s (default= %s)" msg (str_from_val !value));
    let r = read_line () in
    if not (is_empty r) then value := str_to_val r
  with exn ->
    print_exn exn;
    ask_string msg value (str_to_val, str_from_val)





let create_src_config_ml () =
  let out = open_out "src/main-src/config.ml" in
  let code =
    if !redir then
      sprintf "let offline_mode = %b@.let redir = \"%s\""
        !offline_mode !redir_prefix
    else sprintf "let offline_mode = %b@.let redir = \"\"" !offline_mode in
  output_string out code;
  close_out out

let create_server_conf () =
  let buf = Buffer.create 5003 in
  let inc = open_in "build/server.conf.init" in
  Buffer.add_channel buf inc (in_channel_length inc);
  close_in inc;
  let r1 = Str.regexp "VAR_SERVER_NAME" in
  let r2 = Str.regexp "VAR_PREF_PORT" in
  let r3 = Str.regexp "VAR_DOCROOT" in
  let s1 = Str.replace_first r1 !servername (Buffer.contents buf) in
  let s2 = Str.replace_first r2 (string_of_int !prefport) s1 in
  let s3 = Str.replace_first r3 (Filename.concat !installpath "www") s2 in
  let out = open_out "server/server.conf" in
  output_string out ("(* *****************   WARNING  ***************** *)\n" ^
                     "(*                                                *)\n" ^
                     "(* This file has been generated by the conf util  *)\n" ^
                     "(*      If you want to do permanent changes,      *)\n" ^
                     "(*    Edit the file in build/server.conf.init     *)\n" ^
                     "(*         and run the conf util again            *)\n" ^
                     "(*                                                *)\n" ^
                     "(* *****************   WARNING  ***************** *)\n\n");
  output_string out s3;
  close_out out


let create_server_main_ml () =
  let buf = Buffer.create 5003 in
  let out = open_out "server/serverConfig.ml" in
  let server_name =
    if !redir_port = 80 then !servername
    else sprintf "%s:%d" !servername !redir_port in
  output_string out ("(* *****************   WARNING  ***************** *)\n" ^
                     "(*                                                *)\n" ^
                     "(* This file has been generated by the conf util  *)\n" ^
                     "(*      If you want to do permanent changes,      *)\n" ^
                     "(*      Edit the file in build/main.ml.init       *)\n" ^
                     "(*         and run the conf util again            *)\n" ^
                     "(*                                                *)\n" ^
                     "(* *****************   WARNING  ***************** *)\n\n");
  output_string out
    (sprintf
       "let offline_mode = %s\nlet server_name = \"%s\"\nlet gpath = \"%s\""
       (string_of_bool !offline_mode) server_name
       (Filename.concat !installpath "data"));
  close_out out


let write_ocpweconf () =
  let out = open_out ".ocp-we.conf" in
  output_string out
    (sprintf "servername=%s@.prefport=%d@.path=%s@.offlinemode=%b@.redir=%b@."
       !servername !prefport !installpath !offline_mode !redir);
  if !redir then
    output_string out (sprintf "redirprefix=%s@.redirport=%d@."
                         !redir_prefix !redir_port);
  close_out out


let rec create_dir_if_not_exists path =
  if Sys.file_exists path then ()
  else begin
    create_dir_if_not_exists (Filename.dirname path);
    Unix.mkdir path 0o777 end

let move from d =
  let from = Filename.concat from d in
  let path = Filename.concat !installpath d in
  if Sys.file_exists path then begin
    let t = Unix.gmtime (Unix.time ()) in
    let f d = if d < 10 then sprintf "0%d" d else string_of_int d in
    let newpath = sprintf "%s_%s%s%s-%s%s%s"
      path (f t.Unix.tm_mday) (f (t.Unix.tm_mon+1)) (f (t.Unix.tm_year-100))
        (f t.Unix.tm_hour) (f t.Unix.tm_min) (f t.Unix.tm_sec) in
    Sys.rename path newpath end
  else create_dir_if_not_exists !installpath;
  let cmd = sprintf "cp -r %s %s" from !installpath in
  Printf.printf "%s\n%!" cmd;
  if (Sys.command cmd) = 0 then ()
  else raise (Move_failed d)


let install () =
  move "./server" "server.asm";
  move "./server" "server.conf";
  move "." "www";
  let data = Filename.concat !installpath "data" in
  if not (Sys.file_exists data) then
    Unix.mkdir (Filename.concat !installpath "data") 0o777



let parse_conf () =
  let inc = open_in !ocpwebuildpath in
  let rec aux () =
    let str = input_line inc in
    let pos = String.index str '=' in
    let var = String.sub str 0 pos in
    let value = String.sub str (pos+1) (String.length str - pos - 1) in
    let parse_bool s =
      if s = "true" then true
      else if s = "false" then false
      else assert false in
    begin match var with
    | "servername" -> servername := value
    | "prefport" -> prefport := (int_of_string value)
    | "path" -> installpath := value
    | "offlinemode" -> offline_mode := parse_bool value
    | "redir" -> redir := parse_bool value
    | "redirprefix" -> redir_prefix := value
    | "redirport" -> redir_port := (int_of_string value)
    | _ -> assert false
    end;
    aux () in
  try aux ()
  with
  | End_of_file -> close_in inc; printf "@.Configuration file loaded !@."
  | _ -> raise Conffile_corrupted



let ask () =
  ask_string
    "Choose the server name, can also be a domain like www.ocaml.org"
    servername (string_to_servername, (fun s -> s));
  ask_string
    "Choose the preferred port for server's services"
    prefport (string_to_port, string_of_int);
  ask_string
    ("Choose where the server will be installed on your system\n" ^
        "Note: folders \"www/\" and \"data/\" will be located there.\n")
    installpath (string_to_path, (fun s -> s));
  ask_string
    ("Would you set up a redirection for services ? (y/n)\n" ^
        "With a redirection, services will be prefixed by a string," ^
        " and default port used in browser can be different of the services'" ^
        "port asked before.\nNote: You must configure your Apache server" ^
        " for doing this redirection.\n")
    redir (string_to_bool, string_of_bool);
  if !redir then begin
    ask_string
      ("Choose the prefix string for the redirection.\n" ^
          "For example, choose \"api\" will change the service \"/login\"" ^
          " into \"/api/login\".\n")
      redir_prefix (string_to_redir_prefix, (fun s -> s));
    ask_string
      ("Choose the default port which be used for the global web service.\n" ^
          "For example, choose \"8080\" will force you to enter in your" ^
          " browser address the port between the domain and the service" ^
          " name (like www.ocaml.org:8080/login)\n")
      redir_port (string_to_port, string_of_int) end
  else redir_port := !prefport;
  ask_string
    ("Would you like to set the offline mode ? (y/n)\n" ^
        "In offline mode, authentifications are disabled." ^
        "To change this mode in the future, you can run again this script" ^
        " or change the variable \"offline_mode\" in" ^
        " \"src/main-src/config.ml\"\n")
    offline_mode (string_to_bool, string_of_bool);
  write_ocpweconf ()


let do_make () =
  let code = Sys.command "make" in
  if code <> 0 then raise Make_failed;
  printf "@.Make successfully done !@."

let do_make_install () =
  try install ()
  with exn -> raise (Make_install_failed exn)

let do_configure () =
  printf "@.@.Configuring project... Please wait a moment@.";
  create_src_config_ml ();
  create_server_conf ();
  create_server_main_ml ();
  printf "@.Project configured successfully !@."

let configure () =
  if not !install_only then
    do_configure ();

  let finalize msg = print_endline msg in

  let good_finalize () =
    let site_address =
      let prefix = "http://" ^ !servername in
      if !redir_port = 80 then prefix
      else prefix ^ ":" ^ (string_of_int !redir_port) in
    finalize ("\nMake install successfully done !\n" ^
                 "\nCongratulation ! All the project has been built\n" ^
                 "You can now run (or restart) the \"server.asm\" located in :" ^
                 "\n" ^ !installpath ^ "\nThe site address is :\n" ^
                 site_address ^ "\n\nYou can start server with \"make start\"")
  in

  if !all then begin
    do_make ();
    do_make_install ();
    good_finalize () end
  else if !install_only then begin
    do_make_install ();
    good_finalize () end
  else begin
    ask_string
      "Would you run \"make\" now ? (y/n)"
      run_make (string_to_bool, string_of_bool);
    if !run_make then begin
      do_make ();
      ask_string
        "Would you run \"make install\" now ? (y/n)"
        run_make_install (string_to_bool, string_of_bool);
      if !run_make_install then begin
        do_make_install ();
        good_finalize () end
      else
        finalize ("\nEnd of configuration, Good Bye :)\n" ^
                     "You can now run \"make install\"") end
    else
      finalize ("\nEnd of configuration, Good Bye :)\n" ^
                   "You can now run \"make\" and then \"make install\"") end

let start_server () =
  parse_conf ();
  let code = Sys.command (sprintf "%s -pid .serverpid"
                 (Filename.concat !installpath "server.asm")) in
  if code = 0 then printf "Server started@."
  else printf "Failed to start the server@."

exception Kill_fail
let stop_server () =
  try
    let inc = open_in ".serverpid" in
    let pid = int_of_string (input_line inc) in
    close_in inc;
    let code = Sys.command (sprintf "kill %d" pid) in
    if code = 0 then printf "Server stopped@."
    else raise Kill_fail
  with
    Kill_fail ->
      let code = Sys.command "killall server.asm" in
      if code = 0 then printf "Server stopped@."
      else printf "Failed to stop the server@."
  | _ -> printf "Stop server failed@."

let restart_server () =
  stop_server ();
  start_server ()

let switch_mode_offline b =
  parse_conf ();
  offline_mode := b;
  do_configure ();
  write_ocpweconf ();
  stop_server ();
  do_make ();
  do_make_install ();
  let str = if b then "offline" else "online" in
  print_endline ("Mode "^str^" switched !");
  start_server ()


let main () =
  if !trap then begin
    printf "Hey Fabrice !@.Haven't you seen the \"DO NOT USE THIS OPTION\" ?@.So why did you use it !@.@.";
    failwith "Try again without this damned option !" end;

  try
    printf "Welcome in Configuration Utils for ocp-webedit !@.@.";
    if !opt_conf || !install_only then begin
      parse_conf ();
      configure () end
    else begin
      if Sys.file_exists !ocpwebuildpath then begin
        let launch_it = ref false in
        ask_string
          ("\nThe configure file "^(!ocpwebuildpath)^" already exists." ^
              "\nDo you want to use it for this configuration ? (y/n)")
          launch_it
          (string_to_bool, string_of_bool);
        if !launch_it then begin
          parse_conf ();
          configure () end
        else begin
          ask ();
          configure () end end
      else begin
        ask ();
        configure () end end
  with exn ->
    print_exn exn;
    print_endline "\nConfiguration stopped"




open Arg

let spec_list = align ([
  "-conf" , String (fun s -> opt_conf := true; ocpwebuildpath := s),
     "<file> Use the file for the configuration" ;
  "-all" , Set all, " Do \"make\" and \"make install\" without asking" ;
  "-start", Unit (fun () -> server := Start),
     " Start the server (use with the command \"server\")" ;
  "-stop", Unit (fun () -> server := Stop),
     " Stop the server (use with the command \"server\")" ;
  "-restart", Unit (fun () -> server := Restart),
     " Restart the server (use with the command \"server\")" ;
  "-offline", Unit (fun () -> mode := Offline),
     " Switch to offline mode (use with the command \"mode\")" ;
  "-online", Unit (fun () -> mode := Online),
     " Switch to online mode (use with the command \"mode\")" ;
  "-d", Set trap, " DO NOT USE THIS OPTION" ])

let exec_name = Filename.basename Sys.executable_name

let usage_msg = "Usage: " ^ exec_name ^ " [options] <command>\n\n" ^
  "Tool for the configuration of an ocp-wededit server\n\n" ^
  "  " ^ exec_name ^ " config   : Configure the server\n" ^
  "  " ^ exec_name ^ " install  : Install the server\n" ^
  "  " ^ exec_name ^ " -<start|stop|restart> server  : start, stop or restart the server\n" ^
  "  " ^ exec_name ^ " -<online|offline> mode : Switch to online/offline mode\n"

let _ =
  let error () = Arg.usage spec_list usage_msg in
  let main str =
    if str = "config" then main ()
    else if str = "install" then (install_only := true; main ())
    else if str = "mode" then
      (match !mode with
      | Unknown -> error ()
      | Online -> switch_mode_offline false
      | Offline -> switch_mode_offline true)
    else if str = "server" then
      (match !server with
      | Nothing -> error ()
      | Start -> start_server ()
      | Stop -> stop_server ()
      | Restart -> restart_server ())
    else error () in
  if Array.length Sys.argv = 1 then error ()
  else parse spec_list main usage_msg