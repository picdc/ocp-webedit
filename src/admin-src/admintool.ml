open Sqlite3

let root_directory = "/home/qli/edit.ocamlpro.com/site/4.00.1"
let data_directory = "/home/qli/edit.ocamlpro.com/site/4.00.1/data"
let opam_pkg_op = ["list"; "install"; "remove"; "show"; "search"]

exception USER_NOT_EXIST of string
exception SELECT_USER_FAIL of string * string
exception OPAM_ERROR of string

let (/) lhp rhp = Filename.concat lhp rhp

let email_to_dirname email =
  let pos_at = String.index email '@' in
  let add_pre_at = String.sub email 0 pos_at in
  let add_post_at = String.sub email (pos_at+1)
    (String.length email - pos_at - 1) in
  add_pre_at^"_at_"^add_post_at

let rec delete_dir path =
	if not (Sys.is_directory path) then Sys.remove path
	else begin
		Array.iter (fun file -> delete_dir (path / file)) (Sys.readdir path);
		Unix.rmdir path
	end

let out_string_file path content =
	let out = open_out path in
		output_string out content;
		close_out out

let user_dir_default path =
	let workspace = path / "WorkSpace" in
	let project = workspace / "hello_world" in
		if Sys.file_exists path then delete_dir path
		else begin
			List.iter (fun path -> Unix.mkdir path 0o777) [path; workspace; project];
			out_string_file (path / "edit.settings") "theme=eclipse";
			out_string_file (project / "hello_world.ml")
				"let _ =\n\tprint_endline \"Hello World!\"\n";
			out_string_file (project / ".webuild")
				"files=hello_world.ml\noutput=hello_world.byte\ndepend=";
			out_string_file (project / "build.ocp")
				"begin program \"hello_world\"\n\tfiles = [\"hello_world.ml\"]\n\trequires = []\nend"
		end

let opam_wrapper subcommand =
(* 	let path_prefix = root_directory / "opam" / "4.00.1" / "bin" in
	let path_env = Unix.getenv "PATH" in *)
	let _ = Unix.putenv "OPAMROOT" (root_directory / "opam") in
(* 	let _ = try ignore(Str.search_forward (Str.regexp path_prefix) path_env 0)
					with Not_found -> Unix.putenv "PATH" (path_prefix ^ ":" ^ path_env) in *)
	let command = "opam " ^ subcommand in
		if Sys.command command <> 0 then raise (OPAM_ERROR command)

let update_config package =
	let inc = open_in (root_directory / "webedit.conf") in
	let len = in_channel_length inc in
	let buf = Buffer.create 127 in
		Buffer.add_channel buf inc len;
		close_in inc;
		let contents = Buffer.contents buf in
		let pkg_list = ref (Str.split (Str.regexp "\n") contents) in
			if List.mem package !pkg_list then
				pkg_list := (List.filter (fun pkg -> pkg <> package) !pkg_list)
		 	else
		 		pkg_list := (package :: !pkg_list);
		 	out_string_file (root_directory / "webedit.conf")  (String.concat "\n" !pkg_list)

(*****************************************************************************)

let db_initial () =
	Unix.chdir data_directory;
	let db = db_open "user_data" in
	let sqls = ["DROP TABLE IF EXISTS users";
							"CREATE TABLE users (uid integer PRIMARY KEY,
							                     username varchar(127) NOT NULL UNIQUE,
							                     password varchar(127) NOT NULL,
							                     directory varchar(255) NOT NULL,
							                     name varchar(255))"] in
		List.iter (fun sql -> 
			try
				let return_code = exec db sql in
				match return_code with
					| Rc.OK -> ()
					| r -> prerr_endline (Rc.to_string r);
						   	 prerr_endline (errmsg db)
			with Error s -> prerr_endline s) sqls;
		assert(db_close db)

let user_create email password ?name () =
	let username = email_to_dirname email in
	let psw_sha = Sha1.to_hex (Sha1.string password) in
	let directory = data_directory / username in
	let rows = ref "username, password, directory" in
	let values = ref (Format.sprintf "\'%s\', \'%s\', \'%s\'" username psw_sha directory) in
	let _ = match name with
		| None -> ()
		| Some name -> rows := (!rows ^ ", name");
									 values := (!values ^ (Format.sprintf ", \'%s\'" name)) in
	let _ = Unix.chdir data_directory in
	let db = db_open ~mode:`NO_CREATE "user_data" in
	let sql = Format.sprintf "INSERT INTO users (%s) VALUES (%s)" !rows !values in
	let _ = try let return_code = exec db sql in
	            match return_code with
	            	| Rc.OK -> user_dir_default directory
	            	| r -> prerr_endline (Rc.to_string r);
	            	       prerr_endline (errmsg db)
	        with Error s -> prerr_endline s in
	  assert(db_close db)

let user_delete email =
	let username = email_to_dirname email in
	let _ = Unix.chdir data_directory in
	let db = db_open ~mode:`NO_CREATE "user_data" in
	let sql = Format.sprintf "DELETE FROM users WHERE username = \'%s\'" username in
	let _ = try let return_code = exec db sql in
							match return_code with
								| Rc.OK -> if changes db <> 1 then raise (USER_NOT_EXIST email)
													 else delete_dir (data_directory / username)
								| r -> prerr_endline (Rc.to_string r);
								       prerr_endline (errmsg db)
					with Error s -> prerr_endline s in
		assert (db_close db)

let user_change_psw email psw_old psw_new =
	let username = email_to_dirname email in
	let psw_old_sha = Sha1.to_hex(Sha1.string psw_old) in
	let psw_new_sha = Sha1.to_hex(Sha1.string psw_new) in
	let _ = Unix.chdir data_directory in
	let db = db_open ~mode:`NO_CREATE "user_data" in
	let sql = Format.sprintf "UPDATE users SET password = \'%s\'
	                          WHERE username = \'%s\' and password = \'%s\'"
	          psw_new_sha username psw_old_sha in
	let _ = try let return_code = exec db sql in
	            match return_code with
	             | Rc.OK -> if changes db <> 1 then
	                        	raise (SELECT_USER_FAIL (email, psw_old))
	             | r -> prerr_endline (Rc.to_string r);
	                    prerr_endline (errmsg db)
	        with Error s -> prerr_endline s in
	  assert(db_close db)

let user_identify email password =
	user_change_psw email password password

let user_list () =
	()

(*****************************************************************************)

let opam_initial () =
	let path = root_directory / "opam" in
		assert (not (Sys.file_exists path));
		Unix.mkdir path 0o777;
		Unix.putenv "OPAMROOT" path;
		let commands = ["opam init";
		                Format.sprintf "eval `opam config env --root=%s`" path;
		                "opam switch 4.00.1";
		                Format.sprintf "eval `opam config env --root=%s`" path] in
		  print_endline ("OPAMROOT=" ^ (Unix.getenv "OPAMROOT"));
			print_endline "\n\n*****PLEASE BE PATIENT, THIS MAY TAKE SOME TIME*****\n";
			List.iter (fun command ->
				print_endline "\n\n*************THE COMMAND EXECUTING NEXT*************";
				print_endline (command ^ "\n");
				if Sys.command command <> 0 then begin
					delete_dir path;
					raise (OPAM_ERROR ("Failed: " ^ command))
				end) commands;
			out_string_file (root_directory / "webedit.conf") ""


let pkg_operation op ?package () =
	assert (List.mem op opam_pkg_op);
	match package with
		| None -> assert(op = "list");
							opam_wrapper op
		| Some pkg -> opam_wrapper (op ^ " " ^ pkg);
									if op = "install" || op = "remove" then update_config pkg

let get_pkg_op () = opam_pkg_op