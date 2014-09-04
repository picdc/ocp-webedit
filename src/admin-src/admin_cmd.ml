open Arg
open Admintool

let argv_array = Sys.argv

let make_opam_speclist () =
	let pkg_op = get_pkg_op () in
	let symbol_func op =
		if op = "list" then pkg_operation op ()
		else begin
			assert ((!current +1) < (Array.length argv_array - 1));
			pkg_operation op ~package:argv_array.(!current+2) ();
			incr current; 
		end;
	in Arg.align
	["-init", Unit opam_initial, " Install and initial opam under the root directory of server";
	 "-pkg", Symbol (pkg_op, symbol_func),
	 	Printf.sprintf " Package operation done by opam of server, allowed: %s" (String.concat " " pkg_op)]
(*
  let make_db_speclist () =
	let user_op = get_user_op () in
	let op_name = fst (List.split user_op) in
	let symbol_func = function
		| "create" -> 
		| "delete" ->
		| "change_psw" -> 
	in
	["-init", Unit db_initial, " Contruct the data base for users information";
	 "-user", Symbol (op_name, symbol_func),
	   Printf.sprintf " User data management, allowed: %s" (String.concat " " op_name);
	 "-list",Unit user_list, " List all the users in the data base (username [, name ], directory)"]
 *)

let make_db_speclist () =
	let l = Array.length argv_array in
	let create () =
		assert ((!current + 2) < l);
		if (!current + 3) < l then
			let name = argv_array.(!current + 3) in begin
				user_create argv_array.(!current + 1) argv_array.(!current + 2) ~name ();
				current := !current + 3
			end;
		else begin
			user_create argv_array.(!current + 1) argv_array.(!current + 2) ();
			current := !current + 2
		end;
	in
	let delete () =
		assert ((!current + 1) < l);
		user_delete argv_array.(!current + 1);
		incr current
	in
	let chpsw () =
		assert ((!current + 3) < l);
		user_change_psw argv_array.(!current + 1) argv_array.(!current + 2) argv_array.(!current + 3);
		current := !current + 3
	in Arg.align
		["-init", Unit db_initial, " Construct the data base of user information";
		 "-create", Unit create, " -create EMAIL PASSWORD [NAME] creates a new user in the data base";
		 "-delete", Unit delete, " -delete EMAIL deltes the user whose username is EMAIL";
		 "-chpsw", Unit chpsw, " -chpsw EMAIL OLD_PASSWORD NEW_PASSWORD changes the password for user";
		 "-list", Unit user_list, " -list print out information of all users (username [name] director)"]

let print_my_help () =
	print_endline "";
	print_endline "*****************************************************************";
	print_endline "*   This tool helps you with USER DATA MANAGEMENT, please run   *";
	print_endline "*                  admin_cmd database -help                     *";
	print_endline "* and also helps you with OCAML PACKAGES MANAGEMENT, please run *";
	print_endline "*                    admin_cmd opam -help                       *";
	print_endline "*****************************************************************";
	print_endline ""

let main () =
	incr current;
	try
		let domain = argv_array.(!current) in
		let speclist = if domain = "opam" then make_opam_speclist ()
									 else if domain = "database" then make_db_speclist ()
									 else begin
									   print_endline ("Unknow operation domain: " ^ domain); 
									 	 print_my_help (); 
									 	 exit 2 end; 
		in
		let anon_fun op =
			raise (Bad ("Unknow option " ^ op))
		in
		parse speclist anon_fun ""
	with e -> print_my_help (); raise e

let _ = main ()
