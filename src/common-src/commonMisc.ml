
let initial_file_content name =
	let file_name = Filename.basename name in
	let ext = Filename.check_suffix file_name in
  if (ext ".mly") then ""
  else
  Printf.sprintf "(* %s %S *)\n"
  	begin if (ext ".ml") then "Module"
  				else if (ext ".mli") then "Interface"
  				else if (ext ".mll") then "Lexer"
  				else "File"
  	end
    (String.capitalize (Filename.chop_extension file_name))
