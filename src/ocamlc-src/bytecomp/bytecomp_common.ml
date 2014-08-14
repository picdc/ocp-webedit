let reset () =
  Simplif.reset_simplif ();
  Bytegen.reset_bytegen ();
  Bytepackager.reset_bytepackager ();
  Symtable.reset_symtable ();
  (* Translcore.reset_translcore (); *)
  Bytelink.reset_bytelink ()(* ; *)
  (* Bytelibrarian.reset_bytelibrarian (); *)
  (* Emitcode.reset_emitcode (); *)
  (* Translmod.reset_translmod (); *)
  (* Lambda.reset_lambda (); *)
  (* Translobj.reset_translobj (); *)
  (* Dll.reset_dll (); *)
  (* Bytesections.reset_bytesections () *)
