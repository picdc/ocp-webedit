(**
   Location of an error, as a result from ocamlc
*)
type err_format =
    { file : string;
      line : int;
      chars : (int * int) option }

(**
   All errors returned by ocamlc during compilation
*)
type errors = err_format list
