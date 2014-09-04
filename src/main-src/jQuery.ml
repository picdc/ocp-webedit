open JavaScript

let alloc_args nb =
  Array.make nb (Ops.constant "undefined"), ref []

let set_arg args idx arg =
  (fst args).(idx) <- arg

let build_args args =
  Array.concat [
    fst args ;
    Array.of_list (List.rev !(snd args)) ;
  ]

type t = any
let inject_t = Inject.identity
let extract_t = Extract.identity

let dollar s = Js.Unsafe.fun_call
  (Js.Unsafe.variable "jQuery") [|Js.Unsafe.inject (Js.string s)|]

let on obj  event_type handler_event_object_  =
  let args = alloc_args 2 in
  set_arg args 0  (Inject.string event_type) ;
  set_arg args 1  (Ops.wrap_fun (fun arg_0 ->
    ignore (handler_event_object_ ())
  )) ;
  let _res = Ops.call_method obj "on" (build_args args) in
  ()

let modal obj arg =
  let args = alloc_args 1 in
  set_arg args 0  (Inject.string arg) ;
  let _res = Ops.call_method obj "modal" (build_args args) in
  ()

