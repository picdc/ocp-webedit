
open Errors_format

(** [make_error_report p e] creates the event onclick to highlight the error [e]
    in the project [p] *)
let make_error_report project error = Dom_html.handler (fun _ ->
  Eventmanager.goto_next_error#trigger (project, error);
  Js._true)

(** [add_errors_reports p e] creates all the link to go to each error in [e]
    from project [e] (à-la-emacs) *)
let add_errors_reports project errors =
  ignore
    (List.fold_left
       (fun i e ->
         let a = Myutils.get_element_by_id ("error" ^ (string_of_int i)) in
         a##onclick <- make_error_report project e;
         i+1)
       0
       errors)
