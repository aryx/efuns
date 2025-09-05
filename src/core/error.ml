(*s: core/error.ml *)

(* TODO: need flush after Logs.err ? *)

(*s: function [[Efuns.error]] *)
let error_exn s exn =
  let bt = Printexc.get_backtrace () in
  (* less: this introduces a dependency to pfff, but allows for
   * better reporting. An alternative solution would be to register
   * exn handlers/formatters
   *)
  (match exn with 
(* TODO: to restore, but then depends on semgrep lib_parsing
  | Parsing_error.Lexical_error _ | Parsing_error.Syntax_error _ ->
     let ex = Exception.catch exn in
     (* TODO
     let err = Error_code.exception_to_error "XXX" ex in
     pr2 (Error_code.string_of_error err)
      *)
     UCommon.pr2 (Dumper.dump ex)
*)
  | _ -> ()
  );
  Logs.err (fun m -> m "error: %s (exn = %s). backtrace:\n%s" 
                       s (Printexc.to_string exn) bt)
(*e: function [[Efuns.error]] *)

let error s =
  Logs.err (fun m -> m "error: %s" s)
(*e: core/error.ml *)
