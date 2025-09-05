(*s: features/interactive.ml *)
(*s: copyright header2 *)
(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header2 *)
open Common
open Efuns
module Parameter = Parameter_option

(* M-x *)
  
(*s: function [[Interactive.create_bindings]] *)
let create_bindings_help_buffer () =
  let s = Keymap.all_bindings () in
  let text = Text.create s in
  Ebuffer.create "*bindings*" None text (Keymap.create ())
(*e: function [[Interactive.create_bindings]] *)

(*s: constant [[Interactive.meta_hist]] *)
let meta_hist = ref []
(*e: constant [[Interactive.meta_hist]] *)

(*s: function [[Interactive.buf_interactives]] *)
let compute_interactives () =
  Action.actions |> Hashtbl_.to_list |> List.map (fun (s, _) ->
    s, (Action.execute_action s, None)
  )
(*e: function [[Interactive.buf_interactives]] *)
  
(*s: function [[Interactive.exec_interactive]] *)
let exec_interactive interactives frame name =
  try
    let f, key = List.assoc name interactives in
    (*s: [[Interactive.exec_interactive()]] display if has a keybinding *)
    key |> Option.iter (fun key_list ->
      Message.message frame
        ("you can run "^name^" by typing "^(Keymap.print_key_list key_list))
    );
    (*e: [[Interactive.exec_interactive()]] display if has a keybinding *)
    (* run it ! *)
    f frame
  with Not_found -> 
    Message.message frame ("No interactive command "^name)
(*e: function [[Interactive.exec_interactive]] *)
      
(*s: function [[Interactive.call_interactive]] *)
let call_interactive frame =
  let interactives = compute_interactives () in
  Select.select frame "M-x " meta_hist "" 
    (fun _ -> List.map fst interactives)
    (fun s -> s) 
    (fun s -> exec_interactive interactives frame s)
[@@interactive]
(*e: function [[Interactive.call_interactive]] *)

(* variables *)

(*s: constant [[Complex.variable_hist]] *)
let variable_hist = ref []
(*e: constant [[Complex.variable_hist]] *)
(*s: constant [[Complex.value_hist]] *)
let value_hist = ref []
(*e: constant [[Complex.value_hist]] *)
  
(*s: constant [[Complex.all_vars]] *)
let all_vars = ref None
(*e: constant [[Complex.all_vars]] *)
(*s: function [[Complex.all_variables]] *)
let all_variables frame _ =
  let buf = frame.frm_buffer in
  match !all_vars with
    Some (f,l) when f == frame -> l
  | _ ->
      let list = 
        (Store.list buf.buf_vars) @ 
        (Store.list (Globals.editor()).edt_vars) 
      in
      all_vars := Some (frame, list);
      list
(*e: function [[Complex.all_variables]] *)
  
(*s: function [[Complex.set_local_variable]] *)
let set_local_variable frame = 
  Select.select frame "set_local_variable : " variable_hist
    "" (all_variables frame) (fun s -> s) (fun variable ->
      Select.select_string frame (Printf.sprintf "%s : " variable)
      value_hist "" (fun value ->
          Store.input frame.frm_buffer.buf_vars variable value))
[@@interactive]
(*e: function [[Complex.set_local_variable]] *)
  
(*s: function [[Complex.set_global_variable]] *)
let set_global_variable frame =
  Select.select frame "set_global_variable : " variable_hist
    "" (all_variables frame) (fun s -> s) (fun variable ->
      Select.select_string frame (Printf.sprintf "%s : " variable)
      value_hist "" (fun value ->
          Store.input (Globals.editor()).edt_vars variable value))
[@@interactive]
(*e: function [[Complex.set_global_variable]] *)
  
(*s: function [[Complex.get_variable]] *)
let describe_variable frame = 
  Select.select frame "get_variable : " variable_hist "" 
    (all_variables frame)
    (fun s -> s) 
    (fun variable ->
      Top_window.mini_message frame 
        (Printf.sprintf "%s : %s" variable (
          let buf = frame.frm_buffer in
          try
            Store.print buf.buf_vars variable
          with _ ->
            Store.print (Globals.editor()).edt_vars variable)))
[@@interactive]
(*e: function [[Complex.get_variable]] *)

(* parameters *)
open Options

(*s: constant [[Complex.parameters_hist]] *)
let parameters_hist = ref []
(*e: constant [[Complex.parameters_hist]] *)
  
(*s: function [[Complex.set_parameter]] *)
let set_parameter frame = 
  let parameters = Var.get_global Parameter.parameters_var in
  Select.select frame "set-parameter : " parameters_hist
    "" (Parameter.all_parameters frame) (fun s -> s) (fun variable ->
      Select.select_string frame (Printf.sprintf "%s : " variable)
      value_hist "" (fun value ->
          let (input,_print,param) = List.assoc variable parameters
          in
          param =:= input value))
[@@interactive]
(*e: function [[Complex.set_parameter]] *)
  
(*s: function [[Complex.get_parameter]] *)
let get_parameter frame =
  let parameters = Var.get_global Parameter.parameters_var in  
  Select.select frame "get-parameter : " parameters_hist
    "" (Parameter.all_parameters frame) (fun s -> s) (fun variable ->
      Top_window.mini_message frame 
        (Printf.sprintf "%s : %s" variable (
          let (_input,print,param) = List.assoc variable parameters
          in
          print !!param)))
[@@interactive]
(*e: function [[Complex.get_parameter]] *)

(*e: features/interactive.ml *)
