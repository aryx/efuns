(***********************************************************************)
(*                                                                     *)
(*                           Efuns                                     *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
open Efuns
open Options
open Lisp_lexer
module I = Common_indenter
open Compil (* for fields for ocaml-light *)

(***********************************************************************)
(*********************** colors ***********************)
(***********************************************************************)

let color_region buf start_point end_point =
  let _keyword_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.keyword_color) 1 0 false in
  let string_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.string_color) 1 0 false in
  let comment_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.comment_color) 1 0 false in
  (* atoms *)
  let gray_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.module_color) 1 0 false in

  let text = buf.buf_text in
  let curseur = Text.new_point text in
  let lexbuf = Common_lexer.lexing text start_point end_point in

  let rec iter prev_tok lexbuf =
    let (pos,len), token = Lisp_lexer.token lexbuf in
    Text.set_position text curseur pos;
    (match token with
      | EOF _ -> raise Exit
      | COMMENT ->
          Text.set_attrs text curseur len comment_attr
      | STRING | EOFSTRING ->
          Text.set_attrs text curseur len string_attr
      | IDENT _ when prev_tok = QUOTE ->
          Text.set_attrs text curseur len gray_attr         
      (* todo: could color ident after LPAREN if known keyword *)
      | _ -> ()
    );
    iter token lexbuf
  in
  try
    iter COMMENT lexbuf
  with Exit ->
    buf.buf_modified <- buf.buf_modified + 1;
    Text.remove_point text curseur

(***********************************************************************)
(************************  abbreviations ********************)
(***********************************************************************)

let abbreviations = []

(***********************************************************************)
(**********************  indentations *******************)
(***********************************************************************)

let start_regexp = Str.regexp "^("

let pop_to_kwds = I.pop_to_kwds COMMENT
      
let rec parse lexbuf prev_tok  stack eols  indent indents =
  let _, token = Lisp_lexer.token lexbuf in
  match token with
  | EOL pos -> parse lexbuf prev_tok stack (pos::eols) indent indents
  | EOF pos -> I.add indent  (pos :: eols) indents
  | EOFSTRING -> (0,[0]) :: (I.add indent eols indents)
  | COMMENT -> 
     (* if you want all comments at column 0: *)
     (*  parse lexbuf prev_tok stack [] indent (I.add 0 eols indents) *)
     (* otherwise, indent the comment with the current indentation *)
     (* note that we use prev_tok below, to skip COMMENTs *)
     parse lexbuf prev_tok stack [] indent (I.add indent eols indents)

  | LPAREN          (* LPAREN ... RPAREN *)
  | LBRACKET        (* LBRACKET ... RBRACKET  *)
  | LBRACE          (* LBRACE ... RBRACE *)
    ->
      parse lexbuf token ((token,indent)::stack) [] (indent+2) 
        (I.add indent eols indents)
  
  (* Deterministic Terminators *) 
  (* less: could factorize with a 'List.assoc terminators' like in ocaml_mode*)
  | RPAREN ->
      (* find corresponding block delimiter *)
      let (stack,indent) = I.pop_to LPAREN stack in
      parse lexbuf token stack [] indent (I.add indent eols indents)
  | RBRACE ->
      (* find corresponding block delimiter *)
      let (stack,indent) = I.pop_to LBRACE stack in
      parse lexbuf token stack [] indent (I.add indent eols indents)
  | RBRACKET ->
      (* find corresponding block delimiter *)
      let (stack,indent) = I.pop_to LBRACKET stack in
      parse lexbuf token stack [] indent (I.add indent eols indents)

  (* anything else "flushes" the current eols *)
  | _ ->
      parse lexbuf token stack [] indent (I.add indent eols indents)

(* could factorize this function more between the different major modes,
 * but not worth it; it complexifies things.
 * This modifies start_point, because Common_lexer.lexing,
 * which use Text.lexing, does; but it's ok.
 *)
let get_indentations buf start_point end_point =
  let text = buf.buf_text in
  let lexbuf = Common_lexer.lexing text start_point end_point in
  parse  lexbuf COMMENT  [] []   0 []


(* Now, use the indentation from the parser *)

let indent_between_points = 
  I.indent_between_points get_indentations start_regexp

let indent_current_line =
  I.indent_current_line get_indentations start_regexp color_region

(***********************************************************************)
(**********************  find_error  *******************)
(***********************************************************************)

let error_regexp = Str.regexp 
  "File \"\\(.*\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\):"

(* more precise than Compil.find_error_gen, get column range *)
let find_error text error_point =
  let groups = 
    Text.search_forward_groups text error_regexp 
      error_point 4 in
  let error =
    { Compil.
      err_msg = Text.get_position text error_point;
      err_filename = groups.(0);
      err_line = (int_of_string groups.(1)) - 1;
      err_begin = int_of_string groups.(2);
      err_end = int_of_string groups.(3);
    } in
  Text.fmove text error_point 1;
  error
    
(***********************************************************************)
(*********************  installation ********************)
(***********************************************************************)
(* will activate paren minor mode *)
let hooks = Store.create_abstr "lisp_mode_hook"

let mode = Ebuffer.new_major_mode "Lisp" (Some (fun buf ->
  Color.color_buffer_buf buf; 

  buf.buf_syntax_table.(Char.code '_') <- true;
  buf.buf_syntax_table.(Char.code '-') <- true;
  buf.buf_syntax_table.(Char.code '+') <- true;
  buf.buf_syntax_table.(Char.code '*') <- true;

  let abbrevs = Hashtbl.create 11 in
  (* less: could be a major_var instead? *)
  Var.set_local buf Abbrevs.abbrev_table abbrevs;
  Utils.hash_add_assoc abbrevs abbreviations;

  let hooks = Var.get_var buf hooks in
  Hooks.exec_hooks hooks buf;
))

let lisp_mode =
  Major_modes.enable_major_mode mode
[@@interactive]
  
let _ = 
  Hooks.add_start_hook (fun () ->

    Var.set_major_var mode Compil.find_error find_error;
    Var.set_major_var mode Indent.indent_func indent_between_points;
    Var.set_major_var mode Color.color_func color_region;

    (*  add_major_key mode [c_c; ControlMap,Char.code 'e']
        "lisp-eval-buffer" eval_buffer;
    *)

    Keymap.add_major_key mode [NormalMap,XK.xk_Tab] indent_current_line;
(* pad: TODO
    Keymap.add_major_key mode [NormalMap, XK.xk_Return] insert_and_return;
*)
    Var.add_global Ebuffer.modes_alist [(".*\\.\\(el\\|lisp\\|gwm\\)$", mode)];
  )
