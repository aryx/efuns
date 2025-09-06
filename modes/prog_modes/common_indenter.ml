(***********************************************************************)
(*                                                                     *)
(*                             Efuns                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
open Common
open Eq.Operators
open Efuns

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers functions to build programming language indenters.
 *
 * pad: I factorized code between all the prog_modes/xxx_mode.ml
 *)

(*****************************************************************************)
(* get_indentations (defined by each major mode) helpers *)
(*****************************************************************************)

(* Indent level and list of eols (in reverse order).
 *
 * For example on the file content:
(setq foo
  (+ 1 2
    (+ 3 4)
    5 6
  )
)
 * you will get this 'indentations' (when reversed, see print_indentations()):
indent: 2, eols at [9]
indent: 4, eols at [18, 30]
indent: 2, eols at [38]
indent: 0, eols at [42, 44, 45]
 *
 * Each time you have an indentation level and the set of futur
 * lines that must be at this level (the futur lines are represented
 * by the eol ('\n') preceding the line).
 * Remember that positions are 0-indexed based (see text.mli).
 *)
type indentations = (int * (Text.position list (* eols *))) list

(* Indenters usually use an auxillary function to compute the indentations
 * data structure for a region, for example:
 *    let rec parse lexbuf prev_tok  stack eols  indent indents =
 *      ...
 * This function uses an indentation stack to remember at what indentation
 * level was an "opening" token (e.g., a '(') so that when you
 * encounter a "closing" token you can know which indentation level to restore.
 *
 * Note that 'parse' above maintains a list of eols and current indent.
 * If it encounters a "closing" token then it must adjust those eols
 * to the saved indent, not the current indent. Those eols represent
 * the last eols without indentation information yet. For example for Lisp,
 * when you have a series of newlines, you know their indentation only
 * when you encounter a non-empty line (spaces are discarded by the lexer).
 * If the first token on this line is a closing token, then the indentation
 * will be less than current; if it's another token, then those eols
 * will be "flushed" with the current indentation (see lisp_mode.ml for
 * a simple example of indenter).
 *)

type 'tok indentation_stack = ('tok * int) list

let debug_indent = ref true

(* to be used by programming-language-specific get_indentations() *)
let pop_to_top _stack =
  ([],0)

let rec pop_to kwd stack =
  match stack with
  | [] -> ([],0)
  | (kwd',indent) :: stack when kwd' =*= kwd -> stack, indent
  | _ :: stack -> pop_to kwd stack

let rec pop_to_kwds kwd_end_recursion = fun kwds stack ->
  match stack with
  | [] -> ([], (kwd_end_recursion, 0))
  | (kwd,indent) :: stack when List.memq kwd kwds -> stack, (kwd, indent)
  | _ :: stack -> pop_to_kwds kwd_end_recursion kwds stack

(* agglomerate eols when have already an (indent, _) in indents *)
let add indent eols indents =
  match eols with
  | [] -> indents
  | _ -> 
      match indents with
      | (pindent,peols) :: tail when pindent =|= indent -> 
          (indent, eols @ peols) :: tail
      | _ ->  (indent,eols) :: indents

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

(* alt: use Common.dump, it should do a pretty good work *)
let print_indentations list =
  Logs.debug (fun m -> m "Indentations :"); 
  list |> List.rev |> List.iter (fun (indent, list) ->
    Logs.debug (fun m -> m "indent: %d, eols at [%s]" indent
          (list |> List.rev |> List.map string_of_int |> String.concat ", "))
  )


(*
let _print_stack tokens stack =
  print_string "Indentation stack:"; 
  print_newline ();
  let rec iter stack =
    match stack with
      [] -> ()
    | (token, indent) :: stack ->
        Printf.printf "Token %s indent %d" 
          (List.assoc token tokens) indent;
        print_newline ();
        iter stack
  in
  iter stack
*)

(*
let print_exc e s =
  Printf.printf "Caught exception %s in %s" (Printexc.to_string e) s;
  print_newline ()
*)

(*****************************************************************************)
(* Helpers of helper *)
(*****************************************************************************)

let rec pop_indentations indents =
  match indents with
  | [] -> raise Not_found
  | (indent, eols) :: indents ->
      match eols with
      | [] -> pop_indentations indents
      | eol :: eols -> (indent, eol, (indent,eols) :: indents)

let find_phrase_start start_regexp = fun buf curseur ->
  let text = buf.buf_text in
  try
    let _ = Text.search_backward text start_regexp curseur in ()
  with
    Not_found -> Text.set_position text curseur 0


(*****************************************************************************)
(* Indent between points (for indent_func) helpers using get_indentations *)
(*****************************************************************************)

let indent_between_points get_indentations start_regexp = 
 (* returns an "indent_func" compatible function *)
 fun buf start_point end_point ->
  let text = buf.buf_text in
  text |> Text.with_session (fun _session ->
  Text.with_dup_point text start_point (fun curseur ->
   try
    find_phrase_start start_regexp buf curseur;
    let indentations = get_indentations buf curseur end_point in
    if !debug_indent
    then print_indentations indentations;                
    (* remove the Eof indentation *)
    let _,_,indentations = pop_indentations indentations in
    (* indent other lines *)
    let rec iter indents =
      let (current,pos,indents) = pop_indentations indents in
      (* go just after the eol *)                
      Text.set_position text curseur (pos+1);
      Indent.set_indent text curseur current;
      iter indents
    in
    iter indentations
  with Not_found -> ()
 ))

(*****************************************************************************)
(* Indent current line (Tab) helper *)
(*****************************************************************************)
let indent_current_line get_indentations start_regexp color_region = 
 fun frame ->
  let (buf, text, point) = Frame.buf_text_point frame in

  (* colorize line *)
  Text.with_dup_point text point (fun start_point ->
  Text.with_dup_point text point (fun end_point ->
    Text.bmove text start_point (Text.point_to_bol text start_point);
    Text.fmove text end_point (Text.point_to_eol text end_point);
    color_region buf start_point end_point;
  ));

  (* indentation *)
  Text.with_dup_point text point (fun start_point ->
  Text.with_dup_point text point (fun end_point ->
    find_phrase_start start_regexp buf start_point;
    (* if you are on a line with a "closing" token but before this token
     * you want the correct indentation but for that you need to consider
     * the region past this closing token, hence the move here *)
    Text.fmove text end_point (Text.point_to_eol text point);
    let indentations = get_indentations buf start_point end_point in
    if !debug_indent
    then print_indentations indentations;                
    let (_next,_pos,tail) = pop_indentations indentations in
    let current =
      try
        let (current, _, _ ) = pop_indentations tail in 
        current
      with Not_found  -> 0
    in
    text |> Text.with_session (fun _session ->
      Indent.set_indent text point current
    );
  ))

(*****************************************************************************)
(* Electric return to indent? *)
(*****************************************************************************)

(* Interactive: indent the current line, insert newline and indent next line *)
(*
let insert_and_return frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
(* colors *)
  let start_point = Text.dup_point text point in
  Text.bmove text start_point (Text.point_to_bol text start_point);
  color_region buf start_point point;
  Text.remove_point text start_point;
(* indentations *)
  let curseur = Text.dup_point text point in
  try
    find_phrase_start buf curseur;
    let indentations = compute_indentations buf curseur point in
    Text.remove_point text curseur;
    let (next,pos,tail) = pop_indentations indentations in
    let current =
      try
        let (current, _, _ ) = pop_indentations tail in current
      with
        Not_found  -> 0
    in
    let session = Text.start_session text in
    set_indent text point current;
    Edit.insert_char frame '\n';
    set_indent text point next;
    Text.commit_session text session;
    Text.fmove text point next; 
    ()
  with
    e -> 
      Text.remove_point text curseur;
      Edit.insert_char frame '\n'
*)
