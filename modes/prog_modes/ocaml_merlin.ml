(* Yoann Padioleau
 *
 * Copyright (C) 2018 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
open Efuns
module J = JSON

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Support for Merlin (https://github.com/ocaml/merlin) in Efuns.
 *
 * Merlin is an external program providing many services for OCaml programs:
 *   - intellisense completion,
 *     which is smarter than dabbrev because it is contextual
 *   - jump-to-definition,
 *     which is similar to Tags, but is easier to setup, more incremental,
 *     more precise, and faster
 *   - TODO on-the-fly error-checking,
 *     which is similar to flymake, but again easier to setup
 *   - type under cursor, 
 *     which is similar to what the .annot provided under Emacs, but is
 *     incremental by not requiring to refresh the .annot by typing 'make'
 *     
 * 
 * To use merlin from Efuns simply type 'opam install merlin', which
 * should make available an 'ocamlmerlin' program in your PATH.
 * 
 * Note that Emacs supports Merlin, but I actually had troubles 
 * to set it up for my Emacs 23 ... Switching to Efuns might be simpler :)
 *
 * internals: 
 * Look at merlin/doc/dev/PROTOCOL.md to see the list of commands.
 * You can also experiment with merlin directly from the command line:
 * 
 *   $ ocamlmerlin single errors -filename foo.ml < foo.ml
 * 
 * Note that the '-filename' option is optional but very important for
 * multi-files support. Indeed, it is this option that allows merlin to find
 * and process the .merlin in your project.
 * 
 * Some source of inspiration for the code in this file:
 *  - merlin-acme (https://github.com/raphael-proust/merlin-acme) which is a
 *    plugin written in OCaml
 *  - sublime-text-merlin (https://github.com/cynddl/sublime-text-merlin)
 *    which is written in Python
 * The code of those plugins is small compared to the plugins for Emacs or Vim.
 * 
 * FAQ:
 *  - How can I jump to the definition of entities in OPAM packages
 *    when those packages do not have the .cmt or .cmti files?
 * 
 *    See https://github.com/ocaml/merlin/wiki/Letting-merlin-locate-go-to-stuff-in-.opam and put
 *    $ export OPAMKEEPBUILDDIR=true
 *    $ export OCAMLPARAM="_,bin-annot=1"
 *    in your environment before installing OPAM packages.
 *    Then add this to your .merlin:
 *     B external/FOR_MERLIN/**
 *     S external/FOR_MERLIN/**
 *    where FOR_MERLIN points to your OPAM build directory.
 * 
 * 
 * related:
 * - otags
 * - ocamlspotter
 * - pfff stags and codegraph (lang_ml/ and lang_cmt/) 
 * - https://github.com/freebroccolo/ocaml-language-server but it is written
 *   in Javascript and uses Merlin under the hood
 * - mural from Adam Hupp, to accelerate tags under emacs
 * 
 * todo:
 *  - errors
 *  - case-analysis
 * less:
 *  - occurences
 *  - type-enclosing, type-expression
 *  - outline, shape
 *  - expand-prefix (more error resistant than complete-prefix,
 *    maybe use if complete-prefix returns an empty set)
 *  - enclosing
 *  - extension-list, findlib-list, flags-list
 *  - phrase
 *  - list-modules
 *  - path-of-source
 *  - check-configuration
 *)

(*****************************************************************************)
(* Constants and globals *)
(*****************************************************************************)
(* I assume this program is in your PATH *)
let external_program = "ocamlmerlin"

let debug = ref true

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let str_of_current_position frm =
  let (_, text, point) = Frame.buf_text_point frm in
  spf "'%d:%d'" 
    (Text.point_line text point + 1)
    (Text.point_col text point)

(*****************************************************************************)
(* External program communication *)
(*****************************************************************************)
let execute_command frm command =
  let (buf, text, _) = Frame.buf_text_point frm in
  (* todo? use 'server' instead of 'single' so faster? *)
  let final_command = 
    spf "%s single %s %s" external_program command
      (match buf.buf_filename with
      | None -> ""
      | Some file -> spf "-filename '%s'" file
      )
  in
  let (pipe_read, pipe_write) = Unix.open_process final_command in
  let content = Text.to_string text in
  output_string pipe_write content;
  close_out pipe_write;
  let str = input_line pipe_read in
  let _status = Unix.close_process (pipe_read, pipe_write) in
  if !debug
  then UCommon.pr2 str;
  let j = JSON.json_of_string str in
  (match j with
  | J.Object (
    ("class", J.String "return"):: (* | "failure" | "error" | "exception" *)
    ("value", v)::
    ("notifications", J.Array [])::
    ("timing", _)::
      _rest
  ) -> v, str
  | J.Object (
    ("class", J.String "error"):: 
    ("value", J.String err)::
      _rest
  ) -> failwith (spf "ocamlmerlin: %s" err)

  | _ -> failwith (spf "wrong ocamlmerlin JSON output: %s" str)
  )


(*****************************************************************************)
(* Services *)
(*****************************************************************************)

let show_type_at_cursor frm =
  let command = spf "type-enclosing -position %s -index 0" 
    (str_of_current_position frm) in
  let (j, str) = execute_command frm command in
  match j with
  | J.Array (J.Object [
    (* less: could highlight typed expression *)
    "start", _; "end", _;
    "type", J.String str;
    "tail", _;
    (* less: could process _rest which provides bigger enclosing expressions,
     * in which case further C-c C-t should highlight and type those
     * bigger expressions
     *)
    ]::_rest) -> 
    (* todo: sometimes the str contains newlines *)
    (* less: could use ocaml_mode to colorize those types *)
      Message.message frm str
  | _ -> failwith (spf "wrong JSON output for type_at_cursor: %s" str)

(* todo: should look for type if no doc, or goto def if no doc *)
let show_doc_at_cursor frm =
  let command = spf "document -position %s" 
    (str_of_current_position frm) in
  let (j, str) = execute_command frm command in
  match j with
  | J.String str -> 
    Message.message frm str
  | _ -> failwith (spf "wrong JSON output for doc_at_cursor: %s" str)

let goto_def_at_cursor (look_for : string) (frm : Frame.t) : unit =
  let command = spf "locate -position %s -look-for %s" 
    (str_of_current_position frm) look_for in
  let (j, str) = execute_command frm command in
  match j with
  | J.Object [
    "file", J.String file;
    "pos", J.Object ["line", J.Int line; "col", J.Int col];
  ] -> 
    let str = spf "file: %s (%d:%d)" file line col in
    (* for C-M-l to work *)
    Multi_buffers.set_previous_frame frm;

    let frm = Frame.load_file frm.caps frm.frm_window file in
    let (_buf, text, point) = Frame.buf_text_point frm in
    Text.goto_line text point (line - 1);
    Text.fmove text point (col);
    Message.message frm str

  | J.String str -> 
    Message.message frm str
  | _ -> failwith (spf "wrong JSON output for def_at_cursor: %s" str)
  

let completion_hist = ref []
(* alt: 
 *  - todo: use dabbrev style completion (see abbrev.ml)
 *  - use minibuffer *Completion* style (see select.ml)
 *)
let complete_prefix_at_cursor frm =
  let (buf, text, point) = Frame.buf_text_point frm in
  let prefix = 
    Text.with_dup_point text point (fun mark ->
      let tbl = buf.buf_syntax_table in
      tbl.(Char.code '.') <- true;
      Move.to_begin_of_word text mark tbl;
      tbl.(Char.code '.') <- false;
      Text.region text mark point
    )
  in
  UCommon.pr2_gen prefix;
  let command = spf "complete-prefix -prefix '%s' -position %s -types n -doc n"
    prefix (str_of_current_position frm) in
  let (j, str) = execute_command frm command in
  match j with
  | J.Object [
    "entries", J.Array entries;
    "context", _contextTODO
  ] -> 
    let entries = entries |> List.map (fun j ->
      match j with
      | J.Object [
        "name", J.String name;
        (* less: also displays the type and ocamldoc if available *)
        "kind", J.String _kind;
        "desc", J.String _desc;
        "info", J.String _info;
      ] -> name
      | _ -> failwith (spf "wrong JSON output for complete_prefix entry: %s"str)
    ) in
      (* todo: fill with prefix *)
      (* todo: colorize completion buffer *)
      Select.select frm "Completion: " completion_hist ""
        (fun _ -> entries)
        (fun s -> s)
        (fun s -> 
          if (prefix =~ ".*[\\.]$" || prefix = "")
          then ()
          else begin
            Move.backward_word buf point;
            Edit.delete_forward_word buf point;
          end;
          Edit.insert_string frm s;
        )
  | _ -> failwith (spf "wrong JSON output for complete_prefix: %s" str)

(* similar to Emacs Tags search *)
let goto_def frm =
  let _point = frm.frm_point in
  (* less: use what is under the cursor *)
  let prefix = "" in

  (* step1: choose the module *)  
  let command = spf "complete-prefix -prefix '%s' -position %s -types n -doc n"
    prefix (str_of_current_position frm) in
  let (j, str) = execute_command frm command in
  match j with
  | J.Object [
    "entries", J.Array entries;
    "context", _contextTODO
  ] -> 
    let entries = entries |> List.map (fun j ->
      match j with
      | J.Object [
        "name", J.String name;
        (* less: also displays the type and ocamldoc if available *)
        "kind", J.String _kind;
        "desc", J.String _desc;
        "info", J.String _info;
      ] -> name
      | _ -> failwith (spf "wrong JSON output for goto_def entry: %s"str)
    ) in
      (* less: fill with prefix *)
      (* less: colorize completion buffer *)
      Select.select frm "Goto Def: " completion_hist ""
        (fun _ -> entries)
        (fun s -> s)
        (fun prefix -> 
          let look_for = "implementation" in
          (* todo: step2: choose the entity inside a module *)
          (* step3: go to the def *)
          (* less: reuse more code with goto_def_at_cursor *)
          let command = spf "locate -prefix %s -position %s -look-for %s" 
            prefix (str_of_current_position frm) look_for in
          let (j, str) = execute_command frm command in
          match j with
            | J.Object [
              "file", J.String file;
              "pos", J.Object ["line", J.Int line; "col", J.Int col];
            ] -> 
              let str = spf "file: %s (%d:%d)" file line col in
              (* for C-M-l to work *)
              Multi_buffers.set_previous_frame frm;

              let frm = Frame.load_file frm.caps frm.frm_window file in
              let (_buf, text, point) = Frame.buf_text_point frm in

              Text.goto_line text point (line - 1);
              Text.fmove text point (col);
              Message.message frm str

            | J.String str -> 
              Message.message frm str
            | _ -> failwith (spf "wrong JSON output for def_at_cursor: %s" str)

        )
  | _ -> failwith (spf "wrong JSON output for complete_prefix: %s" str)


(*****************************************************************************)
(* Minor mode *)
(*****************************************************************************)

let mode = Ebuffer.new_minor_mode  "Merlin" []

let merlin_mode = 
  Minor_modes.toggle_minor mode
[@@interactive]

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ =
 [
  [Keymap.c_c; ControlMap, Char.code 't'], show_type_at_cursor;
  (* 'i' for information *)
  [Keymap.c_c; ControlMap, Char.code 'i'], show_doc_at_cursor;
  (* replacement for the traditional Emacs tags-find command *)
  (* alt: (goto_def_at_cursor "implementation"); *)
  [MetaMap, Char.code '.'], goto_def;
  (* 'd' for 'definition' below *)
  [Keymap.c_c; ControlMap, Char.code 'd'],(goto_def_at_cursor"implementation");
  [Keymap.c_c; ControlMap, Char.code 'D'], (goto_def_at_cursor"interface");
  (* todo: real indent_or_complete, move in each major mode *)
  [ControlMap, XK.xk_Tab], (fun frm ->
    let (_, text, point) = Frame.buf_text_point frm in
    if Text.point_col text point =|= 0
    then Edit.insert_string frm "  "
    else complete_prefix_at_cursor frm;
    );
 ] |> List.map (fun (key,action) -> Keymap.add_binding mode.min_map key action)
