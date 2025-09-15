(* Yoann Padioleau
 *
 * Copyright (C) 2015 Yoann Padioleau
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

module E = Entity_code
module HC = Highlight_code
module Db = Database_code
module PH = Parse_and_highlight

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Common code of the different pfff-based efuns programming language modes
 * for coloring, outline, tags, ...
 * 
 * todo: 
 *  - indentation, 
 *  - codegraph-based navigation, 
 *  - ?
 *)


(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* see codemap/highlighters/Parse_and_highlight.ml now *)

(*****************************************************************************)
(* Highlight and outline *)
(*****************************************************************************)

(* dupe of pfff/code_map/draw_microlevel.ml *)
let color_of_categ categ =
  let attrs = Highlight_code.info_of_category categ in
  attrs |> List_.find_some (fun attr ->
    match attr with
    | `FOREGROUND s 
    | `BACKGROUND s (* todo: should really draw the background of the text *)
      -> 
        Some (s)
    | _ -> None
  )

(* dupe of pfff/code_map/style.ml#size_font_multiplier_of_categ *)
(* this is part of the attribute, so it must be an int between
 * 0 and 255. Right now 0 means default size.
 * This is used only for the minimap for now.
 *)
let size_of_categ categ =
  match categ with
  | HC.Entity (kind, HC.Def2 _) ->
    (match kind with
    | E.Field | E.Method | E.ClassConstant | E.Constructor -> 1
    | _ -> 3
    )

  | HC.CommentSection0 -> 5
  | HC.CommentSection1 -> 4
  | HC.CommentSection2 -> 2
  | HC.CommentSection3 -> 1
  | HC.CommentSection4 -> 1

  | _ -> 0

(* for outline, maybe could factorize with level_of_categ *)
let level_of_categ categ =
  match categ with
  | HC.CommentSection0 -> 1

  | HC.CommentSection1 -> 2

  | HC.CommentSection2 -> 3
  | HC.CommentSection3 -> 4
  | HC.CommentSection4 -> 5

  (* maybe module def should be 1 too *)
  | HC.Entity (kind, HC.Def2 _) -> 
    (match kind with
    | E.Field | E.Method | E.ClassConstant | E.Constructor -> 3
    | _ -> 2
    )

  (* should be max_int? *)
  | _ -> 0


let colorize_and_set_outlines funcs buf (file : Fpath.t) =
  let xs = funcs.PH.parse file in
  let prefs = Highlight_code.default_highlighter_preferences in
  let text = buf.buf_text in

  (* for outline *)
  let outline_points = ref [] in
  let hcovered_lines = Hashtbl.create 101 in

  Text.with_new_point text (fun cursor ->
      xs |> funcs.PH.highlight ~tag_hook:(fun info categ->
        let color = color_of_categ categ in
        let fontsize = size_of_categ categ in
        let lvl = level_of_categ categ in
        
        let pos = Tok.bytepos_of_tok info in
        Text.set_position text cursor pos;
        let line = Tok.line_of_tok info in

        if lvl > 0 && not (Hashtbl.mem hcovered_lines line) then begin
          Hashtbl.add hcovered_lines line true;
          Stack_.push (lvl, Text.dup_point text cursor) outline_points;
        end;

        let attr = Text.make_attr (Attr.get_color color) 1 fontsize false in
        let str = Tok.content_of_tok info in
        let len = String.length str in
        Text.set_attrs text cursor len attr
      ) prefs file
  );
  (* less: need to set a finalizer for the points stored in outline_points?
   * meh
   *)
  let outline_points = 
    match List.rev !outline_points with
    (* to avoid some index out of bounds create at least one point *)
    | [] -> [0, Text.new_point text]
    | xs -> 
      (* we have no guarantee in which order the hooks are called *)
      xs |> List.sort (fun (_, pt_a) (_, pt_b) -> compare pt_a pt_b)
  in
  Var.set_local buf Outline_mode.outline_var outline_points;
  (* bugfix: needs that to force a redisplay *)
  buf.buf_modified <- buf.buf_modified + 1;
  ()


(*****************************************************************************)
(* Database_code *)
(*****************************************************************************)

let dir_to_db = ref []

let load_database_code frame =
  Select.select_file_from_pwd frame "Load database code file: " (fun file ->
    let dir = Filename.dirname file in
    let db = Database_code.load_database file in
    let entities = 
      Database_code.files_and_dirs_and_sorted_entities_for_completion 
        ~threshold_too_many_entities:250000 db
    in
    let idx = Big_grep.build_index entities in
    Stack_.push (dir, (db, dir, entities, idx)) dir_to_db
  )
[@@interactive]


let db_for_frame _frame =
  (* todo: look for loc_dirname, try to find a matching one,
   * if not then try to load one automatically,
   * if not then ask user for a db
   *)
  List.hd !dir_to_db |> snd

let def_hist = ref []

let goto_def (frame : Frame.t) : unit =
  let (_db, root, entities, _idx) = db_for_frame frame in

  let xs = entities |> List.map (fun e -> 
    if e.Db.e_fullname = "" 
    then e.Db.e_name, e
    else e.Db.e_fullname, e
  )
  in
  let h = Hashtbl_.hash_of_list xs in
  let ys = xs |> List.map fst in

  Select.select frame "Def for: " def_hist ""
    (fun _ -> ys)
    (fun s -> s)
    (fun str -> 
      let e = Hashtbl.find h str in
      let file = Filename.concat root e.Db.e_file in
      let new_frame = Frame.load_file frame.caps frame.frm_window file in
      let pt = new_frame.frm_point in
      let text = new_frame.frm_buffer.buf_text in
      let re_str = (spf "\\b%s\\b" e.Db.e_name) in
      try 
        Text.search_forward text (Str.regexp re_str) pt |> ignore
      with Not_found ->
        failwith (spf "Could not find entity %s (with re = %s)" 
                    e.Db.e_name re_str)
    )
[@@interactive]

(* todo: 
 * - fuzzy file finder
 * - fuzzy entity finder
 * - start from what is under the cursor
 * - correct jump to right place in the file
 * - inline completion in the text
 *)


(*****************************************************************************)
(* Graph_code *)
(*****************************************************************************)

let dir_to_graph = ref []

let load_graph_code frame =
  Select.select_file_from_pwd frame "Load graph file: " (fun file ->
    let dir = Filename.dirname file in
    let g = Graph_code.load (Fpath.v file) in
    Stack_.push (dir, g) dir_to_graph
  )
[@@interactive]


(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ =
  Hooks.add_start_hook (fun () ->
    Keymap.add_global_key [MetaMap, Char.code '.'] goto_def;
    ()
  )
