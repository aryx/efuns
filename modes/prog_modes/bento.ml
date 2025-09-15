(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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
open Compil
open Options
module J = JSON

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A small wrapper around the Bento code checker.
 *
 * Bento is a code-quality/productivity tool.
 * See https://github.com/returntocorp/bento for more information on Bento.
 *
 * todo:
 *  - factorize things in a linter.ml, checker.ml, common_linter.ml?
 *
 * related work:
 *  - Emacs binding:
 *    https://github.com/returntocorp/bento-emacs/blob/master/bento.el
 *)

(*****************************************************************************)
(* Constants and globals *)
(*****************************************************************************)
(* I assume this program is in your PATH *)
let external_program = "bento"
(* Config file to look for in a project *)
let bento_config_file = ".bento.yml"

let debug = ref true

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let rec find_bento_root_dir_opt dir =
  if Sys.file_exists (Filename.concat dir bento_config_file)
  then Some dir
  else 
    let newdir = Filename.dirname dir in
    if newdir = dir
    then None
    else find_bento_root_dir_opt newdir


let json_of_bento_check rootdir =
  (* less: with_chdir, or a shell_quote on rootdir? *)
  let command = spf "cd %s; %s check --formatter bento.formatter.Json" 
      rootdir external_program in
  let pipe_read = Unix.open_process_in command in
  let str = 
    try input_line pipe_read 
    with End_of_file -> "[]" (* no errors, empty Json array *)
  in
  let _status = Unix.close_process_in pipe_read in
  if !debug
  then UCommon.pr2 str;
  JSON.json_of_string str

(* generate error strings compatible with Compil.c_error_regexp *)
let json_to_errors json =
  match json with
  | J.Array xs ->
    xs |> List.map (function
      | J.Object [
            "tool_id", _;
            "check_id", _;
            "line", J.Int line;
            "column", _;
            "message", J.String message;
            "severity", J.Int severity;
            "path", J.String path;
        ] ->
            spf "%s:%d:%s:%s" path line 
              (match severity with
              | 0 ->  "Advice"
              | 1 ->  "Warning"
              | _ ->  "Error"
              ) message
      | _ -> failwith "wrong bento JSON format"
     ) |> String.concat "\n"
  | _ -> failwith "wrong bento JSON format"

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)
(* similar to Compil.color_buffer and Pl_colors.color_number_and_punctuation*)
let color_buffer buf =
  Dircolors.colorize_buffer buf;

  let re = Str.regexp ":Error:" in
  Color.color buf re false
    (Text.make_attr (Attr.get_color "red") 1 0 false);
  let re = Str.regexp ":Warning:" in
  Color.color buf re false
    (Text.make_attr (Attr.get_color "orange") 1 0 false);
  let re = Str.regexp ":Advice:" in
  Color.color buf re false
    (Text.make_attr (Attr.get_color "yellow") 1 0 false);

  Color.color buf 
    (Str.regexp ("\\b[0-9]+\\b")) false
      (Text.make_attr (Attr.get_color !!Pl_colors.number_color) 1 0 false);

  ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let bento_check (frame : Frame.t) : unit =
  let dir = Frame.current_dir frame in
  let rootdir_opt = find_bento_root_dir_opt dir in
  (match rootdir_opt with
  | None -> Message.message frame 
        (spf "no %s found starting from %s; did you run 'bento init'?"
          bento_config_file dir)
  | Some rootdir ->
    let json = json_of_bento_check rootdir in
    let errors = json_to_errors json in

    (* copy-paste of code in Compil.grep *)
    let comp_window =
       match !compilation_frame with
       | None -> Multi_frames.cut_frame frame 
       | Some (new_frame,error_point, _) ->
          Text.remove_point new_frame.frm_buffer.buf_text error_point;
          Ebuffer.kill new_frame.frm_buffer;
          if new_frame.frm_killed 
          then Multi_frames.cut_frame frame
          else new_frame.frm_window 
    in

    let buf_name = "*bento*" in
    let text = Text.create errors in
    let buf = Ebuffer.create buf_name None text (Keymap.create ()) in
    (* todo? propagate Compil vars? *)

    color_buffer buf;
    Ebuffer.set_major_mode buf Compil.mode;
    let comp_frame = Frame.create frame.caps comp_window None buf in
    Frame.active frame; (* switch back to original frame *)
    let error_point = Text.new_point buf.buf_text in
    compilation_frame := Some (comp_frame, error_point, rootdir)
  )
[@@interactive]
