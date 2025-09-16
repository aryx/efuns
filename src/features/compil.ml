(*s: features/compil.ml *)
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
open Regexp_.Operators
open Options
open Efuns

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * todo:
 *  - handle return on an error!
 *  - factorize code between compil and grep
 *)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

(*s: constant [[Compil.compilation_frame]] *)
let compilation_frame = ref None
(*e: constant [[Compil.compilation_frame]] *)

(*s: type [[Compil.error]] *)
type error = {
    (* error location *)    
    err_filename : string;
    err_line : int;
    err_begin : int;
    err_end : int;
    (* error message *)
    err_msg : int;
  }
(*e: type [[Compil.error]] *)

type find_error_fun = Text.t -> Text.point -> error

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: constant [[Compil.c_error_regexp]] *)
let c_error_regexp = define_option ["compil"; "error_regexp"] "" regexp_option
  (string_to_regex "^\\([^:\n]+\\):\\([0-9]+\\):.*$")
(*e: constant [[Compil.c_error_regexp]] *)

(*s: function [[Compil.c_find_error]] *)
let find_error_gen re text error_point =
  let groups = 
    Text.search_forward_groups text re
      error_point 2 in
  let error =
    {  
      err_msg = Text.get_position text error_point;
      err_filename = groups.(0);
      err_line = (int_of_string groups.(1)) - 1;
      err_begin = 0;
      err_end = 0;
    } in
  Text.fmove text error_point 1;
  error
(*e: function [[Compil.c_find_error]] *)

(* apparently the Core Filename module has an expand function doing that *)
let expand_tilde (caps : < Cap.env ; ..>) (s : string) : string =
  if s =~ "^~/\\(.*\\)$"
  then
    let rest = Regexp_.matched1 s in
    Filename.concat (CapSys.getenv caps "HOME") rest
  else s

(*****************************************************************************)
(* More vars *)
(*****************************************************************************)

(*s: constant [[Compil.find_error]] *)
let find_error = Store.create_abstr "find_error"
(*e: constant [[Compil.find_error]] *)

let find_error_location_regexp = Store.create_abstr "find_error_loc_regexp"

let find_error_of_buf buf = 
  try Var.get_var buf find_error
  with Not_found | Failure _ -> 
     let re = 
        try Var.get_var buf find_error_location_regexp
        with Not_found | Failure _ -> snd !!c_error_regexp
     in
     find_error_gen re

(*****************************************************************************)
(* M-x next_error *)
(*****************************************************************************)
  
(*s: function [[Compil.next_error]] *)
let next_error (top_frame : Frame.t) : unit =
  match !compilation_frame with
  | None -> Message.message top_frame "No compilation started"
  | Some (frame, error_point, cdir) ->      
      if frame.frm_killed 
      then Frame.unkill (Multi_frames.cut_frame top_frame) frame;

      let (buf, text, point) = Frame.buf_text_point frame in
      let find_error = find_error_of_buf buf in
      try
        let error = find_error text error_point in
        Text.set_position text frame.frm_start error.err_msg;
        Text.set_position text point error.err_msg;
        frame.frm_force_start <- true;
        frame.frm_redraw <- true;
        if error.err_filename <> "" then
          let filename = 
            if error.err_filename =~ "^/"
            then error.err_filename
            else Filename.concat cdir error.err_filename 
          in
          let buf = Ebuffer.read filename (Keymap.create ()) in
          (* new frame for error buffer *)
          let frame = 
            try Frame.find_buffer_frame buf 
            with Not_found ->
                if Eq.phys_equal frame top_frame then
                  let new_window = Top_window.create top_frame.caps
                      (*Window.display top_window*) 
                  in
                  Frame.create frame.caps new_window.window None buf
                else
                  Frame.create frame.caps top_frame.frm_window None buf
          in
          let text = buf.buf_text in
          let point = frame.frm_point in
          Text.point_to_line text point error.err_line;
          Text.fmove text point error.err_begin;
          Frame.active frame
      with Not_found ->
        Message.message top_frame "No more errors"
[@@interactive]
(*e: function [[Compil.next_error]] *)

(*****************************************************************************)
(* Options *)
(*****************************************************************************)

(*s: constant [[Compil.compile_find_makefile]] *)
let compile_find_makefile = define_option ["compil";"find_makefile"] ""
    bool_option true
(*e: constant [[Compil.compile_find_makefile]] *)
  
(*s: constant [[Compil.make_command]] *)
(* old: was "make -k" to keep going after first error, but I prefer without*)
let make_command = define_option ["compil";"make_command"] ""
    string_option "make"
(*e: constant [[Compil.make_command]] *)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let find_error_error_regexp = Store.create_abstr "find_error_err_regexp"

let color_buffer buf =
  Dircolors.colorize_buffer buf;

  let re =
    try Var.get_var buf find_error_location_regexp
    with Not_found | Failure _ -> snd !!c_error_regexp
  in
  Color.color buf re false
    (Text.make_attr (Attr.get_color "green") 1 0 false);

  let re =
    try Var.get_var buf find_error_error_regexp
    with Not_found | Failure _ -> Str.regexp "Error"
  in
  Color.color buf re false
    (Text.make_attr (Attr.get_color "red") 1 0 false);
  ()

(*****************************************************************************)
(* The mode *)
(*****************************************************************************)
  
let mode = Ebuffer.new_major_mode "Compilation"  None

(*****************************************************************************)
(* M-x compile *)
(*****************************************************************************)
  
(*s: constant [[Compil.make_hist]] *)
let make_hist = ref [!!make_command]
(*e: constant [[Compil.make_hist]] *)
(*s: function [[Compil.compile]] *)
let compile (frame : Frame.t) =
  let default = List.hd !make_hist in
  Select.select_string frame 
    ("Compile command: (default :"^ default^") " )
    make_hist ""
    (fun cmd -> 
      let cmd = if cmd = "" then default else cmd in (* cmd ||| default *)
      let cdir = Frame.current_dir frame in
      (*s: [[Compil.compile()]] find possibly cdir with a makefile *)
      let cdir, cmd = 
        match () with
        | _ when cmd =~ "^cd +\\([^;]+\\);\\(.*\\)$" ->
            let (dir, cmd) = Regexp_.matched2 cmd in
            let finaldir = expand_tilde frame.caps dir in
            finaldir, cmd
        | _ when !!compile_find_makefile && cmd =~ "^make" ->
          (* try to find a Makefile in the directory *)
            let rec iter dir =
              if Sys.file_exists (Filename.concat dir "Makefile") ||
                 Sys.file_exists (Filename.concat dir "makefile") ||
                 Sys.file_exists (Filename.concat dir "GNUmakefile")
              then dir 
              else
                let newdir = Filename.dirname dir in
                if newdir = dir 
                then cdir 
                else iter newdir
            in
            iter cdir, cmd
        | _ -> cdir, cmd
      in
      (*e: [[Compil.compile()]] find possibly cdir with a makefile *)
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
      (* this makes also comp_frame the active frame *)
      let comp_frame = 
        System.start_command frame.caps cdir "*Compile*" comp_window cmd 
        (Some (fun buf _status -> color_buffer buf))
      in
      (* switch back cursor to original frame *)
      Frame.active frame;
      let buf = comp_frame.frm_buffer in
      let error_point = Text.new_point buf.buf_text in
      compilation_frame := Some (comp_frame, error_point, cdir);

      (* propagate vars *)
      let buf2 = frame.frm_buffer in
      (try 
         let x = Var.get_var buf2 find_error in
         Var.set_local buf find_error x
       with Not_found | Failure _ -> ()
      );
      (try 
         let x =  Var.get_var buf2 find_error_location_regexp in
         Var.set_local buf find_error_location_regexp x
       with Not_found | Failure _ -> ()
      );
      (try 
         let x =  Var.get_var buf2 find_error_error_regexp in
         Var.set_local buf find_error_error_regexp x
       with Not_found | Failure _ -> ()
      );
      (* set major mode *)
      Ebuffer.set_major_mode buf mode

  )
[@@interactive]
(*e: function [[Compil.compile]] *)

(*****************************************************************************)
(* M-x grep *)
(*****************************************************************************)

(*s: constant [[Compil.grep_command]] *)
let grep_command = define_option ["compil"; "grep_command"] "" string_option
    "grep -n"
(*e: constant [[Compil.grep_command]] *)
  
(*s: constant [[Compil.grep_hist]] *)
let grep_hist = ref ["grep -n "]
(*e: constant [[Compil.grep_hist]] *)
(*s: function [[Compil.grep]] *)
let grep frame =
  let default = List.hd !grep_hist in
  Select.select_string frame 
    (Printf.sprintf "Grep command: %s (default: %s) " !!grep_command default)
    grep_hist ""
    (fun cmd -> 
      let cmd = if cmd = "" then default else cmd in
      let cmd = !!grep_command ^ " " ^ cmd in
      let cdir = Frame.current_dir frame in
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
      let comp_frame = System.start_command frame.caps cdir "*Grep*" comp_window cmd 
         (Some (fun buf _status -> color_buffer buf))
      in
      Frame.active frame; 
      let buf = comp_frame.frm_buffer in
      let error_point = Text.new_point buf.buf_text in
      compilation_frame := Some (comp_frame, error_point, cdir)
      (* todo? no set major mode? *)
  )
[@@interactive]
(*e: function [[Compil.grep]] *)
(*e: features/compil.ml *)
