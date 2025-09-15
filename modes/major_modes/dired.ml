(*s: major_modes/dired.ml *)
(*s: copyright header *)
(***********************************************************************)
(*                                                                     *)
(*                             ____________                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header *)
open Efuns

(*s: function [[Dired.update]] *)
let update buf =
  let filename = match buf.buf_filename with
      None -> failwith "Not a directory"
    | Some filename -> filename in
  let s = Utils.load_directory filename in
  let text = buf.buf_text in
  Text.update text s;
  (* pad extension *)
  Dircolors.colorize_buffer buf;
  buf.buf_last_saved <- Text.version text
(*e: function [[Dired.update]] *)

(*s: constant [[Dired.file_reg]] *)
let file_reg = Str.regexp ".* \\([^ ]+\\)$"
(*e: constant [[Dired.file_reg]] *)
  
(*s: function [[Dired.get_file_line]] *)
let get_file_line frame =
  frame.frm_buffer.buf_filename |> Option.iter (fun filename ->
    (Globals.editor()).edt_dirname <- Filename.dirname filename;
  );
  let (_, text, point) = Frame.buf_text_point frame in
  Text.with_dup_point text point (fun start_point ->
    let before = Text.point_to_bol text point in
    let after = Text.point_to_eol text point in
    Text.bmove text start_point before;
    let line = Text.sub text start_point (before + after) in
    line
  )
(*e: function [[Dired.get_file_line]] *)
    
(*s: function [[Dired.select_file]] *)
let select_file line =
  if line.[0] = ' ' 
  then
    if Str.string_match file_reg line 0 
    then Str.matched_group 1 line 
    else String.sub line 60 (String.length line - 60)
  else
    failwith "Dired: not a file line"
(*e: function [[Dired.select_file]] *)

(*s: function [[Dired.dirname]] *)
let dirname frame =
  match frame.frm_buffer.buf_filename with
    None -> "."
  | Some dirname -> dirname 
(*e: function [[Dired.dirname]] *)
      
(*s: function [[Dired.fullname]] *)
let fullname frame filename = 
  Filename.concat (dirname frame) filename
(*e: function [[Dired.fullname]] *)
      
(*s: function [[Dired.open_file]] *)
let open_file (frame : Frame.t) =
  let filename = fullname frame (select_file (get_file_line frame)) in
  let buf = Ebuffer.read filename (Keymap.create ()) in
  let frame = Frame.create frame.caps frame.frm_window None buf in
  Frame.active frame
[@@interactive "dired_open"]
(*e: function [[Dired.open_file]] *)
  
(*s: function [[Dired.remove]] *)
let remove frame =
  let line = get_file_line frame in   
  let filename = select_file line in
  Select.select_yes_or_no frame (Printf.sprintf "Remove %s ? (y/n)" filename)
    (fun b -> if b then
          if line.[1] = 'd' then Unix.rmdir filename else
          Unix.unlink filename;
        update frame.frm_buffer) |> ignore
[@@interactive "dired_remove"]
(*e: function [[Dired.remove]] *)

(*s: constant [[Dired.view_list]] *)
let view_list = ref []
(*e: constant [[Dired.view_list]] *)
(*s: constant [[Dired.old_view_list]] *)
let old_view_list = ref []
(*e: constant [[Dired.old_view_list]] *)
(*s: constant [[Dired.compiled_view_list]] *)
let compiled_view_list = ref []
(*e: constant [[Dired.compiled_view_list]] *)
  
(*s: function [[Dired.fast_view]] *)
let fast_view (frame : Frame.t) (filename : string) : unit =
  if not (!old_view_list == !view_list) then
    begin
      compiled_view_list := List.map 
        (fun (file_reg, appli) ->
          Str.regexp file_reg, appli) !view_list;
      old_view_list := !view_list
    end;
  try
    List.iter (fun (regexp, viewer) ->
        if Str.string_match regexp filename 0 then
          try
            CapSys.chdir frame.caps (dirname frame);
            viewer frame filename;
            raise Exit
          with
            _ -> raise Exit
    ) !compiled_view_list;
    let _ = 
      Select.select_yes_or_no frame (Printf.sprintf "Open %s ? (y/n)" filename)
      (fun b -> if b then
            open_file frame) in
      ()

  with
    Exit -> ()      
(*e: function [[Dired.fast_view]] *)
  
(*s: function [[Dired.open_view]] *)
let open_view frame =
  let filename = select_file (get_file_line frame) in
  fast_view frame filename
[@@interactive "dired_view"]
(*e: function [[Dired.open_view]] *)
  
(*s: function [[Dired.mkdir]] *)
let mkdir _frame =
  failwith "Dired.mkdir: TODO"
[@@interactive "dired_mkdir"]
(*
  Select.select_filename frame "Make directory: "
    (fun str -> 
      let file_perm = try get_var frame.frm_buffer file_perm with _ -> 
            0x1ff land (lnot umask) in
      Unix.mkdir str file_perm;
      update frame.frm_buffer)
*)
(*e: function [[Dired.mkdir]] *)
          
(*s: function [[Dired.install]] *)
let install buf = 
  match buf.buf_filename with
    None -> 
      failwith "Dired: Not a directory"
  | Some filename ->
      if not (Utils.is_directory filename) then 
        failwith (Printf.sprintf "Dired: %s not a directory" filename);
      update buf
(*e: function [[Dired.install]] *)
      
(*s: constant [[Dired.mode]] *)
let mode = Ebuffer.new_major_mode "Dired" (Some install)
(*e: constant [[Dired.mode]] *)

(*s: function [[Dired.dired_mode]] *)
let dired_mode =
  Major_modes.enable_major_mode mode
[@@interactive]
(*e: function [[Dired.dired_mode]] *)


(*s: function [[Dired.viewer]] *)
let viewer (cmd : string) (frm : Frame.t) (filename : string) : unit =
  CapSys.command frm.caps (Printf.sprintf "(%s %s) &" cmd filename) |> ignore
(*e: function [[Dired.viewer]] *)

(*s: function [[Dired.commande]] *)
let commande cmd_fmt (frm : Frame.t) (filename : string) : unit =
  CapSys.command frm.caps (Printf.sprintf cmd_fmt filename) |> ignore;
  failwith  (Printf.sprintf cmd_fmt filename)
(*e: function [[Dired.commande]] *)
  
(*s: function [[Dired.unzip_and_view]] *)
let unzip_and_view (frame : Frame.t) (filename : string) : unit =
  let new_filename = Printf.sprintf "/tmp/efuns-view-%s" (
      Filename.chop_extension filename) in
  let res = CapSys.command frame.caps (
      Printf.sprintf "gzip -cd %s > %s" filename new_filename)
  in
  if res = 0 then fast_view frame new_filename
(*e: function [[Dired.unzip_and_view]] *)
    
(*s: toplevel [[Dired._1]] *)
let _ = 
  Keymap.add_major_key mode [NormalMap, XK.xk_Return] open_file;
  Keymap.add_major_key mode [NormalMap, Char.code 'g'] (fun frame -> update frame.frm_buffer);  
  Keymap.add_major_key mode [NormalMap, Char.code 'v'] open_view;  
  Keymap.add_major_key mode [NormalMap, Char.code '+'] mkdir;  
  Keymap.add_major_key mode [NormalMap, Char.code '-'] remove;  

  view_list := [
    ".*\\(\\.jpg\\|\\..gig\\|\\.xpm\\|\\.ppm\\)",viewer "xv";
    ".*\\(\\.ps\\|\\.PS\\)",viewer "gv";
    ".*\\(\\.dvi\\)",viewer "xdvi";
    ".*\\(\\.gz\\|\\.Z\\|\\.z\\)",unzip_and_view; 
    ".*\\.tgz", commande "xterm -e sh -c \"tar zvtf %s | less\"";
    ".*\\.tar", commande "xterm -e sh -c \"tar vtf %s | less\"";
    ".*\\.tar", commande "xterm -e sh -c \"tar vtf %s | less\"";
    ];
  
  Hooks.add_start_hook (fun () ->
    Var.add_global Ebuffer.modes_alist [".*/$",mode];
  )   
(*e: toplevel [[Dired._1]] *)
(*e: major_modes/dired.ml *)
