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
open Keymap
open Efuns


(*s: function Dired.update *)
let update buf =
  let filename = match buf.buf_filename with
      None -> failwith "Not a directory"
    | Some filename -> filename in
  let s = Utils.load_directory filename in
  let text = buf.buf_text in
  Text.update text s;
  buf.buf_last_saved <- Text.version text
(*e: function Dired.update *)

(*s: constant Dired.file_reg *)
let file_reg = Str.regexp ".* \\([^ ]+\\)$"
(*e: constant Dired.file_reg *)
  
(*s: function Dired.get_file_line *)
let get_file_line frame =
  frame.frm_buffer.buf_filename |> Common.do_option (fun filename ->
    (Efuns.location()).loc_dirname <- Filename.dirname filename;
  );
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let start_point = Text.dup_point text point in
  let before = Text.point_to_bol text point in
  let after = Text.point_to_eol text point in
  Text.bmove text start_point before;
  let line = Text.sub text start_point (before + after) in
  Text.remove_point text start_point;
  line
(*e: function Dired.get_file_line *)
    
(*s: function Dired.select_file *)
let select_file line =
  if line.[0] = ' ' 
  then
    if Str.string_match file_reg line 0 
    then Str.matched_group 1 line 
    else String.sub line 60 (String.length line - 60)
  else
    failwith "Dired: not a file line"
(*e: function Dired.select_file *)

(*s: function Dired.dirname *)
let dirname frame =
  match frame.frm_buffer.buf_filename with
    None -> "."
  | Some dirname -> dirname 
(*e: function Dired.dirname *)
      
(*s: function Dired.fullname *)
let fullname frame filename = 
  Filename.concat (dirname frame) filename
(*e: function Dired.fullname *)
      
(*s: function Dired.open_file *)
let open_file frame =
  let filename = fullname frame (select_file (get_file_line frame)) in
  let buf = Ebuffer.read filename (Keymap.create ()) in
  let frame = Frame.create  frame.frm_window None buf in
  Frame.active frame
(*e: function Dired.open_file *)
  
(*s: function Dired.remove *)
let remove frame =
  let line = get_file_line frame in   
  let filename = select_file line in
  Select.select_yes_or_no frame (Printf.sprintf "Remove %s ? (y/n)" filename)
    (fun b -> if b then
          if line.[1] = 'd' then Unix.rmdir filename else
          Unix.unlink filename;
        update frame.frm_buffer) |> ignore
(*e: function Dired.remove *)

(*s: constant Dired.view_list *)
let view_list = ref []
(*e: constant Dired.view_list *)
(*s: constant Dired.old_view_list *)
let old_view_list = ref []
(*e: constant Dired.old_view_list *)
(*s: constant Dired.compiled_view_list *)
let compiled_view_list = ref []
(*e: constant Dired.compiled_view_list *)
  
(*s: function Dired.fast_view *)
let fast_view frame filename =
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
            Unix.chdir (dirname frame);
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
(*e: function Dired.fast_view *)
  
(*s: function Dired.open_view *)
let open_view frame =
  let filename = select_file (get_file_line frame) in
  fast_view frame filename
(*e: function Dired.open_view *)
  
(*s: function Dired.mkdir *)
let mkdir frame =
  failwith "Dired.mkdir: TODO"
(*
  Select.select_filename frame "Make directory: "
    (fun str -> 
      let file_perm = try get_var frame.frm_buffer file_perm with _ -> 
            0x1ff land (lnot umask) in
      Unix.mkdir str file_perm;
      update frame.frm_buffer)
*)
(*e: function Dired.mkdir *)
          
(*s: function Dired.install *)
let install buf = 
  match buf.buf_filename with
    None -> 
      failwith "Dired: Not a directory"
  | Some filename ->
      if not (Utils.is_directory filename) then 
        failwith (Printf.sprintf "Dired: %s not a directory" filename);
      update buf
(*e: function Dired.install *)
      
(*s: constant Dired.mode *)
let mode = Ebuffer.new_major_mode "Dired" [install]
(*e: constant Dired.mode *)

(*s: constant Dired.map *)
let map = mode.maj_map
(*e: constant Dired.map *)
  

(*s: function Dired.viewer *)
let viewer commande frame filename =
  Sys.command (Printf.sprintf "(%s %s) &" commande filename) |> ignore
(*e: function Dired.viewer *)

(*s: function Dired.commande *)
let commande commande frame filename =
  Sys.command (Printf.sprintf commande filename) |> ignore;
  failwith  (Printf.sprintf commande filename)
(*e: function Dired.commande *)
  
(*s: function Dired.unzip_and_view *)
let unzip_and_view frame filename =
  let new_filename = Printf.sprintf "/tmp/efuns-view-%s" (
      Filename.chop_extension filename) in
  let res = Sys.command (
      Printf.sprintf "gzip -cd %s > %s" filename new_filename)
  in
  if res = 0 then fast_view frame new_filename
(*e: function Dired.unzip_and_view *)
    
(*s: toplevel Dired._1 *)
let _ = 
  Keymap.interactive map [NormalMap, XK.xk_Return] "dired_open_file" open_file;
  Keymap.interactive map [NormalMap, Char.code 'g'] "dired_update" 
  (fun frame -> update frame.frm_buffer);  
  Keymap.interactive map [NormalMap, Char.code 'v'] "dired_view_file" open_view;  
  Keymap.interactive map [NormalMap, Char.code '+'] "dired_make_directory" 
    mkdir;  
  Keymap.interactive map [NormalMap, Char.code '-'] "dired_remove_entry" 
    remove;  
  
  view_list := [
    ".*\\(\\.jpg\\|\\..gig\\|\\.xpm\\|\\.ppm\\)",viewer "xv";
    ".*\\(\\.ps\\|\\.PS\\)",viewer "gv";
    ".*\\(\\.dvi\\)",viewer "xdvi";
    ".*\\(\\.gz\\|\\.Z\\|\\.z\\)",unzip_and_view; 
    ".*\\.tgz", commande "xterm -e sh -c \"tar zvtf %s | less\"";
    ".*\\.tar", commande "xterm -e sh -c \"tar vtf %s | less\"";
    ".*\\.tar", commande "xterm -e sh -c \"tar vtf %s | less\"";
    ];
  
  Efuns.add_start_hook (fun () ->
    add_interactive ((Efuns.location()).loc_map) "dired_mode" 
        (fun frame -> 
          Ebuffer.set_major_mode frame.frm_buffer mode);
    set_global Ebuffer.modes_alist 
      ((".*/$",mode) :: (get_global Ebuffer.modes_alist));
  )   
(*e: toplevel Dired._1 *)
(*e: major_modes/dired.ml *)
