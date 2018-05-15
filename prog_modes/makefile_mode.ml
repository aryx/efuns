(*s: prog_modes/makefile_mode.ml *)
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
open Options
open Efuns

(*s: constant [[Makefile_mode.mkfile_vars]] *)
let mkfile_vars= Str.regexp "\\(\\$([a-zA-Z0-9_]*)\\)\\|\\([a-zA-Z0-9_]+=\\)"
(*e: constant [[Makefile_mode.mkfile_vars]] *)
(*s: constant [[Makefile_mode.mkfile_target]] *)
let mkfile_target= Str.regexp "^.*:"
(*e: constant [[Makefile_mode.mkfile_target]] *)
(*s: constant [[Makefile_mode.mkfile_rules]] *)
let mkfile_rules= Str.regexp "^\t.*$"
(*e: constant [[Makefile_mode.mkfile_rules]] *)

let mkfile_comments= Str.regexp "^#.*$"

(*s: constant [[Makefile_mode.rules_color]] *)
let rules_color = define_option ["makefile_mode";"rules_color"] "" 
  string_option "orange"
(*e: constant [[Makefile_mode.rules_color]] *)
(*s: constant [[Makefile_mode.target_color]] *)
let target_color = define_option ["makefile_mode"; "target_color"] ""
    string_option "MediumAquamarine"
(*e: constant [[Makefile_mode.target_color]] *)
(*s: function [[Makefile_mode.makefile_color]] *)
let makefile_color buf =
  Simple.color buf mkfile_rules false
    (Text.make_attr (Attr.get_color !!rules_color) 1 0 false);
  Simple.color buf mkfile_target false 
    (Text.make_attr (Attr.get_color !!target_color) 1 0 false);
  Simple.color buf mkfile_vars false 
    (Text.make_attr (Attr.get_color !!Pl_colors.variable_name_color) 1 0 false);
  Simple.color buf mkfile_comments false 
    (Text.make_attr (Attr.get_color !!Pl_colors.comment_color) 1 0 false);
  ()
(*e: function [[Makefile_mode.makefile_color]] *)
 
(*s: function [[Makefile_mode.install]] *)
let install buf =
  makefile_color buf;
  Action.execute_buffer_action "tab_mode" buf
(*e: function [[Makefile_mode.install]] *)
  
(*s: constant [[Makefile_mode.mode]] *)
let mode = Ebuffer.new_major_mode "Makefile" [makefile_color]
(*e: constant [[Makefile_mode.mode]] *)
  

(*s: constant [[Makefile_mode.local_map]] *)
let local_map = define_option ["makefile_mode"; "local_map"] ""
    (list_option Simple.binding_option) []
(*e: constant [[Makefile_mode.local_map]] *)

(*s: constant [[Makefile_mode.interactives_map]] *)
let interactives_map = define_option ["makefile_mode"; "interactives_map"] ""
    (list_option string2_option) 
  []
(*e: constant [[Makefile_mode.interactives_map]] *)

(* let insert_tab frame = ignore (insert_string frame "\t") *)
 
(*s: toplevel [[Makefile_mode._1]] *)
open Keymap (* c_c *)
let _ =
  if !!local_map = [] then
    local_map =:= [
      [c_c; ControlMap, Char.code 'c'], "compile";    
      [ControlMap, Char.code 'l'], "makefile_mode.color_buffer";
     (*  [NormalMap, XK.xk_Tab], "insert_tab"; *)
    ];
  if !!interactives_map = [] then 
        interactives_map =:= [
          "color_buffer", "makefile_mode.color_buffer";
          (*"compile", "makefile_mode.compile";*)
      ]
(*e: toplevel [[Makefile_mode._1]] *)

(*s: function [[Makefile_mode.makefile_mode]] *)
let makefile_mode frame = 
  Ebuffer.set_major_mode frame.frm_buffer mode
(*e: function [[Makefile_mode.makefile_mode]] *)
        
(*s: toplevel [[Makefile_mode._2]] *)
let _ = 
  Action.define_action "makefile_mode.color_buffer" 
    (fun frame -> makefile_color frame.frm_buffer);
  Action.define_action "makefile_mode" makefile_mode;
  ()
(*e: toplevel [[Makefile_mode._2]] *)

(*s: toplevel [[Makefile_mode._3]] *)
let setup_maps () =
  let map = mode.maj_map in
  !!local_map |> List.iter (fun (keys, action) ->
      try
        Keymap.add_binding map keys (Action.execute_action action)
      with e ->
          Log.printf "Error for action %s" action;
          Log.exn "%s\n" e;
  
  );
  !!interactives_map |> List.iter (fun (name, action) ->
      try
        Keymap.add_interactive map name (Action.execute_action action)
      with e ->
          Log.printf "Error for action %s" action;
          Log.exn "%s\n" e;
          
  );
  ()
(*e: toplevel [[Makefile_mode._3]] *)

  
(*s: toplevel [[Makefile_mode._4]] *)
let _ = 
  (* Keymap.add_prefix mode.maj_map [c_c];   *)
  Hook.add_start_hook (fun () ->
    setup_maps();
    let alist = Var.get_global Ebuffer.modes_alist in
    Var.set_global Ebuffer.modes_alist ((".*/[Mm]akefile.*",mode):: alist);
    
    Parameter.add_option_parameter target_color;
    Parameter.add_option_parameter rules_color;
  )   
(*e: toplevel [[Makefile_mode._4]] *)
(*e: prog_modes/makefile_mode.ml *)
