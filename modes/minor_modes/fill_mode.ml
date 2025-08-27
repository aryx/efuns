(*s: modes/minor_modes/fill_mode.ml *)
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
          
(*s: constant [[Fill_mode.minor_mode_name]] *)
let minor_mode_name = "fill"
(*e: constant [[Fill_mode.minor_mode_name]] *)
  
(*s: function [[Fill_mode.install]] *)
let install _buf = ()
(*e: function [[Fill_mode.install]] *)

(*s: constant [[Fill_mode.fill_line_len]] *)
let fill_line_len = define_option ["fill_line_len"] "" int_option 80
(*e: constant [[Fill_mode.fill_line_len]] *)
  
(*s: constant [[Fill_mode.mode]] *)
let mode = Ebuffer.new_minor_mode minor_mode_name [install]
(*e: constant [[Fill_mode.mode]] *)

(*s: constant [[Fill_mode.fill_on_char]] *)
let fill_on_char = define_option ["fill_on_char"] "" string_option " "
(*e: constant [[Fill_mode.fill_on_char]] *)
  
(*s: toplevel [[Fill_mode._1]] *)
let _ =
  let fill_on_char = !!fill_on_char in
  for i = 0 to String.length fill_on_char - 1 do 
      Keymap.add_binding mode.min_map [NormalMap, Char.code fill_on_char.[i]] 
        Electric.electric_insert_space
  done
(*e: toplevel [[Fill_mode._1]] *)
  
(*s: toplevel [[Fill_mode._2]] *)
let _ = 
  Action.define_action (minor_mode_name ^ "_mode") 
    (Minor_modes.toggle_minor mode)
(*e: toplevel [[Fill_mode._2]] *)
    
(*e: modes/minor_modes/fill_mode.ml *)
