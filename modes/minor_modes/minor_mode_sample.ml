(*s: modes/minor_modes/minor_mode_sample.ml *)
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
open Efuns
          
(*s: constant [[Minor_mode_sample.minor_mode_name]] *)
let minor_mode_name = "my_minor"
(*e: constant [[Minor_mode_sample.minor_mode_name]] *)

(*s: function [[Minor_mode_sample.install]] *)
let install _buf = 
  ()
(*e: function [[Minor_mode_sample.install]] *)

(*s: function [[Minor_mode_sample.minor_mode_fun]] *)
let minor_mode_fun _frame = ()
(*e: function [[Minor_mode_sample.minor_mode_fun]] *)
  
(*s: constant [[Minor_mode_sample.mode]] *)
let mode = Ebuffer.new_minor_mode  minor_mode_name [install]
(*e: constant [[Minor_mode_sample.mode]] *)

(*s: toplevel [[Minor_mode_sample._1]] *)
let _ = 
  List.iter
    (fun key -> 
      Keymap.add_binding mode.min_map [NormalMap, Char.code key]
        minor_mode_fun
  )
  [ ] (* no keys *)
(*e: toplevel [[Minor_mode_sample._1]] *)

(*s: toplevel [[Minor_mode_sample._2]] *)
let _ = 
  Action.define_action (minor_mode_name ^ "_mode") 
    (Minor_modes.toggle_minor mode)
(*e: toplevel [[Minor_mode_sample._2]] *)
    
(*e: modes/minor_modes/minor_mode_sample.ml *)
