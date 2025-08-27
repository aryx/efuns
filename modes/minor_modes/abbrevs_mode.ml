(*s: modes/minor_modes/abbrevs_mode.ml *)
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
open Abbrevs
open Efuns

(*s: constant [[Abbrevs_mode.abbreviations]] *)
let abbreviations = define_option ["abbrevs_mode"; "abbrevs"] ""
    (list_option string2_option) []
(*e: constant [[Abbrevs_mode.abbreviations]] *)
  
(*s: constant [[Abbrevs_mode.abbrevs]] *)
let abbrevs = Hashtbl.create 11
(*e: constant [[Abbrevs_mode.abbrevs]] *)
(*s: toplevel [[Abbrevs_mode._1]] *)
let _ =
  Utils.hash_add_assoc abbrevs !!abbreviations
(*e: toplevel [[Abbrevs_mode._1]] *)
  
(*s: function [[Abbrevs_mode.install]] *)
let install buf =
  try Var.get_local buf abbrev_table |> ignore
  with _ -> Var.set_local buf abbrev_table abbrevs
(*e: function [[Abbrevs_mode.install]] *)

(*s: constant [[Abbrevs_mode.mode]] *)
let mode = Ebuffer.new_minor_mode "abbrevs" [install]
(*e: constant [[Abbrevs_mode.mode]] *)

(*s: constant [[Abbrevs_mode.abbrevs_chars]] *)
let abbrevs_chars = define_option ["abbrevs_mode"; "abbrevs_chars"] ""
    string_option " "
(*e: constant [[Abbrevs_mode.abbrevs_chars]] *)
  
(*s: function [[Abbrevs_mode.find_matching]] *)
let find_matching  frame = 
  Edit.self_insert_command frame; 
  Paren_mode.highlight_paren frame
(*e: function [[Abbrevs_mode.find_matching]] *)
(*s: function [[Abbrevs_mode.char_expand_abbrev]] *)
let char_expand_abbrev frame =
  expand_sabbrev frame; Edit.self_insert_command frame
(*e: function [[Abbrevs_mode.char_expand_abbrev]] *)
  
(*s: toplevel [[Abbrevs_mode._2]] *)
let _ = 
  let chars = !!abbrevs_chars in
  for i = 0 to String.length chars - 1 do
    Keymap.add_binding mode.min_map [NormalMap, Char.code chars.[i]]
      char_expand_abbrev
  done
(*e: toplevel [[Abbrevs_mode._2]] *)

(*s: toplevel [[Abbrevs_mode._3]] *)
let _ = 
  Action.define_action "abbrevs_mode" (Minor_modes.toggle_minor mode)
(*e: toplevel [[Abbrevs_mode._3]] *)

(*e: modes/minor_modes/abbrevs_mode.ml *)
