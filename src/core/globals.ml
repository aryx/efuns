(*s: core/globals.ml *)
(*s: copyright header efuns *)
(***********************************************************************)
(*                                                                     *)
(*                             Efuns                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header efuns *)
open Efuns

(* Note that there are also globals in other modules: 
 *  - Action.actions, 
 *  - Hook.start_hooks
 *  - Top_window.in_start_macro, 
 *  - ... 
 *)

(*s: global [[Efuns.global_editor]] *)
let global_editor = ref None
(*e: global [[Efuns.global_editor]] *)
(*s: function [[Efuns.editor]] *)
let editor () =
  match !global_editor with
  | None -> failwith "no global editor defined"
  | Some x -> x
(*e: function [[Efuns.editor]] *)

(*s: function [[Efuns.with_lock]] *)
let with_lock f =
  let edt = editor () in
  Mutex.lock edt.edt_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock edt.edt_mutex) f
(*e: function [[Efuns.with_lock]] *)

(*************************************************************************)
(*      Initialization      *)
(*************************************************************************)
  
(* Les variables importantes dans le reste du programme. *)
open Options

(*s: constant [[Efuns.check]] *)
let check = ref false
(*e: constant [[Efuns.check]] *)

(*s: constants [[Efuns.debug_xxx]] *)
let debug = ref false
(* alt: use Logs src like in semgrep but ocaml-light does not support
 * functors so simpler to use those extra globals to mimic Logs src
 *)
let debug_graphics = ref false
(*e: constants [[Efuns.debug_xxx]] *)

(*s: constant [[Efuns.load_path]] *)
let load_path = define_option ["efuns_path"] 
  "<load_path> is the path where modules (.cmo and .cma) can be found
  for dynamic linking." path_option []
(*e: constant [[Efuns.load_path]] *)

(*s: constant [[Efuns.path]] *)
let path = (*Dyneval.load_path*) ref []
(*e: constant [[Efuns.path]] *)
  
(*s: constant [[Efuns.efuns_path]] *)
let efuns_path = [ 
      (Filename.concat Utils.homedir ".efuns") ;
(*
      Version.efuns_lib; 
      Version.installdir; 
      Version.ocamllib
*)
  ]
(*e: constant [[Efuns.efuns_path]] *)
  
(*s: toplevel [[Efuns]] load path *)
let _ = 
  path := !!load_path @ efuns_path;
  option_hook load_path (fun _ -> path := !!load_path @ efuns_path)
(*e: toplevel [[Efuns]] load path *)

(* used in some major mode *)
(*s: constant [[Efuns.font]] *)
let font = define_option ["font"] "" string_option "Monospace 20"
(*e: constant [[Efuns.font]] *)
  

(* for ipc/server too *)
(*s: global [[Efuns.displayname]] *)
let displayname = ref ""
(*e: global [[Efuns.displayname]] *)

(*e: core/globals.ml *)
