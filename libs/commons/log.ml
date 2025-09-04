(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* DEPRECATED: should use Logs instead *)

(*
let lock = ref false
let discard = ref true
let list = ref []

let enable () = discard := false
let disable () = discard := true
let unlock () =
  lock := false;
  let rec iter l =
    match l with
      [] -> list := []
    | s :: tail -> print_string s; iter tail
  in
  iter (List.rev !list);
  flush stdout

let p s = 
  if !discard then () else
  if !lock then list := s :: !list 
  else print_string s; flush stdout

let lock () = lock := true
  *)

let logp = ref false
let log_name = ref ((Filename.basename Sys.argv.(0))^".log")
  
let outc = ref None  
let outc () =
  match !outc with
    None -> 
      let oc = open_out !log_name in 
      outc := Some oc; oc
  | Some outc -> outc
      
let printf f x = 
  if !logp then begin
    let oc = outc () in
    Printf.fprintf oc f x;
    flush oc
  end
    
let exn f e =
  if !logp then begin
    let oc = outc () in
    Printf.fprintf oc f (Utils.printexn e);
    flush oc
  end

(*
let catch format f =
  try f () with e -> exn format e
      
let watch format f =
  try f () with e -> exn format e; raise e
*)
