(*s: features/transform.ml *)
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
open Common
open Eq.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let lines str =
  let xs = Str.full_split (Str.regexp "\n") str in
  let rec aux xs =
    match xs with
    | [] -> []
    | Str.Text s1::Str.Delim s2::xs -> (s1 ^ s2)::aux xs
    | Str.Delim s1::xs -> s1::aux xs
    | [Str.Text s1] -> [s1]
    | _ -> failwith (spf "Impossible: wrong Delim/Text sequence for '%s'" str)
  in
  aux xs

let transform_marked_region frame f =
  let (buf, text, point) = Frame.buf_text_point frame in
  let mark =
    match buf.buf_mark with
      | None -> failwith "No mark set"
      | Some mark -> 
        buf.buf_mark <- None;
        mark
  in
  let (a,b) = if mark > point then (point,mark) else (mark,point) in
  let delta = Text.distance text a b in
  let region = Text.sub text a delta in
  let region' = f region in
  
  text |> Text.with_session (fun () ->
    Text.delete text a delta;
    Text.insert text a region';
    Text.remove_point text mark;
  )

(*****************************************************************************)
(* Filling *)
(*****************************************************************************)

(*s: function [[Simple.simplify]] *)
let simplify text start point =
  Text.with_dup_point text start (fun start ->
    let rec iter last_c =
      if start < point then
        let c = Text.get_char text start in
        if c =$= ' ' || c =$= '\n' || c =$= '\t' then
          ( Text.delete text start 1;
            iter ' ')
        else
        if last_c =$= ' ' then
          ( Text.insert text start " ";
            Text.fmove text start 2;
            iter 'a')
        else
          ( Text.fmove text start 1;
            iter 'a')
    in
    iter 'a'
  )
(*e: function [[Simple.simplify]] *)

(*s: function [[Simple.fill_paragraph]] *)
(* We will have to modify this to handle line_comment soon !! *)
let fill_paragraph frame =
  let (buf, text, point) = Frame.buf_text_point frame in
  text |> Text.with_session (fun () ->
    Text.with_dup_point text point (fun start ->
    Move.backward_paragraph buf start;
    Text.with_dup_point text start (fun fin ->
      Move.forward_paragraph buf fin;

      simplify text start fin;
      Text.insert text start "\n";
      let rec iter count last_space =
        if Text.compare text start fin < 0 then
        if Text.fmove_res text start 1 =|= 1 then 
          let c = Text.get_char text start in  
            if c =$= ' ' then (* good, a new space *)
              iter (count+1) 0
          else
          if count > 75 && count <> last_space then 
              begin
              Text.bmove text start (last_space+1);
              Text.delete text start 1;
              Text.insert text start "\n";
              Text.fmove text start 1;
              iter 0 0
              end
            else
              iter (count+1) (last_space+1)
      in
      iter 0 0;  
      Text.insert text fin "\n";
  )))
[@@interactive]
(*e: function [[Simple.fill_paragraph]] *)

(*****************************************************************************)
(* Aligning *)
(*****************************************************************************)

let align_char_string char str =
  let xs = lines str in
  let ys = xs |> List.map (fun s -> 
    try 
      s, String.index s char
    with Not_found -> 
      failwith (spf "Character '%c' not found in line %s" char s)
  ) in
  let max = ys |> List.map snd |> List.fold_left max 0 in
  ys |> List.map (fun (s, idx) -> 
    String.sub s 0 idx ^
    String.make (max - idx) ' ' ^
    String.sub s idx (String.length s - idx)
  ) |> String.concat ""

let align_char frame =
  Select.simple_select frame "character to align: " (fun s ->
    let c = 
      if String.length s <> 1
      then failwith "align_char expects a single character"
      else String.get s 0
    in
    transform_marked_region frame (align_char_string c);
  )
[@@interactive]

(*e: features/transform.ml *)
