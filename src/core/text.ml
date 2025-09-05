(*s: core/text.ml *)
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

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Text.position]] *)
type position = int
(*e: type [[Text.position]] *)

(*s: type [[Text.position2]] *)
type position2 = int
(*e: type [[Text.position2]] *)

(*s: type [[Text.point]] *)
type point = {
    mutable pos : position2;
    mutable line : int;
}
(*e: type [[Text.point]] *)

(*s: type [[Text.coord]] *)
type coord = {
  c_col: int;
  c_line: int;
}
(*e: type [[Text.coord]] *)

(*s: type [[Text.delta]] *)
type delta = int
(*e: type [[Text.delta]] *)

(*s: type [[Text.attribute]] *)
type attribute = int
(*e: type [[Text.attribute]] *)


(*s: type [[Text.version]] *)
type version = int
(*e: type [[Text.version]] *)

(*s: type [[Text.session]] *)
type session = int
(*e: type [[Text.session]] *)


(*s: type [[Text.line]] *)
type line = {
    mutable position : position2; (* bol (beginning of line) *)

    (*s: [[Text.line]] representation fields *)
    mutable boxes : box list; (* sorted in reverse; head is last box on the line *)
    (*x: [[Text.line]] representation fields *)
    mutable repr_string : bytes;
    mutable repr_len : int;
    (*e: [[Text.line]] representation fields *)
    (*s: [[Text.line]] other fields *)
    mutable line_modified : bool;
    (*e: [[Text.line]] other fields *)
  } 
(*e: type [[Text.line]] *)

(*s: type [[Text.box]] *)
and box = 
  { 
    box_col : int;        (* col of box *)
    box_len : int;        (* len of box in Text.t string *)
    mutable box_attr : int;    (* common attribute *)

    (*s: [[Text.box]] other fields *)
    box_charsize : int; (* common size *)
    box_size : int;
    (*x: [[Text.box]] other fields *)
    box_pos_repr : int;  (* pos of box in representation string *)
    (*e: [[Text.box]] other fields *)
  } 
(*e: type [[Text.box]] *)

(*s: type [[Text.t]] *)
type t = {
    mutable text_string : bytes;
    mutable text_size : int; (* Bytes.length text.text_string *)

    mutable text_newlines : line array;
    mutable text_nlines : int; (* Array.length text.text_newlines *)

    (*s: [[Text.t]] gap fields *)
    (* g for gap *)
    mutable gpoint : point;
    mutable gsize : int;
    (*e: [[Text.t]] gap fields *)
    (*s: [[Text.t]] history fields *)
    mutable text_modified : version;
    (*x: [[Text.t]] history fields *)
    mutable text_history : action list;
    (*e: [[Text.t]] history fields *)
    (*s: [[Text.t]] attribute fields *)
    mutable text_attrs : attribute array;
    (*e: [[Text.t]] attribute fields *)
    (*s: [[Text.t]] other fields *)
    mutable text_points : point list;
    (*x: [[Text.t]] other fields *)
    mutable text_clean : bool;
    (*x: [[Text.t]] other fields *)
    mutable text_readonly : bool;
    (*e: [[Text.t]] other fields *)
  } 
(*e: type [[Text.t]] *)
  
(*s: type [[Text.action]] *)
and action =
  Insertion of position * int * int
| Deletion of position * string * int
(*s: [[Text.action]] other cases *)
| Session of action list
(*e: [[Text.action]] other cases *)
(*e: type [[Text.action]] *)

(*****************************************************************************)
(* Global properties *)
(*****************************************************************************)
  
(*s: function [[Text.version]] *)
let version text = 
  text.text_modified
(*e: function [[Text.version]] *)
  
(*s: function [[Text.nbr_lines]] *)
let nbr_lines text = 
  text.text_nlines - 2
(*e: function [[Text.nbr_lines]] *)
  
(*s: function [[Text.size]] *)
let size text = 
  text.text_size - text.gsize
(*e: function [[Text.size]] *)

(*****************************************************************************)
(* Line x Col *)
(*****************************************************************************)
  
(*s: function [[Text.point_col]] *)
let point_col text point = 
  let pos = point.pos in
  let gpos = text.gpoint.pos in
  let bol = text.text_newlines.(point.line).position in
  (* gap handling *)
  if bol <= gpos && gpos < pos
  then pos - bol - text.gsize
  else pos - bol
(*e: function [[Text.point_col]] *)

(*s: function [[Text.find_line_of_pos]] *)
let find_line_of_pos text pos =
  let gpos = text.gpoint.pos in
  let gline = text.gpoint.line in
  let gap_end = gpos + text.gsize in

  if pos >= gap_end then
    (* go forward *)
    let rec iter line =
      if line >= text.text_nlines 
      then text.text_nlines - 1
      else
        if text.text_newlines.(line).position > pos 
        then line - 1
        else iter (line + 1)
    in
    iter (gline+1) 
  else
     (* go backward *)
     let rec iter line =
       if line > 0 
       then
         if text.text_newlines.(line).position > pos 
         then iter (line - 1)
         else line
      else 0
     in
     iter gline
[@@profiling]
(*e: function [[Text.find_line_of_pos]] *)

(*s: function [[Text.point_line]] *)
let point_line text point = 
  (* defensive: *)
  if (point.line <> find_line_of_pos text point.pos)
  then Logs.err (fun m -> m "TODO: point.line = %d != find_line ... = %d"
                     point.line (find_line_of_pos text point.pos));
  point.line
(*e: function [[Text.point_line]] *)

(*s: function [[Text.point_coord]] *)
let point_coord text point =
  { c_col = point_col text point;
    c_line = point_line text point;
  }
(*e: function [[Text.point_coord]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Text.mk_line_with_pos]] *)
let mk_line_with_pos pos = 
  {
   position = pos; 

   boxes = []; 
   repr_string = Bytes.of_string ""; 
   repr_len = 0; 
   line_modified = true; 
  }
(*e: function [[Text.mk_line_with_pos]] *)

(*s: function [[Text.cancel_repr]] *)
let cancel_repr text _point n =
  let line = text.text_newlines.(n) in
  line.line_modified <- true
(*e: function [[Text.cancel_repr]] *)

(*****************************************************************************)
(* Attributes *)
(*****************************************************************************)

(*s: constant [[Text.highlight_bit]] *)
let highlight_bit = 1 lsl 24
(*e: constant [[Text.highlight_bit]] *)

(*s: function [[Text.make_attr]] *)
let make_attr fg bg font highlighted =
  let attr = 
     fg + 
     (bg lsl 8) + 
     (font lsl 16) + 
     (if highlighted then highlight_bit else 0)
  in
  attr
(*e: function [[Text.make_attr]] *)

(*s: constant [[Text.direct_attr]] *)
let direct_attr =  make_attr 0 1 0 false
(*e: constant [[Text.direct_attr]] *)
(*s: constant [[Text.inverse_attr]] *)
let inverse_attr =  make_attr 1 0 0 false
(*e: constant [[Text.inverse_attr]] *)

(*s: function [[Text.unset_attrs]] *)
let unset_attrs text =
  Array.fill text.text_attrs 0 (Array.length text.text_attrs) direct_attr;
  text.text_newlines |> Array.iter (fun line -> line.line_modified <- true)
(*e: function [[Text.unset_attrs]] *)
(*s: function [[Text.set_attrs]] *)
let set_attrs text point len attr = (* should not exceed one line *)
  if len > 0 then
    let gap_end = text.gpoint.pos + text.gsize in

    cancel_repr text point.pos point.line;

    let pos = point.pos in
    let gpos = text.gpoint.pos in
    let before, after, after_pos =
      if pos > gap_end then
        0, (min (text.text_size - pos) len), pos
      else
      if pos + len <= gpos then
        0, len, pos
      else
      let before = gpos - pos in
      let after = min (len - before) (text.text_size - gap_end) in
      before, after, gap_end
    in
    if before > 0 
    then Array.fill text.text_attrs pos before attr;
    Array.fill text.text_attrs after_pos after attr
(*e: function [[Text.set_attrs]] *)

(*s: function [[Text.get_attr]] *)
let get_attr text point =
  let pos = 
    (* gap handling *)
    if point.pos = text.gpoint.pos
    then point.pos + text.gsize 
    else point.pos
  in
  if pos < text.text_size 
  then text.text_attrs.(pos)
  else direct_attr
(*e: function [[Text.get_attr]] *)
(*s: function [[Text.set_attr]] *)
let set_attr text point attr =
  let y = point.line in
  let pos = 
    (* gap handling *)
    if point.pos = text.gpoint.pos
    then point.pos + text.gsize 
    else point.pos
  in
  if pos < text.text_size then begin
    cancel_repr text pos y;
    text.text_attrs.(pos) <- attr
  end
(*e: function [[Text.set_attr]] *)

(*****************************************************************************)
(* Gap *)
(*****************************************************************************)

(*s: function [[Text.move_gpoint_to]] *)
let move_gpoint_to text pos =
  let gpos = text.gpoint.pos in
  let gsize = text.gsize in
  let gline = text.gpoint.line in
  let gap_end = gpos + gsize in

  (*s: [[Text.move_gpoint_to()]] reset [[text_clean]] *)
  text.text_clean <- false;
  (*e: [[Text.move_gpoint_to()]] reset [[text_clean]] *)
  if pos <> gpos then
    if pos < gpos then begin
      (*s: [[Text.move_gpoint_to()]] when pos is before gpos *)
      let delta = gpos - pos in
      let (delta_line,_) = 
        Utils.count_char_sub (Bytes.to_string text.text_string) pos delta '\n'
      in
      Bytes.blit text.text_string pos   text.text_string (pos + gsize)   delta;
      Array.blit text.text_attrs  pos   text.text_attrs  (pos + gsize)   delta;
      for i = gline - delta_line + 1 to gline do
        text.text_newlines.(i).position <- text.text_newlines.(i).position + gsize 
      done;
      text.text_points |> List.iter (fun p -> 
        if p.pos > pos && p.pos <= gpos 
        then p.pos <- p.pos + gsize
      );
      text.gpoint <- { pos = pos; line = gline - delta_line };
      (*e: [[Text.move_gpoint_to()]] when pos is before gpos *)
    end else begin
      (*s: [[Text.move_gpoint_to()]] when pos is after gpos *)
      let delta = pos - gap_end in
      let (delta_line,_) = 
        Utils.count_char_sub (Bytes.to_string text.text_string) gap_end delta '\n' in
      Bytes.blit text.text_string gap_end   text.text_string gpos   delta;
      Array.blit text.text_attrs  gap_end   text.text_attrs  gpos   delta;
      for i = gline + 1 to gline + delta_line do
        text.text_newlines.(i).position <- text.text_newlines.(i).position - gsize
      done;
      text.text_points |> List.iter (fun p -> 
          if p.pos >= gap_end && p.pos <= pos 
          then p.pos <- p.pos - gsize
      );
      text.gpoint <- { pos = pos - gsize; line = gline + delta_line }
      (*e: [[Text.move_gpoint_to()]] when pos is after gpos *)
      end
[@@profiling]
(*e: function [[Text.move_gpoint_to]] *)

(*s: constant [[Text.add_amount]] *)
let add_amount = define_option ["add_amount"] "Size of the gap in the buffer"
  int_option 200
(*e: constant [[Text.add_amount]] *)
(*s: function [[Text.extend_gap]] *)
let extend_gap text amount =
  let add_size = max !!add_amount 
      ((amount / !!add_amount) * !!add_amount + !!add_amount) in
  let old_size = text.text_size in
  let gsize = text.gsize in

  let gpos = text.gpoint.pos in
  let gap_end = gpos + gsize in

  let new_text = Bytes.create (old_size + add_size) in
  Bytes.blit  text.text_string 0   new_text 0    gpos; 
  Bytes.blit  text.text_string gap_end   new_text (gap_end + add_size) 
    (old_size - gap_end);
  text.text_string <- new_text;

  let new_attrs = Array.make (old_size + add_size) direct_attr in
  Array.blit   text.text_attrs 0   new_attrs 0   gpos; 
  Array.blit   text.text_attrs gap_end    new_attrs (gap_end + add_size) 
    (old_size - gap_end);
  text.text_attrs <- new_attrs;

  for i = text.gpoint.line + 1 to text.text_nlines - 1 do
    text.text_newlines.(i).position <- 
      text.text_newlines.(i).position + add_size
  done;
  text.text_points |> List.iter (fun p -> 
      if p.pos > gpos 
      then p.pos <- p.pos + add_size
  );
  text.gsize <- gsize + add_size;
  text.text_size <- old_size + add_size;
  ()
[@@profiling]
(*e: function [[Text.extend_gap]] *)

(*****************************************************************************)
(* Low level insert/delete *)
(*****************************************************************************)

(*s: function [[Text.low_insert]] *)
let low_insert text pos str =
  let strlen = String.length str in
  (*s: [[Text.low_insert()]] fail if readonly buffer *)
  if text.text_readonly 
  then failwith "Buffer is read-only";
  (*e: [[Text.low_insert()]] fail if readonly buffer *)

  (*s: [[Text.low_insert()]] move gap to point *)
  move_gpoint_to text pos;
  (*e: [[Text.low_insert()]] move gap to point *)
  (* subtle: don't move those 'let' earlier, because moving the gap do side
   * effects on the position and line of the gpoint. 
   *)
  let gpos = text.gpoint.pos in
  let gline = text.gpoint.line in

  cancel_repr text gpos gline;
  (*s: [[Text.low_insert()]] extend gap if not enough space *)
  if strlen > text.gsize 
  then extend_gap text strlen;
  (*e: [[Text.low_insert()]] extend gap if not enough space *)
  (* all points should have their point.pos correctly adjusted *)

  String.blit  str 0   text.text_string gpos    strlen;
  (* todo: should refontify! *)
  Array.fill text.text_attrs gpos strlen direct_attr;

  let (nbr_newlines, _nbr_chars) = Utils.count_char str '\n' in
  if nbr_newlines > 0 then 
  begin
    (*s: [[Text.low_insert()]] adjust newlines when str contains newlines *)
    if (Array.length text.text_newlines - text.text_nlines) < nbr_newlines then
      begin
        (*s: [[Text.low_insert()]] grow newlines *)
        let old_size = text.text_nlines in
        let new_cache = Array.make (old_size + (max 20 nbr_newlines)) 
          { (mk_line_with_pos (-1)) with line_modified = false }
        in
        Array.blit 
          text.text_newlines 0 
          new_cache 0 
          old_size;
        text.text_newlines <- new_cache;
        (*e: [[Text.low_insert()]] grow newlines *)
      end;

    Array.blit
      text.text_newlines (gline+1) 
      text.text_newlines (gline+1+ nbr_newlines) 
     (text.text_nlines - gline -1);
    text.text_nlines <- text.text_nlines + nbr_newlines;

    (* similar to compute_newlines() but just for lines after gpos *)
    let rec iter n pos =
      let new_pos = 
       Bytes.index_from text.text_string pos '\n' in
      text.text_newlines.(gline+n) <- mk_line_with_pos (new_pos + 1);
      if n < nbr_newlines 
      then iter (n+1) (new_pos + 1)
    in
    iter 1 gpos;
    (*e: [[Text.low_insert()]] adjust newlines when str contains newlines *)
  end;
  let gline = text.gpoint.line in
  text.text_points |> List.iter (fun p ->
    if p.pos > gpos 
      (* bugfix: there was extra condition line = gline. But bug! unit test *)
    then p.line <- p.line + nbr_newlines;

  );
  text.gpoint <- { pos = gpos + strlen; line = gline + nbr_newlines };
  text.gsize <- text.gsize - strlen;
  (gpos, strlen, text.text_modified) 
(*e: function [[Text.low_insert]] *)

(*s: function [[Text.low_delete]] *)
let low_delete text pos len =
  (*s: [[Text.low_insert()]] fail if readonly buffer *)
  if text.text_readonly 
  then failwith "Buffer is read-only";
  (*e: [[Text.low_insert()]] fail if readonly buffer *)

  (*s: [[Text.low_insert()]] move gap to point *)
  move_gpoint_to text pos;
  (*e: [[Text.low_insert()]] move gap to point *)
  (* subtle: don't move those 'let' earlier, because moving the gap do side
   * effects on the position and line of the gpoint. 
   *)
  let gpos = text.gpoint.pos in
  let gline = text.gpoint.line in

  cancel_repr text gpos gline;

  let gap_end = gpos + text.gsize in
  let len = min (text.text_size - gap_end) len in

  let str = Bytes.sub_string text.text_string gap_end len in

  let (nbr_newlines, _nbr_chars) = Utils.count_char str '\n' in
  if nbr_newlines > 0 then begin
    (*s: [[Text.low_delete()]] adjust newlines when str contained newlines *)
    Array.blit 
      text.text_newlines (gline + nbr_newlines + 1)
      text.text_newlines (gline + 1) 
     (text.text_nlines - gline - nbr_newlines - 1);
    text.text_nlines <- text.text_nlines - nbr_newlines;
    (*e: [[Text.low_delete()]] adjust newlines when str contained newlines *)
  end;
  text.text_points |> List.iter (fun p -> 
      if p.pos > gap_end + len 
      then p.line <- p.line - nbr_newlines
      else
        (* points that were in deleted region *)
        if p.pos > gpos then begin
           p.pos <- gpos;
           p.line <- gline
         end
  );
  text.gsize <- text.gsize + len;
  (gpos, str, text.text_modified) 
(*e: function [[Text.low_delete]] *)

(*****************************************************************************)
(* Undo *)
(*****************************************************************************)

(*s: function [[Text.undo]] *)
let undo text =
  let rec undo action =
    let gpos = text.gpoint.pos in
    let gsize = text.gsize in
    match action with
    | Insertion(point_pos, len, modified) ->
        let point = if point_pos > gpos then point_pos + gsize else point_pos in
        let (pos,str,modif) = low_delete text point len in
        text.text_modified <- modified;
        Deletion(pos,str,modif), point_pos, 0
    | Deletion (point_pos, str, modified) ->
        let point = if point_pos > gpos then point_pos + gsize else point_pos in
        let (pos,len,modif) = low_insert text point str in
        text.text_modified <- modified;
        Insertion(pos,len,modif), point_pos, String.length str
    (*s: [[Text.undo()]] match action cases *)
    | Session actions ->
        let last_point = ref 0 in
        let last_len = ref 0 in
        let rev_actions =
          List.fold_left (fun undos action -> 
              let (rev_action,point,len) = undo action in
              last_point := point;
              last_len := len;
              rev_action :: undos
          ) [] actions
        in
        Session rev_actions, !last_point, !last_len
    (*e: [[Text.undo()]] match action cases *)
  in
  match text.text_history with
    [] -> raise Not_found
  | action :: tail -> 
      let rev_action = undo action in
      text.text_history <- tail;
      rev_action
(*e: function [[Text.undo]] *)

(*s: function [[Text.start_session]] *)
let start_session text =   
  text.text_modified
(*e: function [[Text.start_session]] *)
  
(*s: function [[Text.commit_session]] *)
let commit_session text session_date =
  if text.text_modified > session_date then
    let rec iter session history =
      match history with
        [] -> assert false
      | action :: history ->
          let date =
            match action with
              Session _ -> failwith "Can not commit nested sessions"
            | Insertion (_,_,date) -> date
            | Deletion (_,_,date) -> date
          in
          if date = session_date then
            text.text_history <- (Session (List.rev (action::session)))
            :: history
          else
            iter (action::session) history
    in
    iter [] text.text_history
(*e: function [[Text.commit_session]] *)

(*s: function [[Text.with_session]] *)
let with_session f text =
  let session = start_session text in
  let res = f () in
  commit_session text session;
  res
(*e: function [[Text.with_session]] *)


(*****************************************************************************)
(* Insert/delete public API *)
(*****************************************************************************)

(*s: function [[Text.insert_res]] *)
let insert_res text point str =
  let (pos,len,modif) = low_insert text point.pos str in
  text.text_history <- Insertion(pos,len,modif) :: text.text_history;
  text.text_modified <- text.text_modified + 1;
  pos, len
(*e: function [[Text.insert_res]] *)

(*s: function [[Text.insert]] *)
let insert text point str = 
   insert_res text point str |> ignore
(*e: function [[Text.insert]] *)
  
(*s: function [[Text.delete_res]] *)
let delete_res text point len =
  let (pos,str,modif) = low_delete text point.pos len in
  text.text_history <- Deletion(pos,str,modif) :: text.text_history;
  text.text_modified <- text.text_modified + 1;
  pos, str
(*e: function [[Text.delete_res]] *)

(*s: function [[Text.delete]] *)
let delete text point len = 
  delete_res text point len |> ignore
(*e: function [[Text.delete]] *)

(*s: function [[Text.insert_at_end]] *)
let insert_at_end text str =
  low_insert text text.text_size str |> ignore;
  text.text_history <- [];
  text.text_modified <- text.text_modified + 1
(*e: function [[Text.insert_at_end]] *)


(*****************************************************************************)
(* Constructor *)
(*****************************************************************************)
  
(*s: function [[Text.compute_newlines]] *)
let compute_newlines string =
  let (nbr_newlines,_) = Utils.count_char string '\n' in
  let newlines = Array.make (nbr_newlines + 2) (mk_line_with_pos 0) in
  let curs = ref 0 in
  (* newlines.(0) is already set with 0 position for its bol *)
  for i = 1 to nbr_newlines do
    let pos = String.index_from string !curs '\n' in
    newlines.(i) <- mk_line_with_pos (pos+1);
    curs := pos + 1;
  done;
  newlines.(nbr_newlines+1) <- mk_line_with_pos (String.length string + 1);
  newlines
(*e: function [[Text.compute_newlines]] *)

(*s: function [[Text.create]] *)
let create str =
  let newlines = compute_newlines str in
  let attrs = (Array.make (String.length str) direct_attr) in

    {
      text_string = (Bytes.of_string str);
      text_size = String.length str;

      text_newlines = newlines;
      text_nlines = Array.length newlines;

      text_attrs = attrs;

      (* no gap at the beginning *)
      gpoint = { pos = 0; line = 0 };
      gsize = 0;
      text_points = [];

      text_modified = 0;
      text_clean = false;
      text_history = [];
      text_readonly = false;
    }
(*e: function [[Text.create]] *)
  
(*****************************************************************************)
(* Points *)
(*****************************************************************************)

(*s: function [[Text.new_point]] *)
let new_point text =
  let p = { pos = 0; line = 0; } in    
  text.text_points <- p :: text.text_points;
  p
(*e: function [[Text.new_point]] *)

(*s: function [[Text.dup_point]] *)
let dup_point text point =
  let p = { pos = point.pos; line = point.line } in
  text.text_points <- p :: text.text_points;
  p
(*e: function [[Text.dup_point]] *)

(*s: function [[Text.goto_point]] *)
let goto_point _text p1 p2 =
  (* less: could assert they references the same text *)
  p1.pos <- p2.pos;
  p1.line <- p2.line
(*e: function [[Text.goto_point]] *)

(*s: function [[Text.move_point_to]] *)
let move_point_to_pos text point pos =
  point.pos <- pos;
  point.line <- find_line_of_pos text pos
(*e: function [[Text.move_point_to]] *)

(*s: function [[Text.remove_point]] *)
let remove_point text p =
  text.text_points <- 
    text.text_points |> List.fold_left (fun points point ->
        if point == p 
        then points 
        else point :: points
    ) [];
  (* being defensive *)
  p.pos <- -1;
  p.line <- -1;
  ()
(*e: function [[Text.remove_point]] *)

(*s: function [[Text.with_dup_point]] *)
let with_dup_point text point f =
  let p = dup_point text point in
  Fun.protect ~finally:(fun () -> remove_point text p) (fun () -> f p)
(*e: function [[Text.with_dup_point]] *)

(*s: function [[Text.with_new_point]] *)
let with_new_point text f =
  let p = new_point text in
  Fun.protect ~finally:(fun () -> remove_point text p) (fun () -> f p) 
(*e: function [[Text.with_new_point]] *)


(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(*s: function [[Text.read]] *)
let read inc =
  create (Utils.read_string inc)
(*e: function [[Text.read]] *)

(*s: function [[Text.save]] *)
let save text outc =
  let str = text.text_string in
  let gpos = text.gpoint.pos in
  let gsize = text.gsize in
  output outc str 0 gpos;
  (* skipping the gap *)
  output outc str (gpos + gsize) (text.text_size - gpos - gsize)
(*e: function [[Text.save]] *)

(*****************************************************************************)
(* Distance, delta *)
(*****************************************************************************)

(*s: function [[Text.low_distance]] *)
let low_distance text p1 p2 =
  if p1 >= p2 
  then 0 
  else
    if p1 <= text.gpoint.pos 
    then
      if p2 <= text.gpoint.pos 
      then p2 - p1
      else p2 - p1 - text.gsize
    else
      if p2 <= text.gpoint.pos 
      then p2 - p1 + text.gsize
      else p2 - p1
(*e: function [[Text.low_distance]] *)

(*s: function [[Text.distance]] *)
let distance text p1 p2 =
  low_distance text p1.pos p2.pos
(*e: function [[Text.distance]] *)

(*s: function [[Text.compare]] *)
let compare _text p1 p2 = 
  compare p1.pos p2.pos
(*e: function [[Text.compare]] *)

(*****************************************************************************)
(* Text Getters/setters *)
(*****************************************************************************)
  
(*s: function [[Text.get_char]] *)
let get_char text point =
  let pos = point.pos in
  let gpos = text.gpoint.pos in
  let pos = 
    if pos = gpos 
    then pos + text.gsize 
    else pos
  in
  if pos < text.text_size 
  then Bytes.get text.text_string pos
  else '\000'
(*e: function [[Text.get_char]] *)

(*****************************************************************************)
(* Moving *)
(*****************************************************************************)

(*s: function [[Text.fmove_res]] *)
let fmove_res text p delta =
  if delta = 0 then 0 else
  let gpos = text.gpoint.pos in
  let size = text.text_size in
  let gap_end = gpos + text.gsize in
  let gline = text.gpoint.line in
  let pos = p.pos in
  let old_pos = pos in
  let lines = text.text_newlines in
  let rec iter y point end_point =
    if end_point > point 
    then
      let end_line = lines.(y+1).position in
      if end_point >= end_line 
      then iter (y+1) end_line end_point
      else (y, end_point)
    else (y,point)
  in
  let (y,pos) = 
    if pos + delta <= gpos 
    then iter p.line pos (pos+delta)
    else
      if pos >= gap_end 
      then
        let delta = min delta (size - pos) in
        iter p.line pos (pos + delta)
      else
        let delta = min (delta - (gpos - pos)) (size - gap_end) in
        iter gline gap_end (gap_end + delta) 
  in
  p.pos <- pos;
  p.line <- y;
  low_distance text old_pos pos
(*e: function [[Text.fmove_res]] *)

(*s: function [[Text.bmove_res]] *)
let bmove_res text p delta =
  if delta = 0 then 0 else
  let gpos = text.gpoint.pos in
  let gap_end = gpos + text.gsize in
  let gline = text.gpoint.line in
  let pos = p.pos in
  let old_pos = pos in
  let lines = text.text_newlines in
  let rec iter y end_point =
    let start_line = lines.(y).position in
    if end_point >= start_line then
      (y, end_point)
    else
      iter (y-1) end_point
  in
  let (y,pos) = 
    if pos - delta >= gap_end then
      if pos - delta < lines.(gline+1).position then
        (
          gline, pos - delta)
      else
        iter p.line (pos-delta)
    else
    if pos <= gpos then
      let delta = min delta pos in
      iter p.line (pos - delta)
    else
    let delta = min (delta - (pos - gap_end)) gpos in
    iter gline (gpos - delta) 
  in
  p.pos <- pos;
  p.line <- y;
  low_distance text pos old_pos
(*e: function [[Text.bmove_res]] *)

(*s: function [[Text.bmove]] *)
let bmove text p delta = 
  bmove_res text p delta |> ignore
(*e: function [[Text.bmove]] *)

(*s: function [[Text.fmove]] *)
let fmove text p delta = 
  fmove_res text p delta |> ignore
(*e: function [[Text.fmove]] *)


(*s: function [[Text.move_res]] *)
let move_res text point n =
  if n > 0 
  then fmove_res text point n
  else bmove_res text point (-n)
(*e: function [[Text.move_res]] *)

(*s: function [[Text.move]] *)
let move text point n = 
  move_res text point n |> ignore
(*e: function [[Text.move]] *)

(*****************************************************************************)
(* Position *)
(*****************************************************************************)
  
(*s: function [[Text.get_position]] *)
let get_position text point = 
  (* gap handling *)
  if point.pos > text.gpoint.pos 
  then point.pos - text.gsize
  else point.pos
(*e: function [[Text.get_position]] *)

(*s: function [[Text.set_position]] *)
let set_position text point pos =
  move_point_to_pos text point
    (if pos > text.gpoint.pos 
     then pos + text.gsize
     else pos
     )
(*e: function [[Text.set_position]] *)

(*s: function [[Text.goto_line]] *)
let goto_line text point y =
  if text.text_nlines - 1 <= y 
  then set_position text point (size text)
  else begin
    let line = text.text_newlines.(y) in
    point.pos <- line.position;
    point.line <- y
  end
(*e: function [[Text.goto_line]] *)

(*****************************************************************************)
(* Sub content  *)
(*****************************************************************************)

let iter text cursor delta f =
  with_dup_point text cursor (fun cursor ->
    for _i = 0 to delta - 1 do
      f cursor;
      fmove text cursor 1
    done
  )

(*
let iter buf debut fin =
  let text = buf.buf_text in
  Text.with_new_point text (fun curseur ->
  Text.with_new_point text (fun final ->
    Text.set_position text curseur debut;
    Text.set_position text final fin;
    while curseur < final do
      let attr = Text.get_attr text curseur in
      Text.set_attr text curseur (attr lor Text.highlight_bit);
      Text.fmove text curseur 1
    done;
*)


(* should be in search/replace, but it's used by blit too *)
(*s: function [[Text.clean_text]] *)
let clean_text text =
  if not text.text_clean then begin
    let size = text.text_size in
    let gsize = text.gsize in
    let string = text.text_string in
    move_gpoint_to text size;
    Bytes.fill string (size - gsize) gsize '\000';
    text.text_clean <- true
  end
(*e: function [[Text.clean_text]] *)
    

(*s: function [[Text.blit]] *)
let blit str text point len =
  let pos = point.pos in
  let len = min len (low_distance text pos text.text_size) in
  let gpos = text.gpoint.pos in
  let gap_end = gpos + text.gsize in
  if pos+len >= gpos && pos < gap_end 
  then clean_text text;
  (try
     Bytes.blit text.text_string pos str 0 len
   with e -> raise e
   );
  len
(*e: function [[Text.blit]] *)
    
(*s: function [[Text.sub]] *)
let sub text point len =
  let str = Bytes.create len in
  blit str text point len |> ignore;
  Bytes.to_string str
(*e: function [[Text.sub]] *)

(*s: function [[Text.region]] *)
let rec region text p1 p2 =
  if p1>p2 
  then region text p2 p1
  else sub text p1 (distance text p1 p2)
(*e: function [[Text.region]] *)

(*****************************************************************************)
(* Search/replace  *)
(*****************************************************************************)

(*s: function [[Text.search_forward]] *)
let search_forward text regexp point =
  let gsize = text.gsize in
  let gap_end = text.gpoint.pos + gsize in

  if point.pos = text.gpoint.pos 
  then point.pos <- gap_end;
  if point.pos < gap_end 
  then clean_text text;

  let gap_end = text.gpoint.pos + gsize in  
  let string = Bytes.to_string text.text_string in
  let pos = Str.search_forward regexp string point.pos in
  let pos = if pos >= gap_end then pos - gsize else pos in
  set_position text point pos;
  Str.match_end () - Str.match_beginning ()
(*e: function [[Text.search_forward]] *)

(*s: function [[Text.replace_matched]] *)
let replace_matched text repl =
  Str2.replace_matched repl text.text_string
(*e: function [[Text.replace_matched]] *)
  
(*s: function [[Text.search_forward_matched]] *)
let search_forward_matched text regexp point =
  let gsize = text.gsize in
  let gap_end = text.gpoint.pos + gsize in

  if point.pos = text.gpoint.pos 
  then point.pos <- gap_end;
  if point.pos < gap_end 
  then clean_text text;

  let string = Bytes.to_string text.text_string in
  let pos = Str.search_forward regexp string point.pos in
  let pos = 
    if pos >= text.gpoint.pos + gsize 
    then pos - gsize 
    else pos 
  in
  set_position text point pos;
  Str.matched_string string
(*e: function [[Text.search_forward_matched]] *)

(*s: function [[Text.search_forward_groups]] *)
let search_forward_groups text regexp point groups =
  let gsize = text.gsize in
  let gap_end = text.gpoint.pos + gsize in

  if point.pos = text.gpoint.pos 
  then point.pos <- gap_end;
  if point.pos < gap_end 
  then clean_text text;

  let gap_end = text.gpoint.pos + gsize in  
  let string = Bytes.to_string text.text_string in
  let pos = Str.search_forward regexp string point.pos in
  let pos = if pos >= gap_end then pos - gsize else pos in
  let array = Array.init groups (fun i -> Str.matched_group (i+1) string) in
  set_position text point pos;
  array
(*e: function [[Text.search_forward_groups]] *)

(*s: function [[Text.search_backward]] *)
let search_backward text regexp point =
  if point.pos > text.gpoint.pos 
  then clean_text text;
  let string = Bytes.to_string text.text_string in
  let start_pos =     
    if point.pos > 0 
    then point.pos - 1 
    else raise Not_found 
  in
  let pos =  Str.search_backward regexp string start_pos in
  set_position text point pos;
  Str.match_end () - Str.match_beginning ()
(*e: function [[Text.search_backward]] *)

(*s: function [[Text.search_backward_groups]] *)
let search_backward_groups text regexp point groups =  
  if point.pos > text.gpoint.pos 
  then clean_text text;
  let string = Bytes.to_string text.text_string in
  let start_pos =     
    if point.pos > 0 
    then point.pos - 1 
    else raise Not_found  
  in
  let pos =  Str.search_backward regexp string start_pos in
  let array = Array.init groups (fun i -> Str.matched_group (i+1) string) in
  set_position text point pos;
  array
(*e: function [[Text.search_backward_groups]] *)

(*****************************************************************************)
(* Representation  *)
(*****************************************************************************)

(*s: constant [[Text.repr_string]] *)
let repr_string = ref (Bytes.of_string "")
(*e: constant [[Text.repr_string]] *)

(*s: constant [[Text.repr_size]] *)
let repr_len = ref 0
(*e: constant [[Text.repr_size]] *)

(*s: constant [[Text.dummy_line]] *)
let (dummy_line : line) = 
  {
    position = max_int;
    boxes = [];
    repr_len = 0;
    repr_string = Bytes.of_string "";
    line_modified = true;
  } 
(*e: constant [[Text.dummy_line]] *)
  
(*s: constant [[Text.tabreprs]] *)
let tabreprs = [|
    "         ";
    "        ";
    "       ";
    "      ";
    "     ";
    "    ";
    "   ";
    "  ";
    " ";
    ""
  |]
(*e: constant [[Text.tabreprs]] *)

(*s: type [[Text.charreprs]] *)
type charreprs = string array
(*e: type [[Text.charreprs]] *)

(*s: function [[Text.compute_representation]] *)
(* On devrait reprendre la representation la ou elle est ... *)
let compute_representation text charreprs n =
  (*s: [[Text.compute_representation]] if n is too big, return dummy line *)
  if n >= text.text_nlines - 1 then begin
      dummy_line.position <- text.text_size;
      dummy_line
  end
  (*e: [[Text.compute_representation]] if n is too big, return dummy line *)
  else
    let line = text.text_newlines.(n) in
    if line.line_modified then begin

      repr_string := line.repr_string;
      repr_len    := Bytes.length line.repr_string;
      let boxes = ref [] in

      let text_cursor = ref line.position in
      let repr_cursor = ref 0 in

      let end_pos = text.text_newlines.(n+1).position - 1 in

      (*s: [[Text.compute_representation()]] locals *)
      let box_col_start = ref 0 in
      let repr_start = ref 0 in

      let char_repr = ref "" in
      let char_size = ref 0 in
      (*x: [[Text.compute_representation()]] locals *)
      let gpos = text.gpoint.pos in
      let gsize = text.gsize in
      (*e: [[Text.compute_representation()]] locals *)
      
      (*s: [[Text.compute_representation()]] adjust [[text_cursor]] if in gap *)
      if !text_cursor >= gpos && !text_cursor < gpos + gsize 
      then text_cursor := !text_cursor + gsize;
      (*e: [[Text.compute_representation()]] adjust [[text_cursor]] if in gap *)
      while !text_cursor < end_pos do
        (*s: [[Text.compute_representation()]] loop [[text_cursor]] to [[end_pos]] *)
        let charcode = Char.code (Bytes.get text.text_string !text_cursor) in
        let charattr = text.text_attrs.(!text_cursor) in
        let charrepr =
          (*s: [[Text.compute_representation()]] compute charrepr, special char *)
          if charcode = 9 
          then tabreprs.(!repr_cursor mod 9)
          (*e: [[Text.compute_representation()]] compute charrepr, special char *)
          else charreprs.(charcode) 
        in
        let charsize = String.length charrepr in

        let box_len = ref 0 in

        while !text_cursor < end_pos && 
          begin
            let char_code = Char.code (Bytes.get text.text_string !text_cursor)
            in
            let char_attr = text.text_attrs.(!text_cursor) in
            char_repr := 
               (*s: [[Text.compute_representation()]] compute [[char_repr]], special char *)
               if char_code = 9 
               then tabreprs.(!repr_cursor mod 9)
               (*e: [[Text.compute_representation()]] compute [[char_repr]], special char *)
               else charreprs.(char_code);
            char_size := String.length !char_repr;

            !char_size = charsize && char_attr = charattr
          end
        do
          (*s: [[Text.compute_representation()]] grow [[repr_string]] if needed *)
          if !repr_cursor + charsize >= !repr_len then begin
              (* find a better heuristic to realloc the line string *)
              let new_len = !repr_len + 
                  (low_distance text end_pos !text_cursor) + charsize * 2 
              in
              let new_repr = Bytes.create new_len in
              Bytes.blit !repr_string 0 new_repr 0 !repr_cursor;
              repr_string := new_repr;
              repr_len := new_len;
          end;
          (*e: [[Text.compute_representation()]] grow [[repr_string]] if needed *)
          String.blit !char_repr 0 !repr_string !repr_cursor charsize;
          repr_cursor := !repr_cursor + charsize;
          text_cursor := !text_cursor + 1;
          box_len := !box_len +1;
          (*s: [[Text.compute_representation()]] adjust [[text_cursor]] if reach gap *)
          if !text_cursor = gpos 
          then text_cursor := gpos + gsize;
          (*e: [[Text.compute_representation()]] adjust [[text_cursor]] if reach gap *)
        done;

        let box = {
          box_col = !box_col_start;
          box_len = !box_len;
          box_attr = charattr;

          box_charsize = charsize;
          box_size = !box_len * charsize;
          box_pos_repr = !repr_start;
        } in
        repr_start := !repr_cursor;
        box_col_start := !box_col_start + !box_len;
        boxes := box :: !boxes;
        (*e: [[Text.compute_representation()]] loop [[text_cursor]] to [[end_pos]] *)
      done;
      (*s: [[Text.compute_representation()]] adjust line fields after loop *)
      line.boxes <- !boxes;
      line.repr_string <- !repr_string;
      line.repr_len <- !repr_cursor;
      line.line_modified <- false;
      (*e: [[Text.compute_representation()]] adjust line fields after loop *)
    end;
    line
[@@profiling]
(*e: function [[Text.compute_representation]] *)

(*****************************************************************************)
(* Point_to_xxx  *)
(*****************************************************************************)

(*s: function [[Text.point_to_eol]] *)
let point_to_eol text point =
  low_distance text point.pos
    (text.text_newlines.(point.line + 1).position - 1)
(*e: function [[Text.point_to_eol]] *)

(*s: function [[Text.point_to_bol]] *)
(* less: kinda bol_to_point really *)
let point_to_bol text point =
  low_distance text 
    text.text_newlines.(point.line).position
    point.pos
(*e: function [[Text.point_to_bol]] *)

(*s: function [[Text.point_to_eof]] *)
let point_to_eof text point =
  low_distance text point.pos text.text_size
(*e: function [[Text.point_to_eof]] *)

(*s: function [[Text.point_to_bof]] *)
let point_to_bof text point =
  low_distance text 0 point.pos
(*e: function [[Text.point_to_bof]] *)

(*s: function [[Text.point_to_lof]] *)
let point_to_lof text point n =
  if n > 0 
  then point_to_eof text point
  else point_to_bof text point
(*e: function [[Text.point_to_lof]] *)

(*s: function [[Text.point_to_lol]] *)
let point_to_lol text point n =
  if n > 0 
  then point_to_eol text point
  else point_to_bol text point
(*e: function [[Text.point_to_lol]] *)


(*s: function [[Text.point_to_line]] *)
let point_to_line text point line =
  let pos = 
    if text.text_nlines <= line + 1 
    then text.text_size
    else text.text_newlines.(line).position
  in
  move_point_to_pos text point pos
(*e: function [[Text.point_to_line]] *)


(*****************************************************************************)
(* Misc  *)
(*****************************************************************************)

(*s: function [[Text.to_string]] *)
let to_string text =
  let len = text.text_size - text.gsize in
  if len = 0 
  then "" 
  else begin
    let str = Bytes.create len in
    let gpos = text.gpoint.pos in
    let gap_end = gpos + text.gsize in
    Bytes.blit text.text_string 0 str 0 gpos;
    Bytes.blit text.text_string gap_end str gpos (len- gpos);
    Bytes.to_string str
  end
(*e: function [[Text.to_string]] *)

(*s: function [[Text.clear]] *)
let clear text =
  low_delete text 0 (text.text_size - text.gsize) |> ignore;
  text.text_history <- [];
  List.iter (fun p -> p.pos <- 0; p.line <- 0) text.text_points
(*e: function [[Text.clear]] *)

(*s: function [[Text.update]] *)
let update text str =
  let newlines = compute_newlines str in
  let len = String.length str in
  text.text_points |> List.iter (fun point -> 
    point.pos <- get_position text point
  );
  text.text_string <- Bytes.of_string str;
  text.text_attrs <- (Array.make len direct_attr);
  text.text_size <- len;
  text.gpoint <- { pos = 0; line = 0 };
  text.gsize <- 0;
  text.text_newlines <- newlines;
  text.text_nlines <- Array.length newlines;
  text.text_modified <- text.text_modified + 1 ;
  text.text_clean <- true;
  text.text_history <- [];
  text.text_points |> List.iter (fun point -> 
      let pos = point.pos in
      point.pos <- 0;
      point.line <- 0;
      set_position text point pos
  ) 
  
(*e: function [[Text.update]] *)

(*s: function [[Text.lexing]] *)
let lexing text curseur end_point =
  clean_text text;
  Lexing.from_function (fun str len ->
    let len = min len (distance text curseur end_point) in
    let len = blit str text curseur len in
    fmove text curseur len;
    len
  )
(*e: function [[Text.lexing]] *)

    
(*s: function [[Text.readonly]] *)
let readonly text = 
  text.text_readonly
(*e: function [[Text.readonly]] *)
  
(*s: function [[Text.toggle_readonly]] *)
let toggle_readonly text = 
  text.text_readonly <- not text.text_readonly
(*e: function [[Text.toggle_readonly]] *)
  
(*e: core/text.ml *)
