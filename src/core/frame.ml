(*s: core/frame.ml *)
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
open Common
open Eq.Operators
open Options

open Efuns
open Text (* for the line fields *)

(*s: type [[Frame.t]] *)
type t = Efuns.frame
(*e: type [[Frame.t]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Frame.buf_text_point]] *)
let buf_text_point frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  (buf, text, point)
(*e: function [[Frame.buf_text_point]] *)

(*****************************************************************************)
(* Status line *)
(*****************************************************************************)

(*s: constant [[Frame.status_format]] *)
let status_format = ref [
    StatModified, (1, 2);
    StatName, (5, 20);
    StatMode, (30,30);
    StatLine, (65, 5);
    StatCol, (70, 5);
    StatFile, (35,15);
  ]
(*e: constant [[Frame.status_format]] *)

(*s: function [[Frame.status_print]] *)
let status_print status str stat_type =
  status.status_modified <- true;
  try
    let (pos,maxlen) = List.assoc stat_type status.status_format in
    let len = min (String.length str) maxlen in
    Bytes.blit_string str 0 status.status_string pos len;
    Bytes.fill status.status_string (pos + len) (maxlen - len) ' '
  with Not_found -> ()
(*e: function [[Frame.status_print]] *)


(*s: function [[Frame.status_modified]] *)
let status_modified frame modified =
  let status = frame.frm_status in
  if status.stat_modified <> modified then begin
    status_print status (if modified then "**" else "--") StatModified;
    status.stat_modified <- modified
  end
(*e: function [[Frame.status_modified]] *)

(*s: function [[Frame.status_col]] *)
let status_col frame col =
  let status = frame.frm_status in
  if status.stat_col <> col then begin
    status.stat_col <- col;
    status_print status (Printf.sprintf "C%d" (col+1)) StatCol
  end
(*e: function [[Frame.status_col]] *)

(*s: function [[Frame.status_major_mode]] *)
let status_major_mode frame  =
  let buf = frame.frm_buffer in
  let status = frame.frm_status in
  if not (Eq.phys_equal status.stat_modes buf.buf_minor_modes &&
           Eq.phys_equal status.stat_mode buf.buf_major_mode
    ) then
    begin
      status.stat_modes <- buf.buf_minor_modes;
      status.stat_mode <- buf.buf_major_mode;
      status_print status (Printf.sprintf "(%s)" 
          (String.concat " "
            (buf.buf_major_mode.maj_name ::
            (List.map (fun m -> m.min_name) status.stat_modes))))
      StatMode;
    end
(*e: function [[Frame.status_major_mode]] *)

(*s: function [[Frame.status_line]] *)
let status_line frame line =
  let status = frame.frm_status in
  if status.stat_line <> line then begin
    status.stat_line <- line;
    status_print status (Printf.sprintf "L%d" (line+1)) StatLine
  end
(*e: function [[Frame.status_line]] *)

(*s: function [[Frame.status_name]] *)
let status_name frame name =
  let status = frame.frm_status in
  if status.stat_name <> name then begin
    status.stat_name <- name;
    status_print status name StatName
  end
(*e: function [[Frame.status_name]] *)

(*****************************************************************************)
(* Kill *)
(*****************************************************************************)

(*s: function [[Frame.kill]] *)
let kill frame = 
  let (buf, text, point) = buf_text_point frame in
  (*s: [[Frame.kill()]] setting [[frm_killed]] field *)
  frame.frm_killed <- true;
  (*e: [[Frame.kill()]] setting [[frm_killed]] field *)
  buf.buf_shared <- buf.buf_shared - 1;
  Text.remove_point text buf.buf_point;
  Text.remove_point text buf.buf_start;
  buf.buf_point <- point;
  buf.buf_start <- frame.frm_start
(*e: function [[Frame.kill]] *)

(*****************************************************************************)
(* Display *)
(*****************************************************************************)

(*s: function [[Frame.install]] *)
let install window frame =
  (*s: [[Frame.install()]] sanity check frame is not a minibuffer *)
  (* TODO: dunno why but can't use =:= for bool *)
  if window.win_mini =*= (frame.frm_mini_buffer =*= None) then begin 
    kill frame; 
    failwith "Cannot install in minibuffer"
  end;
  (*e: [[Frame.install()]] sanity check frame is not a minibuffer *)

  window |> Window.iter (fun f -> 
     (* TODO? != or <> ? *)
     if (Eq.phys_not_equal f frame) 
     then kill f
  );
  window.win_down <- WFrame frame;

  frame.frm_xpos <- window.win_xpos;
  frame.frm_ypos <- window.win_ypos;
  frame.frm_width <- window.win_width;
  frame.frm_height <- window.win_height;
  frame.frm_window <- window;

  (*s: [[Frame.install()]] adjust [[frm_cutline]] *)
  if frame.frm_cutline < max_int 
  then frame.frm_cutline <- window.win_width - 1;
  (*e: [[Frame.install()]] adjust [[frm_cutline]] *)
  (*s: [[Frame.install()]] set [[frm_table]] *)
  frame.frm_table <- (Array.init window.win_height (fun _i -> 
     {
       frm_text_line = Text.dummy_line;
       frmline_boxes = [];

       lineidx_in_text = 0;
       first_box_extra_offset = 0;

       repr_prev_offset = 0;
       prev_frmline_boxes = [];
     } 
  ));
  (*e: [[Frame.install()]] set [[frm_table]] *)
  frame.frm_redraw <- true
(*e: function [[Frame.install]] *)

(*****************************************************************************)
(* Constructor *)
(*****************************************************************************)

(*s: constant [[Frame.editname]] *)
let editname = "Efuns:"
(*e: constant [[Frame.editname]] *)
(*s: constant [[Frame.dummy_mode]] *)
let dummy_mode = Ebuffer.new_major_mode "" None
(*e: constant [[Frame.dummy_mode]] *)
  
(*s: function [[Frame.create_without_top]] *)
let create_without_top window mini buf =
  let text = buf.buf_text in

  let frm_start = Text.dup_point text buf.buf_start in
  let point     = Text.dup_point text buf.buf_point in
  let frm_end   = Text.dup_point text buf.buf_start in (* ?? *)

  buf.buf_shared <- buf.buf_shared + 1;

  (*s: [[Frame.create_without_top()]] let status *)
  let status = {
      stat_name = "";
      stat_file = "";
      stat_line = -1;
      stat_col = -1;
      status_modified = true;
      stat_modified = (buf.buf_last_saved =|= Text.version buf.buf_text);

      stat_modes = [];
      stat_mode = dummy_mode;

      status_format = !status_format;

      status_string = Bytes.make 256 '-';
    } in
  String.blit editname 0 status.status_string 5 (String.length editname);
  (*e: [[Frame.create_without_top()]] let status *)

  let frame =
    { frm_buffer = buf;

      frm_window = window;
      frm_xpos = window.win_xpos;
      frm_ypos = window.win_ypos;
      frm_width = window.win_width;
      frm_height = window.win_height;

      frm_start = frm_start;
      frm_point = point;
      frm_end = frm_end;

      (* this will be set later in set_cursor called from update_display *)
      frm_cursor_x = 0;
      frm_cursor_y = 0;
      frm_cursor = Bytes.make 1 ' ';
      frm_cursor_attr = Text.direct_attr;

      frm_last_text_updated = 0;
      frm_last_buf_updated = 0;

      frm_redraw = true; (* ! *)
      frm_cutline = window.win_width - 1; (* ! *)

      frm_force_start = false;
      frm_x_offset = 0;
      frm_y_offset = 0;
      frm_table = [||];

      frm_has_status_line = 1;
      frm_status = status;
      frm_mini_buffer = mini;
      
      frm_prefix = [];
      frm_last_action = Keymap.dummy_action;

      frm_killed = false;
    } 
  in
  (*s: [[Frame.create_without_top()]] adjust status of frame *)
  status_name frame buf.buf_name;
  (*x: [[Frame.create_without_top()]] adjust status of frame *)
  status_major_mode frame;
  (*e: [[Frame.create_without_top()]] adjust status of frame *)

  install window frame;
  frame
(*e: function [[Frame.create_without_top]] *)

(*s: function [[Frame.active]] *)
let active frame =
  let top_window = Window.top frame.frm_window in
  top_window.top_active_frame <- frame;
  frame.frm_buffer.buf_filename |> Option.iter (fun filename ->
    (Globals.editor()).edt_dirname <- Filename.dirname filename
  )
(*e: function [[Frame.active]] *)
      
(*s: function [[Frame.create]] *)
let create window mini buf =
  let frame = create_without_top window mini buf in
  (*s: [[Frame.create()]] adjust active frame *)
  let top_window = Window.top window in
  top_window.top_active_frame <- frame;
  (*e: [[Frame.create()]] adjust active frame *)
  frame
(*e: function [[Frame.create]] *)

(*s: function [[Frame.create_inactive]] *)
let create_inactive window buf =
  create_without_top window None buf
(*e: function [[Frame.create_inactive]] *)

(*****************************************************************************)
(* Cursor *)
(*****************************************************************************)

(*s: function [[Frame.point_to_cursor]] *)
let point_to_x_when_no_cutline buf point =
  let text = buf.buf_text in
  let line = Ebuffer.compute_representation buf (Text.point_line text point) in
  let xpos = Text.point_col text point in
  let rec iter boxes =
    match boxes with
    | [] -> 0
    | box :: tail ->
        if xpos < box.box_col
        then iter tail
        else box.box_pos_repr + box.box_charsize *  (xpos - box.box_col)
  in
  iter line.Text.boxes
(*e: function [[Frame.point_to_cursor]] *)

(*s: function [[Frame.cursor_to_point]] *)
let cursor_to_coord frame x y =
  (*s: [[Frame.cursor_to_point()]] sanity check parameters in range *)
  if (y < 0) || (x<0) || (y >= frame.frm_height-1) || (x>frame.frm_cutline) 
  then raise Not_found;
  (*e: [[Frame.cursor_to_point()]] sanity check parameters in range *)
  let frm_line = frame.frm_table.(y) in
  let line = frm_line.lineidx_in_text in
  let rec iter x boxes default =
    match boxes with
      [] -> default
    | box :: tail -> 
        if x < box.box_size
        then box.box_col +   x / box.box_charsize
        else iter (x - box.box_size) tail (box.box_col + box.box_len)
  in
  let col = iter (x + frame.frm_x_offset + frm_line.first_box_extra_offset) 
    frm_line.frmline_boxes 0 
  in
  { Text.c_col = col; Text.c_line = line }
(*e: function [[Frame.cursor_to_point]] *)


(*****************************************************************************)
(* Display *)
(*****************************************************************************)

(*s: function [[Frame.display_line]] *)
let display_line graphic frame repr_string y = 
  let frm_line = frame.frm_table.(y) in

  let rec iter x offset boxes =
    if x < frame.frm_width then
      match boxes with
      | [] -> 
          graphic.Xdraw.clear_eol (x+frame.frm_xpos) (y+frame.frm_ypos)
            (frame.frm_width - x)
      | box :: tail ->
          let len = min (frame.frm_width-x) (box.box_size - offset) in
          graphic.Xdraw.draw_string (x+frame.frm_xpos) (y+frame.frm_ypos)
            repr_string (box.box_pos_repr+offset) len
            box.box_attr;
          iter (x+len) 0 tail
    else
      (*s: [[Frame.display_line()]] in iter, line overflow [[frm_width]] *)
      graphic.Xdraw.draw_string 
         (frame.frm_width+frame.frm_xpos-1) (y+frame.frm_ypos)
         "/" 0 1 Text.direct_attr
      (*e: [[Frame.display_line()]] in iter, line overflow [[frm_width]] *)
  in
  iter 0 (frm_line.first_box_extra_offset + frame.frm_x_offset) frm_line.frmline_boxes
(*e: function [[Frame.display_line]] *)

(*s: function [[Frame.point_to_xy_opt]] *)
let point_to_xy_opt frame point =
  let (buf, text, _) = buf_text_point frame in

  let x_nocut = point_to_x_when_no_cutline buf point in
  let line = Ebuffer.compute_representation buf (point_line text point) in

  let xref = ref 0 in
  let yref = ref 0 in
  
  try
    for i = 0 to frame.frm_height - 1 do
      let line_repr = frame.frm_table.(i) in
      if Eq.phys_equal line_repr.frm_text_line line then
        let x,y =
          if x_nocut =|= 0 
          then 0,i
          else
            (*s: [[Frame.point_to_xy_opt()]] x y value handling overflow lines *)
            ((x_nocut-1) mod frame.frm_cutline) + 1, i + (x_nocut-1) / frame.frm_cutline
            (*e: [[Frame.point_to_xy_opt()]] x y value handling overflow lines *)
        in
        xref := x;
        yref := y;
        raise Exit
    done;
    None
  with Exit ->
    Some (!xref, !yref)
(*e: function [[Frame.point_to_xy_opt]] *)

(*s: function [[Frame.set_cursor]] *)
let set_cursor frame =
  let (buf, text, point) = buf_text_point frame in

  match point_to_xy_opt frame point with
  | None ->
      (* insert cursor is not on frame *)
      Bytes.set frame.frm_cursor 0 '\000'
  | Some (x, y) ->
      frame.frm_cursor_x <- x;
      frame.frm_cursor_y <- y;
      let col = point_col text point in
      let line_ = point_line text point in
      let line = Ebuffer.compute_representation buf line_ in

      let rec iter boxes =
        match boxes with
        | [] -> 
            Bytes.set frame.frm_cursor 0 ' '
        | box :: tail ->
            if box.box_col <= col && box.box_col + box.box_len > col
            then begin
              let pos_repr =
                box.box_pos_repr + box.box_charsize * 
                (col - box.box_col)
              in
              Bytes.set frame.frm_cursor 0 
                  (Bytes.get line.repr_string pos_repr);
              frame.frm_cursor_attr <- box.box_attr;
            end else
              iter tail
      in
      let frm_line = frame.frm_table.(frame.frm_cursor_y) in
      iter frm_line.frmline_boxes
(*e: function [[Frame.set_cursor]] *)

(*s: function [[Frame.update_table]] *)
let update_table frame =
  let (buf, text, _) = buf_text_point frame in

  let start = frame.frm_start in
  let height = frame.frm_height - frame.frm_has_status_line in

  let current_n = ref (Text.point_line text start) in
  let current_line = ref (Ebuffer.compute_representation buf !current_n) in

  (*s: [[Frame.update_table()]] adjust current line when [[frm_y_offset]] negative *)
  (* assert frame.frm_y_offset >= 0 *)

  while frame.frm_y_offset < 0 && !current_n > 0 do
    current_n := !current_n - 1;
    current_line := Ebuffer.compute_representation buf !current_n;
    let lines = !current_line.repr_len / frame.frm_cutline in
    frame.frm_y_offset <- frame.frm_y_offset + lines + 1;
  done;
  if !current_n =|= 0 && frame.frm_y_offset <0 
  then frame.frm_y_offset <- 0;
  (*e: [[Frame.update_table()]] adjust current line when [[frm_y_offset]] negative *)
  (*s: [[Frame.update_table()]] adjust current line when [[frm_y_offset]] positive *)
  (* assert current_line is the first line *)

  while frame.frm_y_offset > !current_line.repr_len / frame.frm_cutline
      && !current_n < nbr_lines text
  do
    frame.frm_y_offset <- frame.frm_y_offset - 
      (!current_line.repr_len / frame.frm_cutline) - 1;
    current_n := !current_n + 1;
    current_line := Ebuffer.compute_representation buf !current_n;
  done;

  if !current_n =|= nbr_lines text && 
     frame.frm_y_offset > !current_line.repr_len / frame.frm_cutline
  then frame.frm_y_offset <- !current_line.repr_len / frame.frm_cutline;
  (*e: [[Frame.update_table()]] adjust current line when [[frm_y_offset]] positive *)

  (* update frame.frm_start *)
  Text.goto_line text start !current_n; 

  (* update frame representation *)
  (*s: function [[Frame.update_table.iter_line]] *)
  let rec iter_line y n line =
    if y < height then begin
      let reprs = List.rev line.boxes in
      if y >= 0 then begin
          let frm_line = frame.frm_table.(y) in
          frm_line.frm_text_line <- line;
          frm_line.frmline_boxes <- reprs;
          frm_line.lineidx_in_text <- n;
          frm_line.first_box_extra_offset <- 0;
      end;
      iter_repr frame.frm_cutline (y+1) n line reprs
    end
    else Text.goto_line text frame.frm_end (n-1)
  (*e: function [[Frame.update_table.iter_line]] *)
  (*s: function [[Frame.update_table.iter_repr]] *)
  and iter_repr xcutline y n line boxes =
    (*s: [[Frame.update_table.iter_repr()]] if line too big *)
    if line.repr_len > xcutline then
      match boxes with
      | box :: tail ->
          if box.box_pos_repr <= xcutline && 
             box.box_pos_repr + box.box_size > xcutline
          then
            if y =|= height 
            then Text.goto_line text frame.frm_end n 
            else begin
                if y>= 0 then begin
                  let frm_line = frame.frm_table.(y) in
                  frm_line.frm_text_line <- line;
                  frm_line.lineidx_in_text <- n;
                  frm_line.first_box_extra_offset <- xcutline - box.box_pos_repr;
                  frm_line.frmline_boxes <- boxes;
                end;
                iter_repr (xcutline+frame.frm_cutline) (y+1) n line boxes
            end
          else iter_repr xcutline y n line tail
      | [] -> 
          let line = Ebuffer.compute_representation buf (n + 1) in
          iter_line (y+1) (n+1) line
    (*e: [[Frame.update_table.iter_repr()]] if line too big *)
    else  
      let line = Ebuffer.compute_representation buf (n+1) in
      iter_line y (n+1) line
  in
  (*e: function [[Frame.update_table.iter_repr]] *)

  iter_line (- frame.frm_y_offset) !current_n !current_line
[@@profiling]
(*e: function [[Frame.update_table]] *)

(*s: function [[Frame.display]] *)
let display top_window frame =
  let (buf, text, point) = buf_text_point frame in

  let width = frame.frm_width in
  let height = frame.frm_height - frame.frm_has_status_line in

  let graphic = Efuns.backend top_window in

  (*s: [[Frame.display()]] if buf sync goto end of text *)
  if buf.buf_sync && buf.buf_modified <> frame.frm_last_buf_updated 
  then Text.set_position text point (Text.size text); 
  (*e: [[Frame.display()]] if buf sync goto end of text *)
  if
    (*s: [[Frame.display()]] conditions for redraw *)
    (*s: [[Frame.display()]] conditions for redraw, point outside frame *)
    (point < frame.frm_start) ||
    (point > frame.frm_end)  || 
    (*e: [[Frame.display()]] conditions for redraw, point outside frame *)
    (*s: [[Frame.display()]] conditions for redraw, buffer modified *)
    (version text <> frame.frm_last_text_updated) ||
    (buf.buf_modified <> frame.frm_last_buf_updated) ||
    (*e: [[Frame.display()]] conditions for redraw, buffer modified *)
    (*s: [[Frame.display()]] conditions for redraw, forced redraw *)
    frame.frm_redraw
    (*e: [[Frame.display()]] conditions for redraw, forced redraw *)
    (*e: [[Frame.display()]] conditions for redraw *)
  then begin
    (*s: [[Frame.display()]] redraw *)
    if !Globals.debug_graphics
    then Logs.debug (fun m -> m "redraw");
    (*s: [[Frame.display()]] redraw, possibly update [[frm_y_offset]] *)
    let start = frame.frm_start in
    let start_c = point_to_x_when_no_cutline buf start in
    if start_c > 0 then begin
      frame.frm_y_offset <- frame.frm_y_offset - start_c / frame.frm_cutline;
      Text.bmove text start start_c
    end;
    (*e: [[Frame.display()]] redraw, possibly update [[frm_y_offset]] *)
    (*s: [[Frame.display()]] redraw, possibly update [[frm_x_offset]] *)
    let point_c = point_to_x_when_no_cutline buf point in
    if point_c < frame.frm_x_offset then begin
        frame.frm_x_offset <- max (point_c - width / 2) 0;
        frame.frm_redraw <- true;
    end else if frame.frm_cutline =|= max_int && 
                (point_c mod frame.frm_cutline >= frame.frm_x_offset + width - 3)  
      then begin
        frame.frm_x_offset <- point_c - (width / 2);
        frame.frm_redraw <- true;
    end;
    (*e: [[Frame.display()]] redraw, possibly update [[frm_x_offset]] *)
    (* invariant: now frm_start is at a bol *)
    update_table frame;
    (* invariant: now frm_end has been correctly set *)

    if (point > frame.frm_end) || (point < start) then begin
        (*s: [[Frame.display()]] redraw, if [[frm_force_start]] *)
        if frame.frm_force_start then begin
          let coord = cursor_to_coord frame frame.frm_cursor_x frame.frm_cursor_y in
          Text.goto_line text frame.frm_point coord.Text.c_line;
          Text.fmove text frame.frm_point coord.Text.c_col
        end 
        (*e: [[Frame.display()]] redraw, if [[frm_force_start]] *)
        else begin
          (* center around point *)
          Text.goto_point text start point;
          (*s: [[Frame.display()]] redraw, update [[frm_y_offset]] again *)
          frame.frm_y_offset <- - height / 2;
          let start_c = point_to_x_when_no_cutline buf start in
          if start_c > 0 then begin
            frame.frm_y_offset <- frame.frm_y_offset - start_c / frame.frm_cutline;
            Text.bmove text start start_c
          end;
          (*e: [[Frame.display()]] redraw, update [[frm_y_offset]] again *)
          (* invariant: now frm_start is at a bol again *)
          update_table frame;
       end
    end;

    frame.frm_force_start <- false;

    frame.frm_last_text_updated <- version text;
    frame.frm_last_buf_updated <- buf.buf_modified;

    for y = 0 to height - 1 do
      (*s: [[Frame.display()]] redraw, draw line y if line changed *)
      let frm_line = frame.frm_table.(y) in

      if not ((Eq.phys_equal frm_line.prev_frmline_boxes frm_line.frmline_boxes) &&
              (Eq.phys_equal frm_line.repr_prev_offset frm_line.first_box_extra_offset)) 
         || frame.frm_redraw
      then
        begin
          frm_line.prev_frmline_boxes <- frm_line.frmline_boxes;
          frm_line.repr_prev_offset <- frm_line.first_box_extra_offset;

          display_line graphic frame 
              (Bytes.to_string frm_line.frm_text_line.repr_string)
              y;
        end;
      (*e: [[Frame.display()]] redraw, draw line y if line changed *)
    done;
    frame.frm_redraw <- false
    (*e: [[Frame.display()]] redraw *)
  end;
  (*s: [[Frame.display()]] draw status line or minibuffer *)
  match frame.frm_mini_buffer with
  | None -> 
      (*s: [[Frame.display()]] draw status line *)
      (*
      let status = frame.frm_status in

      status_modified frame (version text <> buf.buf_last_saved);
      status_line frame  (point_line text frame.frm_point);
      status_col  frame  (point_col  text frame.frm_point);
      status_name frame buf.buf_name;
      status_major_mode frame;

      if status.status_modified 
      then
        graphic.Xdraw.draw_string 
          frame.frm_xpos (frame.frm_ypos + frame.frm_height - 1)
          status.status_string 
          0 width Text.inverse_attr
      *)
      let str = spf "-%s- %s  %s L%-6d (%s) --"
        (if (version text <> buf.buf_last_saved) then "**" else "--")
        (buf.buf_name ^ 
         String.make (max 1 ((width / 3) - String.length buf.buf_name)) ' ')
        (match () with
        | _ when Text.get_position text point =|= 0 -> "Top"
        | _ when Text.get_position text point =|= Text.size text -> "Bot"
        | _ -> spf "%2d%%" 
          (Text.point_line text point * 100 / Text.nbr_lines text)
        )
        (Text.point_line text point + 1)
        (let xs = buf.buf_minor_modes |> List.map (fun m -> m.min_name) in
         let xs = buf.buf_major_mode.maj_name :: xs in
         xs |> String.concat " "
        )
      in
      let minuses = String.make 256 '-' in
      let str = str ^ minuses in
      graphic.Xdraw.draw_string 
        frame.frm_xpos (frame.frm_ypos + frame.frm_height - 1)
        str
        0 width Text.inverse_attr
      (*e: [[Frame.display()]] draw status line *)
  | Some request ->
      (*s: [[Frame.display()]] draw minibuffer request string *)
      graphic.Xdraw.draw_string
       0 (top_window.top_height-1)  
       request 
       0 (String.length request) Text.direct_attr
      (*e: [[Frame.display()]] draw minibuffer request string *)
  (*e: [[Frame.display()]] draw status line or minibuffer *)
[@@profiling]
(*e: function [[Frame.display]] *)

(*****************************************************************************)
(* Misc  *)
(*****************************************************************************)

(*s: exception [[Frame.BufferKilled]] *)
exception BufferKilled
(*e: exception [[Frame.BufferKilled]] *)
(*s: function [[Frame.unkill]] *)
let unkill window frame =
  let (buf, text, _) = buf_text_point frame in
  if buf.buf_shared < 0 
  then raise BufferKilled;
  install window frame;
  frame.frm_start <- Text.dup_point text buf.buf_start;
  frame.frm_end <- Text.dup_point text buf.buf_start;
  frame.frm_point <- Text.dup_point text buf.buf_point;
  frame.frm_y_offset <- 0;
  buf.buf_shared <- buf.buf_shared + 1;
  frame.frm_killed <- false
(*e: function [[Frame.unkill]] *)

(*s: function [[Frame.move_point]] *)
let move_point frame point x y =
  let (_, text, _) = buf_text_point frame in
  let coord = cursor_to_coord frame (x - frame.frm_xpos) (y - frame.frm_ypos) in
  Text.goto_line text point coord.Text.c_line;
  Text.fmove text point coord.Text.c_col
(*e: function [[Frame.move_point]] *)

(*s: function [[Frame.current_dir]] *)
let current_dir frame =
  let buf = frame.frm_buffer in
  match buf.buf_filename with
  | Some filename -> Filename.dirname filename ^ "/"
  | None -> (Globals.editor()).edt_dirname ^ "/"
(*e: function [[Frame.current_dir]] *)

(*s: exception [[Frame.FoundFrame]] *)
exception FoundFrame of frame
(*e: exception [[Frame.FoundFrame]] *)

(*s: function [[Frame.find_buffer_frame]] *)
let find_buffer_frame buf =
  try
    (Globals.editor()).top_windows |> List.iter (fun top_window ->
      top_window.window |> Window.iter (fun frame -> 
        if Eq.phys_equal frame.frm_buffer buf 
        then raise (FoundFrame frame)
      )
    );
    raise Not_found
  with FoundFrame frame -> frame
(*e: function [[Frame.find_buffer_frame]] *)

(*s: constant [[Frame.change_buffer_hooks]] *)
let change_buffer_hooks = define_option ["change_buffer_hooks"] "" 
    (list_option string_option)
  [ "check_file" ]
(*e: constant [[Frame.change_buffer_hooks]] *)

(*s: function [[Frame.load_file]] *)
let load_file window filename =
  let buf = Ebuffer.read filename (Keymap.create ()) in
  let frame = create window None buf in
  Hooks.exec_named_hooks !!change_buffer_hooks frame;
  status_name frame buf.buf_name;
  frame
(*e: function [[Frame.load_file]] *)

(*s: function [[Frame.change_buffer]] *)
let change_buffer window name = 
  try
    let buf = Hashtbl.find (Globals.editor()).edt_buffers name in
    let frame = create window None buf in
    Hooks.exec_named_hooks !!change_buffer_hooks frame;
    status_name frame buf.buf_name
  with Not_found -> ()
(*e: function [[Frame.change_buffer]] *)

(*s: function [[Frame.save_buffer]] *)
let save_buffer frame =
  Ebuffer.save frame.frm_buffer
(*e: function [[Frame.save_buffer]] *)

(*s: function [[Frame.bindings_help]] *)
let help_bindings frame =
  let window = frame.frm_window in
  change_buffer window "*bindings*"
[@@interactive]
(*e: function [[Frame.bindings_help]] *)

(*s: function [[Simple.to_frame]] *)
let to_frame f frame =
  f frame.frm_buffer frame.frm_point
(*e: function [[Simple.to_frame]] *)
(*e: core/frame.ml *)
