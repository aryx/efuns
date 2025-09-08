open Common
open Efuns

(*****************************************************************************)
(* Draw API *)
(*****************************************************************************)

(* helper *)
let move_to col line =
  let (w,h) = Graphics.text_size "d" in
  let size_y = Graphics.size_y () in
  Graphics.moveto (w * col) (size_y - h - (line * h))
  

let clear_eol col line len =
  Logs.debug (fun m -> m "clear_eol: %d %d %d" col line len);
  move_to col line;
  let (w,h) = Graphics.text_size "d" in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect (Graphics.current_x()) (Graphics.current_y())
    (w * len) h;
  ()

let draw_string col line  str  offset len   attr =
  Logs.debug (fun m -> m "draw_string %d %d %s %d %d %d"
         col line str offset len attr);
  let (w,h) = Graphics.text_size "d" in
  move_to col line;
  Graphics.set_color Graphics.white;
  Graphics.fill_rect (Graphics.current_x()) (Graphics.current_y())
    (w * len) h;
  Graphics.set_color Graphics.black;
  move_to col line;
  Graphics.draw_string (String.sub str offset len);
  ()

let update_display () =
  Logs.debug (fun m -> m "update_displays")

let backend = { Xdraw. clear_eol; 
                draw_string; 
                update_display;
                update_window_title = (fun _ -> ());
                get_clipboard = (fun () -> None);
                set_clipboard = (fun _opt -> ());
              }


(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

let init init_files =
  Graphics.open_graph (spf " ");
  Graphics.set_window_title "efuns";

  let top_window = Top_window.create () in
  top_window.graphics <- Some backend;

  let _ = Interactive.create_bindings_help_buffer () in

  (* open the first buffers *)
  init_files |> List.iter (fun name ->
    let _ = Frame.load_file top_window.window name in ()
  );
  Top_window.update_display ();

  (* Main loop *)
  let rec loop () =
    Graphics.loop_at_exit [
      Graphics.Button_down;
      Graphics.Key_pressed;
    ] (fun status ->
      if status.Graphics.keypressed
      then 
        let charkey = status.Graphics.key in
        let code = Char.code charkey in
        Logs.debug (fun m -> m "key: %c, %d" charkey code);
        let modifiers, code = 
          match code with
          | 8 | 9 | 13  -> 0, code
          | _ when code >= 1 && code <= 26 -> 
            Xtypes.controlMask, code - 1 + Char.code 'a'
          | _ -> 0, code
        in
        let evt = Xtypes.XTKeyPress (modifiers, spf "%c" charkey, code) in
        Top_window.handler top_window evt
    )
  in
  loop ()
