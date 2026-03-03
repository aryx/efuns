(* Helpers for headless testing of efuns *)

(* A no-op graphics backend for testing without GTK/Cairo *)
let null_backend : Xdraw.graphics_backend = {
  Xdraw.clear_eol = (fun _ _ _ -> ());
  draw_string = (fun _ _ _ _ _ _ -> ());
  get_clipboard = (fun () -> None);
  set_clipboard = (fun _ -> ());
  update_display = (fun () -> ());
  update_window_title = (fun _ -> ());
}

(* Capabilities for frame creation, obtained once via Cap.main *)
let test_caps : < Efuns.frame_caps > =
  Cap.main (fun caps -> (caps :> < Efuns.frame_caps >))

let editor_initialized = ref false

(* Initialize a minimal editor for testing. Call once before tests. *)
let init_editor () =
  if not !editor_initialized then begin
    let editor : Efuns.editor = {
      edt_buffers = Hashtbl.create 13;
      edt_files = Hashtbl.create 13;
      top_windows = [];
      edt_width = 80;
      edt_height = 25;
      edt_fg = "white";
      edt_bg = "black";
      edt_font = "fixed";
      edt_map = Keymap.create ();
      edt_dirname = Sys.getcwd ();
      edt_vars = Store.new_store ();
      edt_colors_names = Array.make 256 "";
      edt_colors = Hashtbl.create 37;
      edt_colors_n = 0;
      edt_fonts_names = Array.make 256 "";
      edt_fonts = Hashtbl.create 37;
      edt_fonts_n = 0;
      edt_mutex = Mutex.create ();
    } in
    Globals.global_editor := Some editor;
    (* Register foreground (0), background (1) colors *)
    Attr.get_color "white" |> ignore;
    Attr.get_color "black" |> ignore;
    (* Register default font (0) *)
    Attr.get_font "fixed" |> ignore;
    (* Run start hooks (registers modes etc.) *)
    let hooks = List.rev !Hooks.start_hooks in
    Hooks.start_hooks := [];
    hooks |> List.iter (fun f -> f ());
    editor_initialized := true
  end

(* Create a top_window + frame from string content.
   Returns (top_window, frame, text, point). *)
let make_frame ?(width = 80) ?(height = 25) content =
  init_editor ();
  let (edt : Efuns.editor) = Globals.editor () in
  let text = Text.create content in
  let (buf : Efuns.buffer) = Ebuffer.create "test" None text (Keymap.create ()) in
  let (window : Efuns.window) = Window.create_at_top 0 0 width (height - 1) in
  let (frame : Efuns.frame) = Frame.create_without_top test_caps window None buf in
  let (top_window : Efuns.top_window) = {
    top_width = width;
    top_height = height;
    window;
    top_active_frame = frame;
    top_name = buf.buf_name;
    top_mini_buffers = [];
    top_second_cursor = None;
    graphics = Some null_backend;
  } in
  window.win_up <- Efuns.TopWindow top_window;
  edt.top_windows <- top_window :: edt.top_windows;
  (top_window, frame, text, frame.frm_point)

(* Extract text content from a frame's buffer *)
let frame_content (frame : Efuns.frame) =
  Text.to_string frame.frm_buffer.buf_text
