(* Yoann Padioleau
 * 
 * Copyright (C) 2015, 2025 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
open Options

open World
open Efuns

(* floats are the norm in cairo *)
open Common2_.ArithFloatInfix

module Color = Simple_color
module CH = Cairo_helpers

(*****************************************************************************)
(* Metrics *)
(*****************************************************************************)
let conv_unit unit = 
  float_of_int unit /. (float_of_int Pango.scale)

let compute_metrics edt desc =

  let fontmap = Cairo_pango.Font_map.get_default () in
  let ctx = Cairo_pango.Font_map.create_context fontmap in
  Pango.Context.set_font_description ctx desc;

  let metrics = Pango.Context.get_metrics ctx desc None in

  (* not reliable *)
  let width = conv_unit (Pango.Font.get_approximate_char_width metrics) in
  Logs.info (fun m -> m "metrics width = %f" width);


  let descent = conv_unit (Pango.Font.get_descent metrics) in
  let ascent =  conv_unit (Pango.Font.get_ascent metrics) in
  (* CONFIG? *)
  let height = (ascent + descent) * 1.1 in

  let metrics = { 
    font_width = width; 
    font_height = height;

    main_width = float_of_int edt.edt_width * width;
    main_height = float_of_int edt.edt_height * height;

    mini_factor = 10.;
    margin_factor = 150.;

    (* set later; derived from above (see the code below) *)
    linemax = 0;
    mini_width = 0.;
    margin_width = 0.;
    full_width = 0;
    full_height = 0;
  }
  in
  metrics.linemax <- 
   metrics.main_height * metrics.mini_factor / metrics.font_height 
       |> ceil |> int_of_float;
  metrics.mini_width <- metrics.main_width / metrics.mini_factor;
  metrics.margin_width <- metrics.main_width / metrics.margin_factor;

  (* those are the dimensions for the main view, the drawing area *)
  metrics.full_width <- 
    metrics.main_width + metrics.mini_width + metrics.margin_width * 2.
       |> ceil |>int_of_float;
    (* 1320 *)
  metrics.full_height <- metrics.main_height |> ceil |> int_of_float;
    (* 1400 *)
  
  metrics


(*****************************************************************************)
(* Draw Efuns API *)
(*****************************************************************************)

(* less: could rewrite to just take w instead of cr x pg *)

(* helper *)
let move_to cr pg col line =
  let (_, metrics) = pg in
  let w = metrics.font_width in
  let h = metrics.font_height in
  Cairo.move_to cr (w * col) ((line * h) + h * 0.1)

(* ugly hacks below but had many graphic glitches; cairo 
 * floats are imprecise?
 * CONFIG need to position to ascent point! otherwise some of the
 *  descent part can get erased (look at the 'y','g' chars, often the very
 *  top is erased)
 *)

let clear_eol ?(color="DarkSlateGray") cr pg  col line len =
  (* "WX_xterm.clear_eol: %.f %.f %d, color = %s" col line len color);*)
  let (_, metrics) = pg in
  let w = metrics.font_width in
  let h = metrics.font_height in

  let x =  w * col +
    if color = "DarkSlateGray"
    then 0.
    else w * 0.1

  in
  let y = line * h +
    if color = "DarkSlateGray"
    then 0.
    else h * 0.2
  in
  let h = h *
    if color = "DarkSlateGray"
    then 1.04
    else 0.8
  in
  let w = w * (float_of_int len) + w * 
    (if color = "DarkSlateGray"
     then 0.04 
     else -. 0.1
    )
  in
  (* to debug use draw and pink color ! so get bounding clear box *)
(*  draw_rectangle_xywh ~cr ~x ~y ~w:(w * (float_of_int len)) ~h ~color:"pink" ();*)
  CH.fill_rectangle_xywh ~cr ~x ~y ~w ~h ~color (); 
  ()

let draw_string edt cr pg   col line  str  offset len   attr =
  if !Globals.debug_graphics
  then Logs.debug (fun m -> m "WX_xterm.draw_string %.1f %.1f \"%s\" %d %d attr = %d" 
              col line str offset len attr);
  let bgcolor = 
    let idx = (attr lsr 8) land 255 in
    let idx = 
      if attr land Text.highlight_bit > 0
      then 2
      else idx
    in
    edt.edt_colors_names.(idx)
  in
  clear_eol ~color:bgcolor cr pg col line len;
  move_to cr pg col line;
  let fgcolor = 
    let idx = (attr) land 255 in
    edt.edt_colors_names.(idx)
  in
  CH.set_source_color ~cr ~color:fgcolor ();
  let (ly, metrics) = pg in
  let s = (CH.prepare_string (String.sub str offset len)) in
  Pango.Layout.set_text ly s;
  Cairo_pango.update_layout cr ly;

  (* sanity check that we use a monospace font *)
  let (w, _h) = Pango.Layout.get_size ly in
  let w = conv_unit w in
  let len = String.length s in
  let expected_w = metrics.font_width *. float_of_int len in
  if w <> expected_w
  then failwith (spf "mismatch for '%s': %f <> expected  %f" s w expected_w);

  Cairo_pango.show_layout cr ly;
  ()

let backend w da top_gtk_win = 
  let conv x = float_of_int x in

  let cr = Cairo.create w.base in
  Cairo.translate cr (w.metrics.margin_width) 0.0;
  let pg = (w.ly, w.metrics) in
  let clipboard = GData.clipboard Gdk.Atom.clipboard in

  let edt = w.edt in
  { Xdraw. 
    clear_eol = (fun a b c -> 
      clear_eol cr pg (conv a) (conv b) c
    ); 
    draw_string = (fun a b c d e f -> 
      draw_string edt cr pg (conv a) (conv b) c d e f
    );

    (* refresh drawing area *)
    update_display = (fun () -> 
      if !Globals.debug_graphics
      then Logs.debug (fun m -> m "backend.update_display()");

      Minimap.draw_minimap_when_idle w da;
      (* this will trigger the expose event *)
      GtkBase.Widget.queue_draw da#as_widget;
    );
    update_window_title = (fun s ->
      top_gtk_win#set_title s
    );
    get_clipboard = (fun () ->
      clipboard#text
    );
    set_clipboard = (fun str_opt ->
      let str =
        match str_opt with
        | Some s -> s
        | None -> ""
      in
      clipboard#set_text str
    );
  }

(*****************************************************************************)
(* paint/configure/expose *)
(*****************************************************************************)

let paint () =
  (* this will trigger backend.update_display *)
  Top_window.update_display () 


let get_w = ref (fun () -> failwith "no w yet, configure has been called?")

let configure edt top_window desc metrics da top_gtk_win =
 fun ev ->
  let width = GdkEvent.Configure.width ev in
  let height = GdkEvent.Configure.height ev in

  (* todo: metrics should be recomputed by
   * first adjusting loc_width and loc_height
   *)

  (*-------------------------------------------------------------------*)
  (* Cairo/pango graphics backend setup *)
  (*-------------------------------------------------------------------*)
  let cr = Cairo_gtk.create (*px#pixmap*) da#misc#window in
  let surface = Cairo.get_target cr in

  let colorkind = Cairo.COLOR_ALPHA in

  let ly = CH.pango_layout cr desc in

  (* bugfix: adjust font_width, more reliable than get_approximate_char_width*)
  let text = "m" in
  Pango.Layout.set_text ly text;
  Cairo_pango.update_layout cr ly;
  let (w, _h) = Pango.Layout.get_size ly in
  let w = conv_unit w in
  Logs.info (fun m -> m "adjust metrics.width to %f" w);

  let metrics = { metrics with font_width = w } in

  let w = {
    edt = edt;
    base =    Cairo.Surface.create_similar surface colorkind  width height;
    overlay = Cairo.Surface.create_similar surface colorkind  width height;
    final = surface;
    ly;
    metrics;
    last_top_frame_info = ("", -1, -1);
  }
  in
  top_window.graphics <- Some (backend w da top_gtk_win); 
  get_w := (fun () -> w);

  let cr = Cairo.create w.base in

  CH.fill_rectangle_xywh ~cr ~x:0. ~y:0. 
    ~w:(w.metrics.margin_width) 
    ~h:(float_of_int height)
    ~color:"Black" ();
  Cairo.translate cr (w.metrics.margin_width) 0.0;
  CH.fill_rectangle_xywh ~cr ~x:0. ~y:0. 
    ~w:(float_of_int width) ~h:(float_of_int height)
    ~color:"DarkSlateGray" ();
  Cairo.translate cr (w.metrics.main_width) 0.0;
  CH.fill_rectangle_xywh ~cr ~x:0. ~y:0. 
    ~w:(w.metrics.margin_width) 
    ~h:(float_of_int height)
    ~color:"Black" ();


  (* force a redraw for all the frame after a resize. w.base has changed! *)
  (Globals.editor()).top_windows |> List.iter (fun top_window ->
     top_window.window |> Window.iter(fun frm -> frm.frm_redraw <- true;);
     (match top_window.top_mini_buffers with
      | [] -> ()
      | frm :: _ -> frm.frm_redraw <- true;
     );
  ); 
(*
  let pg = (layout, metrics) in
  for i = 0 to (Globals.location()).loc_height -.. 1 do
    clear_eol cr pg 0. (float_of_int i) 80;
  done;
*)

  paint ();
  true



let assemble_layers w =
  let surface_src = w.base in
  let cr_final = Cairo.create w.final in
  Cairo.set_operator cr_final Cairo.OVER;
  Cairo.set_source_surface cr_final surface_src 0. 0.;
  Cairo.paint cr_final;
  Cairo.set_operator cr_final Cairo.OVER;
  Cairo.set_source_surface cr_final w.overlay 0. 0.;
  Cairo.paint cr_final;
  ()

(* expose should do very little, it should be fast, because
 * this may be called every second after an update to some
 * graphics, e.g. the cursor thread
 *)
let expose _ev =
  let w = !get_w() in
  assemble_layers w;
  true

(*****************************************************************************)
(* The cursor *)
(*****************************************************************************)
let cnt = ref 0
let start_cursor_thread () =
  Thread.create (fun () ->
    while true do
      incr cnt;
      (* Logs.debug (fun m -> m "%d" !cnt);*)
      Thread.delay 0.5;
      Globals.with_lock (fun () ->
        (Globals.editor()).top_windows |> List.iter (fun top_window ->
          if !cnt mod 2 =|= 0
          then Top_window.cursor_on top_window
          else Top_window.cursor_off top_window;
          let graphic = Efuns.backend top_window in
          graphic.Xdraw.update_display();
        )
      )
    done
  ) ()

(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

(* for the special key, Control, Meta, etc *)
let modifiers = ref 0

let init2 init_files =

  (*-------------------------------------------------------------------*)
  (* Graphics initialisation *)
  (*-------------------------------------------------------------------*)
  let _locale = GtkMain.Main.init () in
  let edt = Globals.editor () in

  let desc = Pango.Font.from_string edt.edt_font
  (* see https://github.com/hbin/top-programming-fonts to install
   * some nice programming fonts (e.g., Menlo)
   *)
(*
    "Monospace 19"
    "Monaco 16"
    "Sans Bold 25" 
    "Fixed Bold 32"
    "Courier 19"
    "Menlo 18" <- current
*)
  in
  Logs.info (fun m -> m "pango font: %s" (Pango.Font.to_string desc));
  (* Pango.Font.set_weight desc `ULTRABOLD; *)

  let metrics = compute_metrics edt desc in

  (*-------------------------------------------------------------------*)
  (* Window creation *)
  (*-------------------------------------------------------------------*)

  let win = GWindow.window ~title:"Efuns" () in

  (*-------------------------------------------------------------------*)
  (* Creation of core DS of Efuns (buffers, frames, top_window) *)
  (*-------------------------------------------------------------------*)

  (* edt.loc_height <- 45; *)
  (* will boostrap and use a newly created *help* buffer *)
  let top_window = Top_window.create () in
  (* the *bindings* buffer *)
  Interactive.create_bindings_help_buffer () |> ignore;
  (* open the first buffers *)
  init_files |> List.iter (fun name ->
    Frame.load_file top_window.window name |> ignore
  );

  (*-------------------------------------------------------------------*)
  (* Layout *)
  (*-------------------------------------------------------------------*)

  let vbox = GPack.vbox ~packing:win#add () in

    (*-------------------------------------------------------------------*)
    (* Menu *)
    (*-------------------------------------------------------------------*)
    (* not necessary, can even be distracting, but can be useful
     * for beginners to have at least an help menu.
     *)
    let menubar = GMenu.menu_bar ~packing:vbox#add () in
      let factory = new GMenu.factory menubar in

      factory#add_submenu "_File" |> (fun menu -> 
        GToolbox.build_menu menu ~entries:
          (!!Top_window.file_menu |> List.map (fun (str, action) ->
            if str = "" 
            then `S
            else `I (str, (fun () -> 
              let frame = top_window.top_active_frame in
              Action.execute_action action frame;
              paint ()
            ))))
      ) |> ignore;
      factory#add_submenu "_Edit" |> (fun menu -> 
        GToolbox.build_menu menu ~entries:
          (!!Top_window.edit_menu |> List.map (fun (str, action_name) ->
            match str with
            | "" -> `S
            | _ -> `I (str, (fun () -> 
              let frame = top_window.top_active_frame in
              Action.execute_action action_name frame;
              paint ();
            ))
           ))
      ) |> ignore;

      factory#add_submenu "_Buffers" |> (fun _menu -> 
        (* TODO *)
        ()
      );
      factory#add_submenu "_Help" |> (fun menu -> 
        GToolbox.build_menu menu ~entries:
          (!Top_window.help_menu 
           |> Array.to_list 
           |> List.map (fun (str, action) ->
                `I (str, (fun () -> 
                  let frame = top_window.top_active_frame in
                  action frame;
                  paint ();
                ))))
      ) |> ignore;

    (*-------------------------------------------------------------------*)
    (* main view *)
    (*-------------------------------------------------------------------*)

    let da = GMisc.drawing_area ~packing:vbox#add () in
    (* we manage ourselves layers with cairo *)
    da#misc#set_double_buffered false;
    da#set_size ~width:metrics.full_width ~height:metrics.full_height;
    da#misc#set_can_focus true ;
    da#event#add [ `BUTTON_MOTION; `POINTER_MOTION;
                   `BUTTON_PRESS; `BUTTON_RELEASE ];
    (* `KEY_PRESS; ? or let the even go to the window ? *)
(*
    let px = GDraw.pixmap ~width ~height ~window:win () in
    px#set_foreground `BLACK;
    px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
    GMisc.pixmap px ~packing:vbox#add () |> ignore;
*)

    da#event#connect#configure 
      ~callback:(configure edt top_window desc metrics da win) |> ignore;
    da#event#connect#expose ~callback:(expose) |> ignore;

    (*-------------------------------------------------------------------*)
    (* status bar *)
    (*-------------------------------------------------------------------*)
    let _statusbar = GMisc.statusbar ~packing:vbox#add () in

  (*-------------------------------------------------------------------*)
  (* Events *)
  (*-------------------------------------------------------------------*)

  win#event#connect#key_press ~callback:(fun key ->
    Logs.debug (fun m -> m "key: %d, %s" 
                (GdkEvent.Key.keyval key) (GdkEvent.Key.string key));

    let code_opt =
      match GdkEvent.Key.keyval key with

      (* special keys *)

      | 65289 -> Some XK.xk_Tab
      | 65288 -> Some XK.xk_BackSpace

      | 65293 -> Some XK.xk_Return

      | 65361 -> Some XK.xk_Left
      | 65362 -> Some XK.xk_Up
      | 65363 -> Some XK.xk_Right
      | 65364 -> Some XK.xk_Down
      | 65365 -> Some XK.xk_Prior
      | 65366 -> Some XK.xk_Next
        
      (* modifiers
       * coupling: modify also the release event
      *)

      | 65507 -> modifiers := !modifiers lor Xtypes.controlMask; None
      (* on macOS? *)
      | 65511 -> modifiers := !modifiers lor Xtypes.mod1Mask; None
      (* on Linux: Alt *)
      | 65513 -> modifiers := !modifiers lor Xtypes.mod1Mask; None

      | x when x > 65000 -> None

      (* default *)

      | x -> Some x
    in
    code_opt |> Option.iter (fun code ->
      let evt = Xtypes.XTKeyPress (!modifiers, GdkEvent.Key.string key, code) in
      (* this will generate a redisplay event via backend.update_display *)
      Top_window.handler top_window evt;
    );
    true
  ) |> ignore;

  win#event#connect#key_release ~callback:(fun key ->
    (match GdkEvent.Key.keyval key with
    | 65507 -> modifiers := !modifiers land (lnot Xtypes.controlMask)
    | 65511 -> modifiers := !modifiers land (lnot Xtypes.mod1Mask)
    | 65513 -> modifiers := !modifiers land (lnot Xtypes.mod1Mask)
    | _ -> ()
    );
    true
  ) |> ignore;
  win#event#connect#focus_out ~callback:(fun _focus ->
    if !Globals.debug_graphics
    then Logs.debug (fun m -> m "Focus Out");
    true;
  ) |> ignore;
  win#event#connect#focus_in ~callback:(fun _focus ->
    if !Globals.debug_graphics
    then Logs.debug (fun m -> m "Focus In");
    (* bugfix: reset modifiers when focus back in, otherwise
     * when you Alt-Tab to another window modifiers is set to mod1mask,
     * and when you go back it is like Alt was still on
     *)
    modifiers := 0;
    true;
  ) |> ignore;

  da#event#connect#button_press (fun ev ->
    let (x, y) = GdkEvent.Button.x ev, GdkEvent.Button.y ev in

    (match GdkEvent.get_type ev with
    | `BUTTON_PRESS ->
        (* TODO should get the latest version *)
        let metrics = metrics in
        let button = GdkEvent.Button.button ev in
        let x = (x / metrics.font_width) |> int_of_float in
        let y = (y / metrics.font_height) |> int_of_float in
        if !Globals.debug_graphics
        then Logs.debug (fun m -> m "click on x = %d, y = %d " x y);
        let evt = Xtypes.XTButtonPress(!modifiers, button, x, y) in
        Top_window.handler top_window evt
    | _ -> ()
    );
    true
  ) |> ignore;

  (*-------------------------------------------------------------------*)
  (* End *)
  (*-------------------------------------------------------------------*)
  let _cursor_thread = start_cursor_thread() in

  let quit () = 
    (*Thread.kill cursor_thread;*)
    GMain.Main.quit (); 
  in

  GtkSignal.user_handler := (fun exn -> 
    (match exn with
    | Exit.ExitCode _ -> quit ()
    | _ ->
        let s = Printexc.get_backtrace () in
        Logs.err (fun m -> m "GtkSignal.user_handler: exception!");
        Logs.err (fun m -> m "%s" s);
        Logs.err (fun m -> m "end backtrace");
        (*
          let pb = "pb: " ^ Common.exn_to_s exn in
          G.dialog_text ~text:pb ~title:"pb";
        *)
        raise exn
    )
  );
  win#connect#destroy ~callback:quit |> ignore;
  win#show ();
  GMain.main()

(*****************************************************************************)
(*****************************************************************************)

let init a =
  if !Globals.check
  then Test_libs.test_cairo ()
  else init2 a
