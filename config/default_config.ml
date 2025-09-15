(*s: config/default_config.ml *)
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
module Parameter = Parameter_option

(* pad: this used to be called config.ml, but this conflict with 
 * compiler-libs config.ml file *)

open Keymap (* for the c_xxx *)
(*s: constant [[standard_map]] *)
let standard_map = [
  (*s: [[standard_map]] entries *)
  (* -------------------------------------------------------- *)
  (* Loading *)
  (* -------------------------------------------------------- *)
  (*s: loading keys *)
  [c_x; ControlMap, Char.code 'f'], Multi_buffers.load_buffer;
  (*x: loading keys *)
  [c_x; NormalMap, Char.code 'i'], Multi_buffers.insert_file;
  (*e: loading keys *)

  (* -------------------------------------------------------- *)
  (* Navigating (in the file) *)
  (* -------------------------------------------------------- *)
  (*s: navigating keys *)
  [NormalMap, XK.xk_Left], (fun frm -> ignore (Move.move_backward frm 1)); 
  [NormalMap, XK.xk_Right], (fun frm -> ignore (Move.move_forward frm 1)); 
  [ControlMap, Char.code 'b'], (fun frm -> ignore (Move.move_backward frm 1)); 
  [ControlMap, Char.code 'f'], (fun frm -> ignore (Move.move_forward frm 1)); 

  [ControlMap, XK.xk_Left ], (Frame.to_frame Move.backward_word);
  [ControlMap, XK.xk_Right ], (Frame.to_frame Move.forward_word);  
  (* ?? standard ?*)
  [MetaMap, XK.xk_Left ], (Frame.to_frame Move.backward_word);
  [MetaMap, XK.xk_Right ], (Frame.to_frame Move.forward_word);

  [ControlMap, Char.code 'a'], Move.beginning_of_line;
  [ControlMap, Char.code 'e'], Move.end_of_line;

  [NormalMap, XK.xk_Up], Move.backward_line; 
  [NormalMap, XK.xk_Down], Move.forward_line; 


  [ControlMap, XK.xk_Up], (Frame.to_frame Move.backward_paragraph);
  [ControlMap, XK.xk_Down], (Frame.to_frame Move.forward_paragraph);  

  [NormalMap, XK.xk_Prior], Scroll.backward_screen; 
  [NormalMap, XK.xk_Next], Scroll.forward_screen;

  [ControlMap, XK.xk_Next], Move.end_of_file;
  [ControlMap, XK.xk_Prior], Move.begin_of_file;
  [MetaMap, Char.code '>'], Move.end_of_file;
  [MetaMap, Char.code '<'], Move.begin_of_file;
  (*x: navigating keys *)
  [ControlMap, Char.code 'u'; ControlMap, Char.code ' '], 
    Move.goto_last_saved_pos; 
  (*x: navigating keys *)
  [c_x; ControlMap, Char.code 'x'], Move.point_at_mark;
  (*e: navigating keys *)

  (* -------------------------------------------------------- *)
  (* Editing *)
  (* -------------------------------------------------------- *)

  (* ------------- *)
  (* Inserting *)
  (* ------------- *)
  (* see also the many self_insert_xxx in core_map *)
  (*s: inserting keys *)
  [NormalMap, XK.xk_Return], Edit.insert_return; 
  (*e: inserting keys *)
  (* ------------- *)
  (* Deleting *)
  (* ------------- *)
  (*s: deleting keys *)
  [NormalMap, XK.xk_BackSpace], Edit.delete_backspace_char; 
  [ControlMap, Char.code 'd'], Edit.delete_char;
  [NormalMap, XK.xk_Delete], Edit.delete_char; 

  [MetaMap, Char.code 'd' ], (Frame.to_frame Edit.delete_forward_word);
  [MetaMap, XK.xk_BackSpace ], (Frame.to_frame Edit.delete_backward_word);

  [ControlMap, Char.code 'k'], Copy_paste.kill_end_of_line;
  (*x: deleting keys *)
  (* ?? *)
  [ControlMap, XK.xk_BackSpace], Electric.hungry_electric_delete;
  (*e: deleting keys *)
  (* ------------------------------ *)
  (* Moving (Cut, copy, paste) *)
  (* ------------------------------ *)
  (*s: moving keys *)
  [ControlMap, Char.code ' '], Copy_paste.mark_at_point;
  [ControlMap, Char.code 'w'], Copy_paste.kill_region;
  [ControlMap, Char.code 'y'], Copy_paste.insert_killed;
  [MetaMap, Char.code 'y'], Copy_paste.insert_next_killed;
  [MetaMap, Char.code 'w'], Copy_paste.copy_region;
  [c_x; NormalMap, Char.code 'h'], Copy_paste.mark_whole_buffer;
  (*e: moving keys *)
  (* ---------------------- *)
  (* Transforming *)
  (* ---------------------- *)
  (*s: transforming keys *)
  [ControlMap, Char.code 't'], (Frame.to_frame Edit.transpose_chars);
  [MetaMap, Char.code 't'], (Frame.to_frame Edit.transpose_words);

  [MetaMap, Char.code 'l'], (fun frm ->
    Edit.on_word frm.frm_buffer frm.frm_point String.lowercase_ascii
  );
  [MetaMap, Char.code 'u'], (fun frm ->
    Edit.on_word frm.frm_buffer frm.frm_point String.uppercase_ascii
  );
  [MetaMap, Char.code 'c'], (fun frm ->
    Edit.on_word frm.frm_buffer frm.frm_point String.capitalize_ascii
  );
  [MetaMap, XK.xk_q], Transform.fill_paragraph;
  (*e: transforming keys *)

  (* -------------------------------------------------------- *)
  (* Search/replace *)
  (* -------------------------------------------------------- *)
  (*s: searching keys *)
  [ControlMap, Char.code 's'], Search.isearch_forward;
  [ControlMap, Char.code 'r'], Search.isearch_backward;
  (* standard? *)
  [MetaMap, Char.code 's'], Search.isearch_forward_regexp;
  [MetaMap, Char.code 'r'], Search.isearch_backward_regexp;
  (*e: searching keys *)
  (*s: replacing keys *)
  [MetaMap, Char.code '%'], Search.query_replace_string;
  (*e: replacing keys *)

  (* -------------------------------------------------------- *)
  (* Undoing *)
  (* -------------------------------------------------------- *)
  (*s: undoing keys *)
  [ControlMap, Char.code '_'], Edit.undo;
  (*e: undoing keys *)

  (* -------------------------------------------------------- *)
  (* External commands *)
  (* -------------------------------------------------------- *)
  (*s: external commands keys *)
  [MetaMap, Char.code '!'], System.shell_command;
  (*e: external commands keys *)

  (* -------------------------------------------------------- *)
  (* Buffers/windows/frames *)
  (* -------------------------------------------------------- *)
  (*s: buffer management keys *)
  [c_x; NormalMap, Char.code 'b'], Multi_buffers.change_buffer;
  (*x: buffer management keys *)
  [c_x; NormalMap, Char.code 'k'], Multi_buffers.kill_buffer;
  (*e: buffer management keys *)
  (*s: buffer navigating keys *)
  (* pinning, lefessant?? *)
  [ControlMetaMap, XK.xk_Down], Multi_buffers.down_buffer;
  [ControlMetaMap, XK.xk_Up], Multi_buffers.up_buffer;
  (*e: buffer navigating keys *)
  (*s: frame management keys *)
  [c_x; NormalMap, Char.code '2'], Multi_frames.vertical_cut_frame;    
  [c_x; NormalMap, Char.code '3'], Multi_frames.horizontal_cut_frame;    
  (*x: frame management keys *)
  [c_x; NormalMap, Char.code '1'], Multi_frames.one_frame;
  [c_x; NormalMap, Char.code '0'], Multi_frames.delete_frame;
  (*e: frame management keys *)
  (*s: frame navigation keys *)
  [c_x; NormalMap, Char.code 'o'], Multi_frames.next_frame;
  (*e: frame navigation keys *)

  (* -------------------------------------------------------- *)
  (* Meta *)
  (* -------------------------------------------------------- *)
  (*s: meta keys *)
  [MetaMap, Char.code 'x'], Interactive.call_interactive;
  (*e: meta keys *)

  (* -------------------------------------------------------- *)
  (* Saving *)
  (* -------------------------------------------------------- *)
  (*s: saving keys *)
  [c_x; ControlMap, Char.code 's'], Multi_buffers.save_buffer; 
  (*x: saving keys *)
  [c_x; NormalMap,  Char.code 's'], Multi_buffers.save_some_buffers;
  [c_x; ControlMap, Char.code 'w'], Multi_buffers.write_buffer; 
  (*e: saving keys *)

  (* -------------------------------------------------------- *)
  (* Rectangles *)
  (* -------------------------------------------------------- *)
  [c_x; NormalMap, Char.code 'r'; NormalMap, Char.code 't'],
    Rectangle.rectangle_insert;
  [c_x; NormalMap, Char.code 'r'; NormalMap, Char.code 'k'],
    Rectangle.rectangle_kill;

  (* -------------------------------------------------------- *)
  (* Compile/grep *)
  (* -------------------------------------------------------- *)

  (* -------------------------------------------------------- *)
  (* Macros *)
  (* -------------------------------------------------------- *)
  [c_x; NormalMap, Char.code '('], Macros.start_macro;
  [c_x; NormalMap, Char.code ')'], Macros.end_macro;
  [c_x; NormalMap, Char.code 'e'], Macros.call_macro;

  (* -------------------------------------------------------- *)
  (* Misc *)
  (* -------------------------------------------------------- *)
  (*s: misc keys *)
  [c_h; NormalMap, Char.code 'v'], Interactive.describe_variable;
  (*x: misc keys *)
  [c_x; NormalMap, Char.code '='], Misc_features.cursor_position;
  (*x: misc keys *)
  [c_x; ControlMap, Char.code 'c'], Misc_features.exit; 
  (*x: misc keys *)
  [ControlMap, Char.code 'l'], Scroll.recenter;
  (*x: misc keys *)
  [c_h; NormalMap, Char.code 'K'], Frame.help_bindings;
  (*x: misc keys *)
  [ c_c; NormalMap, Char.code '-'], Structure.next_hole;
  (*x: misc keys *)
  [c_x;NormalMap, Char.code '`' ], Compil.next_error;
  (*x: misc keys *)
  [NormalMap, Char.code ' '], Abbrevs.char_expand_abbrev;
  [MetaMap, Char.code '/'], Abbrevs.dabbrev_expand;
  (*x: misc keys *)
  [NormalMap, XK.xk_Insert], Edit.toggle_overwrite_mode;
  (*e: misc keys *)
  (*e: [[standard_map]] entries *)
] 
(*e: constant [[standard_map]] *)

(*s: constant [[core_map]] *)
let core_map = 
 (*s: [[core_map]] entries *)
 (* standard chars *)
 (List_.enum 32 127 |> List.map (fun key ->
   [NormalMap, key], Edit.self_insert_command
 )) @
 (*x: [[core_map]] entries *)
 (let c_q = (ControlMap, Char.code 'q') in
 (List_.enum 65 (65+25) |> List.map (fun key ->
   [c_q;ControlMap, key], Misc_features.insert_special_char
 )) @
 (* needed? *)
 (List_.enum 97 (97+25) |> List.map (fun key ->
   [c_q;ControlMap, key], Misc_features.insert_special_char
 ))) @
 (*x: [[core_map]] entries *)
 ([
 (* see pbs_accent.txt in this directory
 *)
 ] |> List.map (fun (key, char) ->
     (* special for AZERTY keyboards *)
         [NormalMap, key], (Edit.char_insert_command char)
     )
 ) @
 (*e: [[core_map]] entries *)
 []
(*e: constant [[core_map]] *)

(*s: function [[Config.save_options]] *)
let save_options _frame = 
  Options.save ()
[@@interactive]
(*e: function [[Config.save_options]] *)

open Options
  
(*s: constant [[Config.global_map]] *)
let global_map = define_option ["global_map"] "" 
  (list_option Keymap.binding_option) []
(*e: constant [[Config.global_map]] *)

(*s: function [[Config.init_global_map]] *)
let init_global_map () = 
  if !!global_map =*= [] 
  then 
    (core_map @ standard_map) |> List.iter (fun (keys, action) ->
        Keymap.add_global_key keys action
    )
  else 
  !!global_map |> List.iter (fun (keys, action) ->
      try
        Keymap.add_global_key keys (Action.execute_action action)
      with e ->
        Log.printf "Error for action %s" action;
        Log.exn "%s\n" e;
  );

  (* Mouse *)
  (*s: [[Config.init_global_map()]] mouse keys setup *)
  Keymap.add_global_key [NormalMap, XK.xk_Pointer_Button1] Mouse.mouse_set_frame;
  (*x: [[Config.init_global_map()]] mouse keys setup *)
  Keymap.add_global_key [NormalMap, XK.xk_Pointer_Button3]
    Mouse.mouse_save_then_kill;
  Keymap.add_global_key [NormalMap, XK.xk_Pointer_Button2]
    Mouse.mouse_yank_at_click;
  (*e: [[Config.init_global_map()]] mouse keys setup *)
  ()
(*e: function [[Config.init_global_map]] *)
  
(*s: toplevel [[Config]] menu settings *)
let _ =
  (*s: [[Config]] file menu setup *)
  if !!Top_window.file_menu =*= [] then begin
    Top_window.file_menu =:= [
      "Open File", "load_buffer";
      "Save Buffer", "save_buffer";
      (*s: file menu entries *)
      "Kill Buffer", "kill_buffer";
      (*x: file menu entries *)
      "Compile", "compile";
      (*e: file menu entries *)
      "", "";
      "Quit", "exit";
    ]
    end;
  (*e: [[Config]] file menu setup *)
  (*s: [[Config]] edit menu setup *)
  if !!Top_window.edit_menu =*= [] then begin
      Top_window.edit_menu =:= [ 
        "Cut",    "kill_region";
        "Paste",  "insert_killed";

        "Undo",    "undo";
        "", "";
        (*s: [[edit_menu]] entries *)
        "Cut Frame", "vertical_cut_frame";
        (*x: [[edit_menu]] entries *)
        "One Frame", "one_frame";
        "Delete Frame", "delete_frame";
        (*e: [[edit_menu]] entries *)
      ];
    end;
  (*e: [[Config]] edit menu setup *)
  (*s: [[Config]] help menu setup *)
  Top_window.help_menu := [|
    "Key Bindings", (fun frame ->
      Frame.change_buffer frame.caps frame.frm_window "*bindings*"
    );
    "About Efuns", (fun frame ->
      Frame.change_buffer frame.caps frame.frm_window "*help*"
    );
    "Changes", (fun _frame ->
      (*
        let _ = Frame.load_file frame.frm_window (
            Version.efuns_lib ^"/Changes") in ()
      *)
      failwith "Std_xxx.menu changes: TODO"
    );
  |];
  (*e: [[Config]] help menu setup *)
  (*s: [[Config]] buffers menu setup *)
  Top_window.buffers_menu := (fun top_window _button () ->
      let buffers = ref [] in
      let edt = Globals.editor() in
      Hashtbl.iter (fun name _buf -> buffers := name :: !buffers) edt.edt_buffers;
      let _desc = Array.map (fun name -> 
            (name, Top_window.wrap top_window (fun top_window ->
                  let frame = top_window.top_active_frame in
                  let window = frame.frm_window in
                  Frame.change_buffer frame.caps window name
              )))
        (Array.of_list !buffers) in

      (* X11 *)
      (*
      let menu = new WX_popup.t top_window.top_root desc in
      let (x,y) = button#root_coordinates in
      menu#popup_once x (y + button#height) (Some !WX_types.button_event)
      *)
      failwith "Std_menu: show menus TODO"
      )
  (*e: [[Config]] buffers menu setup *)
(*e: toplevel [[Config]] menu settings *)
  

(*s: toplevel [[Config]] starting hook *)
let _ =
  Hooks.add_start_hook (fun () ->
    (*s: [[Config._5]] start hooks options *)
    Parameter.add_option_parameter Compil.compile_find_makefile;
    Parameter.add_option_parameter Text.add_amount;
    (*e: [[Config._5]] start hooks options *)
    init_global_map ();
    Hooks.add_hook Top_window.handle_key_start_hook Misc_features.check_file;
  )
(*e: toplevel [[Config]] starting hook *)
(*e: config/default_config.ml *)
