open Efuns

(* This file contains all the pad-specific keys/cmds/options.
 * Hopefully it will be shorter than my .emacs ...
 * alt: have a .efunsrc, but not everything is customizable through options.
 *  moreover I prefer OCaml typed code to untyped text configuration file!
 *)

(* special functions *)
let gtd (frm : Frame.t) : unit = 
  Frame.load_file frm.caps frm.frm_window "/home/pad/GTD/GTD-daily.org" |> ignore
[@@interactive]

let _ =
  Hooks.add_start_hook (fun () ->

    (* keybindings *)
    [
      [MetaMap, Char.code 'g'], Misc_features.goto_line;

      [MetaMap, XK.xk_Up], Scroll.scroll_up; 
      [MetaMap, XK.xk_Down], Scroll.scroll_down; 
      [MetaMap, XK.xk_Left], Scroll.scroll_other_up; 
      [MetaMap, XK.xk_Right], Scroll.scroll_other_down; 

      [ControlMap, Char.code '/'], Edit.undo;

      [MetaMap, XK.xk_Return], Compil.compile;
      (* Lefessant did that *)
      [Keymap.c_c; ControlMap, Char.code 'c'], Compil.compile;
      [Keymap.c_c; ControlMap, Char.code 'b'], Indent.indent_buffer;
      [Keymap.c_c; ControlMap, Char.code 'l'], Color.color_buffer;
      [MetaMap,Char.code 'q'], Indent.indent_phrase;

      [ControlMap, Char.code 'n' ], Compil.next_error;

      [MetaMap, Char.code '1'], Shell.eshell_num;
      [MetaMap, Char.code '2'], Shell.eshell_num;
      [MetaMap, Char.code '3'], Shell.eshell_num;
      [MetaMap, Char.code '4'], Shell.eshell_num;
      [MetaMap, Char.code '5'], Shell.eshell_num;
      [MetaMap, Char.code '9'], Shell.eshell_num;
      [MetaMap, Char.code '0'], Shell.eshell_num;
      [MetaMap, Char.code '-'], Shell.eshell_num;
      [MetaMap, Char.code '='], Shell.eshell_num;

      [ControlMetaMap, Char.code '1'], Outline_mode.outline_num;
      [ControlMetaMap, Char.code '2'], Outline_mode.outline_num;
      [ControlMetaMap, Char.code '3'], Outline_mode.outline_num;
      [ControlMetaMap, Char.code '4'], Outline_mode.outline_num;
      [ControlMetaMap, Char.code '5'], Outline_mode.outline_num;

      [ControlMetaMap, XK.xk_Tab], Buffer_menu.menu;
      [ControlMetaMap, Char.code 'l'], Multi_buffers.switch_to_other_buffer;

      (* Lefessant? in emacs now? *)
      [ControlMetaMap, XK.xk_Left], Multi_buffers.left_buffer;
      [ControlMetaMap, XK.xk_Right], Multi_buffers.right_buffer;

      (* pad: xemacs inspired, but often intercepted by window manager *)
      [ControlMap, XK.xk_Tab], Multi_frames.next_frame;

    ] |> List.iter (fun (keys, action) ->
      Keymap.add_global_key keys action
    );
  
    (* ~/.login like *)
    (*
    Unix.putenv "PFFF_HOME" "/home/pad/pfff";
    Unix.putenv "PATH" (
       "/home/pad/.opam/4.02.3/bin:" ^
        (try (Unix.getenv "PATH") with Not_found -> "/bin") ^ 
       ":/home/pad/bin"
     );
    *)

    (* projects *)

    (* Hooks *)

    (* old: but requires caps now so simpler to move in CLI.ml
       or start interactively via M-x server_start
       Server.start (); 
     *)
  )
