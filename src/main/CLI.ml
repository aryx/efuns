(*s: CLI.ml *)
(*s: copyright header *)
(***********************************************************************)
(*                                                                     *)
(*                             ____________                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header *)
open Options
open Efuns (* for fields for ocaml-light *)

type caps = < Cap.no_caps >

(*s: constant [[Efuns.init_files]] *)
let initial_files = ref []
(*e: constant [[Efuns.init_files]] *)

(*s: constants Main options *)
(* old: I used to enforce 80, but many people sometimes write in col 80
 * so simpler go a little bit above
 *)
let width      = define_option ["width"] "" int_option 82
let height     = define_option ["height"] "" int_option 44
let foreground = define_option ["foreground"] "" string_option "wheat"
let background = define_option ["background"] "" string_option "DarkSlateGray"
(*e: constants Main options *)
(*s: constant [[Main.highlight_color]] *)
let highlight_color = define_option ["highlight_color"] "" color_option "cyan"
(*e: constant [[Main.highlight_color]] *)

(*s: function [[Efuns.init]] *)
let init_efuns editor =
  Globals.global_editor := Some editor;
  let hooks = List.rev !Hooks.start_hooks in
  Hooks.start_hooks := [];
  hooks |> List.iter (fun f -> f ())

(*e: function [[Efuns.init]] *)

(*s: exception [[Main.SigInt]] *)
exception SigInt
(*e: exception [[Main.SigInt]] *)

(*s: constants [[Main.xxx_opt]] *)
let width_opt   = ref None
let height_opt  = ref None
let fg_opt      = ref None
let bg_opt      = ref None
let font_opt    = ref None
(*e: constants [[Main.xxx_opt]] *)
  
(*s: constant [[Efuns.usage_str]] *)
let usage_str =
 "A small editor entirely written in Objective Caml 
by Fabrice LE FESSANT, INRIA Rocquencourt, FRANCE
http://pauillac.inria.fr/efuns
Options:
"
(*e: constant [[Efuns.usage_str]] *)
  
(*s: function [[Main.main]] *)
let main (_caps : < .. >) (argv : string array) : Exit.t =
  (*s: [[main()]] set signal handlers *)
  Utils.register_exn (fun e ->
    match e with
    | Unix.Unix_error (error,f,arg) ->
        Printf.sprintf "Unix error %s: %s %s" f (Unix.error_message error) arg
    | _ -> raise e
  );
  (* TODO: not in ocaml-light: Printexc.record_backtrace true; *)

  Sys.signal Sys.sigint (Sys.Signal_handle (fun _ ->  
    (* Should auto-save all buffers, and then exit ... *)
    exit 1
  )) |> ignore; 
  Sys.signal Sys.sigterm (Sys.Signal_handle (fun _ ->  
    (* Should auto-save all buffers, and then exit ... *)
    raise SigInt
  )) |> ignore;
  Sys.signal Sys.sighup (Sys.Signal_handle (fun _ -> 
    raise SigInt
  )) |> ignore;
  (*e: [[main()]] set signal handlers *)

  let level = ref (Some Logs.Warning) in

  let options = [
    (*s: [[main()]] command line options *)
    "-width"  , Arg.Int (fun i -> width_opt := Some i), "<len>: Width in chars";
    "-height" , Arg.Int (fun i -> height_opt := Some i), "<len>: Height in chars";
    "-fg"     , Arg.String(fun s -> fg_opt := Some s), "<color>: Foreground color";
    "-bg"     , Arg.String(fun s -> bg_opt := Some s), "<color>: Background color";
    "-font"   , Arg.String(fun s -> font_opt := Some s), "<font>: Font name";
    (*x: [[main()]] command line options *)
    "-check", Arg.Set Globals.check, ": only for testing";
    (*x: [[main()]] command line options *)
      "-I",Arg.String (fun s -> Globals.load_path =:= 
          (Utils.string_to_path s) @ !!Globals.load_path), "<path>: Load Path";
    (*x: [[main()]] command line options *)
    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-debug", Arg.Unit (fun () -> 
       Globals.debug := true;
       level := Some Logs.Debug;
    ),
    " debug mode";
    "-quiet", Arg.Unit (fun () -> level := None),
    " no logs";
    "-debug_graphics", Arg.Set Globals.debug_graphics, 
    " for debugging";
    "-debug_display", Arg.Set Globals.debug_display, 
    " for debugging";
    "-debug_init", Arg.Set Globals.debug_init, 
    " for debugging";

    "-debugger", Arg.Unit (fun () ->
      Globals.debug := true;
    ), " for debugging";
    (*e: [[main()]] command line options *)
   ] 
   (* @  Common2.cmdline_flags_devel () *)
   in
   Arg.parse_argv argv options
      (fun name -> initial_files := name :: !initial_files)
   usage_str;

  (* Logs_.setup ~level:!level (); *)
  Logs.set_level !level;
  Logs.info (fun m -> m "starting logging");

  (*s: [[main()]] set options *)
  (*s: [[main()]] set options filename *)
  Options.filename := 
    (try Utils.find_in_path (Utils.homedir :: !!Globals.load_path) ".efunsrc" 
     with _ -> Filename.concat Utils.homedir ".efunsrc"
    );
  (try Options.load () 
   with exn -> Error.error_exn "init error" exn
  );
  (*e: [[main()]] set options filename *)
  (*s: [[main()]] adjust options *)
  (match !width_opt  with None -> () | Some color -> width =:= color);
  (match !height_opt with None -> () | Some color -> height =:= color);
  (match !fg_opt     with None -> () | Some color -> foreground =:= color);
  (match !bg_opt     with None -> () | Some color -> background =:= color);
  (match !font_opt   with None -> () | Some color -> Globals.font =:= color);
  (*e: [[main()]] adjust options *)
  (*e: [[main()]] set options *)

  (*s: [[main()]] initial editor *)
  let editor = { Efuns.
      edt_buffers = Hashtbl.create 13;
      edt_files = Hashtbl.create 13;

      top_windows = [];

      edt_width = !!width;
      edt_height = !!height;
      edt_fg = !!foreground;
      edt_bg = !!background;
      edt_font = !!Globals.font;

      edt_map = Keymap.create ();
      edt_dirname = Sys.getcwd ();

      edt_vars = Store.new_store ();
    
      edt_colors_names = Array.make 256 "";
      edt_colors = Hashtbl.create 37;
      edt_colors_n = 0;

      edt_fonts_names = Array.make 256 "";
      edt_fonts = Hashtbl.create 37;
      edt_fonts_n = 0;

      edt_mutex = Mutex.create ()
  } in
  (*e: [[main()]] initial editor *)
  (*s: [[main()]] initialize the world *)
  init_efuns editor (* launch first hooks *);
  (*e: [[main()]] initialize the world *)
  (*s: [[main()]] misc initializations *)
  (* color 0 is foreground *)
  Attr.get_color !!foreground |> ignore;
  (* color 1 is background *)
  Attr.get_color !!background |> ignore;
  (* color 2 is highlight *)
  Attr.get_color !!highlight_color |> ignore;
  (*x: [[main()]] misc initializations *)
  (* font 0 is initial font *)
  Attr.get_font !!Globals.font |> ignore;
  (*e: [[main()]] misc initializations *)

  (*s: [[main()]] run the UI *)
  Graphics_efuns.init !initial_files;
  (*e: [[main()]] run the UI *)
  Exit.OK
(*e: function [[Main.main]] *)

(*e: CLI.ml *)
