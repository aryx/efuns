open Efuns

(*****************************************************************************)
(* Draw API *)
(*****************************************************************************)

(* helper *)
let move_to col line =
  failwith "TODO"

let clear_eol col line len =
  failwith "TODO"

let draw_string col line  str  offset len   attr =
  failwith "TODO"

let update_displays () =
  failwith "TODO"

let backend = { Xdraw. clear_eol; draw_string; update_displays }

(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

let init location displayname =
  let display = "" in
  let top_window = Top_window.create location display in
  top_window.graphics <- Some backend;

  WX_xterm.setHighlight display 2;
  Dyneval.init true;
  Eval.load top_window "Efunsrc";
  Efuns.init location; (* launch second hooks *)

  let _ = Interactive.create_bindings location in

  (* open the first buffers *)
  !init_files |> List.iter (fun name ->
    let _ = Frame.load_file top_window.window name in ()
  );
  !init_frames |> List.iter (fun str -> 
    let top_window = Top_window.create top_window.top_location
      (Window.display top_window)
    in
    let _ = Frame.load_file top_window.window str in ()
  );

  Top_window.update_display location;

  if not (Sys.file_exists (Filename.concat Utils.homedir ".efunsrc")) then
    begin
      Printf.printf "Saving .efunsrc after install"; print_newline ();
      Options.save ();
    end;

  if !check then exit 0; 

  (* Main loop *)
  let rec loop () =
    try
      WX_types.loop ()
    with
      SigInt -> loop ()
  in
  loop ()
