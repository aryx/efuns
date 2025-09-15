(*s: features/multi_buffers.ml *)
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
open Efuns

(*s: function [[Simple.buffer_list]] *)
let buffer_list () =
  (Globals.editor()).edt_buffers |> Hashtbl_.to_list |> List.map fst
(*e: function [[Simple.buffer_list]] *)

(*s: constant [[Select.prev_buffers]] *)
(* todo: maybe this should be in frame.ml, next to change_buffer,
 * so change_buffer can maintain it instead of imposing to everyone
 * to use set_previous_frame just before Frame.change_buffer.
 * todo: or do via change_buffer_hook?
 *)
let prev_buffers = ref []
(*e: constant [[Select.prev_buffers]] *)
(*s: function [[Select.set_previous_frame]] *)
let set_previous_frame frame = 
  let name = frame.frm_buffer.buf_name in
  prev_buffers := name :: (Utils.list_removeq !prev_buffers name)
(*e: function [[Select.set_previous_frame]] *)
(*s: function [[Select.get_previous_frame]] *)
let get_previous_frame () = 
  match !prev_buffers with
  | [] -> ""
  | x::_xs -> x
(*e: function [[Select.get_previous_frame]] *)

(*s: function [[Multi_buffers.switch_to_other_buffer]] *)
(* C-M-l *)
let switch_to_other_buffer (frame : Frame.t) : unit =
  let default = get_previous_frame () in
  set_previous_frame frame;
  Frame.change_buffer frame.caps frame.frm_window default
(*e: function [[Multi_buffers.switch_to_other_buffer]] *)


(*s: constant [[Select.buf_hist]] *)
let buf_hist = ref []
(*e: constant [[Select.buf_hist]] *)
(*s: function [[Select.select_buffer]] *)
let select_buffer frame request default action =
  Select.select frame (request^"(default :"^ default ^ ") ") buf_hist ""
    (fun _ -> buffer_list ()) 
    (fun s -> s) 
    (fun str ->
      let str = if str = "" then default else str in
      action str)
(*e: function [[Select.select_buffer]] *)


(*s: function [[Simple.next_buffer]] *)
(* useful when want to iterate over all buffers, e.g. in dabbrev_expand *)
let next_buffer buf =
  let buf_list = Utils.list_of_hash (Globals.editor()).edt_buffers in
  let rec iter list =
    match list with
      [] -> raise Not_found 
    | (_name,b) :: tail ->
        if Eq.phys_equal b buf 
        then 
          match tail with
          | [] -> snd (List.hd buf_list) (* go back to head *)
          | (_,b)::_ -> b
        else
          iter tail
  in
  iter buf_list
(*e: function [[Simple.next_buffer]] *)

(*s: function [[Simple.kill_buffer]] *)
let kill_buffer (frame : Frame.t) : unit =
  let window = frame.frm_window in
  let buf = frame.frm_buffer in

  let new_buf = 
    match !prev_buffers with
    | x::xs when x <> buf.buf_name -> 
        prev_buffers := xs;
        (try 
          Hashtbl.find (Globals.editor()).edt_buffers x
        with Not_found -> 
          next_buffer buf
        )
    | _ -> next_buffer buf 
  in

  let _new_frame = Frame.create frame.caps window None new_buf in
  if buf.buf_shared =|= 0 
  then Ebuffer.kill buf
[@@interactive]
(*e: function [[Simple.kill_buffer]] *)

(*s: constant [[Complex.up_buffer]] *)
let up_buffer = ref ""
(*e: constant [[Complex.up_buffer]] *)
(*s: function [[Complex.down_buffer]] *)
let down_buffer frame = 
  up_buffer := frame.frm_buffer.buf_name
[@@interactive]
(*e: function [[Complex.down_buffer]] *)

(*s: function [[Complex.up_buffer]] *)
let up_buffer (frame : Frame.t) : unit =
  if !up_buffer = "" 
  then raise Not_found;
  set_previous_frame frame;
  Frame.change_buffer frame.caps frame.frm_window !up_buffer
[@@interactive]
(*e: function [[Complex.up_buffer]] *)
  
(*s: function [[Complex.left_buffer]] *)
let left_buffer (frame : Frame.t) : unit =
  set_previous_frame frame;
  Frame.change_buffer frame.caps frame.frm_window 
    (match !prev_buffers with
    | name :: buffer :: tail ->
        prev_buffers := tail @ [name]; 
        buffer
    | _ -> raise Not_found
    )
[@@interactive]
(*e: function [[Complex.left_buffer]] *)

(*s: function [[Complex.right_buffer]] *)
let right_buffer (frame : Frame.t) : unit =
  set_previous_frame frame;
  Frame.change_buffer frame.caps frame.frm_window 
    (match !prev_buffers with
    | name :: tail ->
        (match List.rev tail with
        | buffer :: tail ->
            prev_buffers := name :: (List.rev tail);
            buffer
        | _ -> raise Not_found
        )
    | _ -> raise Not_found
    )
[@@interactive]
(*e: function [[Complex.right_buffer]] *)

(*s: function [[Complex.change_buffer]] *)
let change_buffer (frame : Frame.t) : unit =
  let default = get_previous_frame () in
  set_previous_frame frame;
  select_buffer frame " Switch to buffer: " default (fun str ->
    Frame.change_buffer frame.caps frame.frm_window str
  )
[@@interactive]
(*e: function [[Complex.change_buffer]] *)

(* was in simple.ml or complexe.ml before *)

(*s: function [[Complex.save_buffers_and_action]] *)
let rec save_buffers_and_action frame buffers action =
  match buffers with
    [] -> let () = action frame in ()
  | (_,buf) :: buffers ->
      let text = buf.buf_text in
      if buf.buf_last_saved =|= Text.version text  ||
        buf.buf_name.[0] =$= '*'
      then
        save_buffers_and_action frame buffers action
      else
      let map = Keymap.create () in
      let request = Printf.sprintf "Save buffer %s ? (y,n,!,a)" buf.buf_name
      in
      let yes mini_frame =
        Minibuffer.kill mini_frame frame;
        Ebuffer.save buf;
        save_buffers_and_action frame buffers action
      in
      let no mini_frame =
        Minibuffer.kill mini_frame frame;
        save_buffers_and_action frame buffers action; ()
      in
      let action_immediately mini_frame = 
        Minibuffer.kill mini_frame frame;
        let () = action mini_frame in ()
      in
      let abort mini_frame =
        Minibuffer.kill mini_frame frame
      in
      Keymap.add_binding map [NormalMap, Char.code 'y'] yes;
      Keymap.add_binding map [NormalMap, Char.code 'Y'] yes;
      Keymap.add_binding map [NormalMap, Char.code 'n'] no;
      Keymap.add_binding map [NormalMap, Char.code 'N'] no;
      Keymap.add_binding map [NormalMap, Char.code '!'] action_immediately;
      Keymap.add_binding map [NormalMap, Char.code 'a'] abort;
      Keymap.add_binding map [NormalMap, Char.code 'A'] abort;
      Keymap.add_binding map [ControlMap, Char.code 'g'] abort;
      Minibuffer.create frame map request |> ignore
(*e: function [[Complex.save_buffers_and_action]] *)

(*s: function [[Complex.save_some_buffers]] *)
let save_some_buffers frame =
  let buffers = Utils.list_of_hash (Globals.editor()).edt_buffers in
  save_buffers_and_action frame buffers (fun _ -> ())
[@@interactive]
(*e: function [[Complex.save_some_buffers]] *)

(*s: function [[Complex.load_buffer]] *)
let load_buffer (frame : Frame.t) : unit = 
  set_previous_frame frame;
  Select.select_file_from_pwd frame "Find file: " (fun str -> 
    Frame.load_file frame.caps frame.frm_window str |> ignore
  )
[@@interactive]
(*e: function [[Complex.load_buffer]] *)

(*s: function [[Complex.insert_file]] *)
let insert_file frame =
  Select.select_file_from_pwd frame "Insert file: " (fun str ->
    let inc = open_in str in
    Edit.insert_string frame (Utils.read_string inc);
    close_in inc
  )
[@@interactive]
(*e: function [[Complex.insert_file]] *)

(*s: function [[Complex.write_buffer]] *)
let write_buffer frame = 
  let buf = frame.frm_buffer in
  Select.select_file_from_pwd frame "Save file as: " (fun str -> 
    Ebuffer.change_name buf str;
    Ebuffer.save buf
  )
[@@interactive]
(*e: function [[Complex.write_buffer]] *)

(*s: function [[Complex.save_buffer]] *)
let save_buffer frame =
  let buf = frame.frm_buffer in
  match buf.buf_filename with
  | Some _ -> Ebuffer.save buf
  | None -> write_buffer frame
[@@interactive]
(*e: function [[Complex.save_buffer]] *)

(*e: features/multi_buffers.ml *)
