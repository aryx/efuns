(*s: core/ebuffer.ml *)
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

(*s: type [[Ebuffer.t]] *)
type t = Efuns.buffer
(*e: type [[Ebuffer.t]] *)

(* this file is called ebuffer.ml because buffer.ml already exists in stdlib *)

(*s: constant [[Ebuffer.create_buf_hook]] *)
let create_buf_hook = Store.create_abstr "create_buf_hook"
(*e: constant [[Ebuffer.create_buf_hook]] *)
(*s: constant [[Ebuffer.modes_alist]] *)
let modes_alist = Store.create_abstr "modes_alist"
(*e: constant [[Ebuffer.modes_alist]] *)

(*s: function [[Ebuffer.create_syntax_table]] *)
let create_syntax_table ()  =
  let table = Array.make 256 false 
  in  
  for i = Char.code 'a' to Char.code 'z' do
    table.(i) <- true;
  done;
  for i = Char.code 'A' to Char.code 'Z' do
    table.(i) <- true;
  done;
  for i = Char.code '0' to Char.code '9' do
    table.(i) <- true;
  done;
  table
(*e: function [[Ebuffer.create_syntax_table]] *)

(*s: constant [[Ebuffer.default_syntax_table]] *)
let default_syntax_table = create_syntax_table ()
(*e: constant [[Ebuffer.default_syntax_table]] *)

(*s: function [[Ebuffer.get_name]] *)
let get_unique_name filename =
  let basename = Filename.basename filename in
  let name = 
    if basename = "" 
    then (Filename.basename (Filename.dirname filename)) ^ "/"
    else basename
  in
  let i = ref 0 in
  let compute_name () =
    if !i =|= 0 
    then name 
    else Printf.sprintf "%s<%d>" name !i
  in
  try
    while true do
      let _ = Hashtbl.find (Globals.editor()).edt_buffers (compute_name ()) 
      in 
      incr i
    done; 
    assert false
  with Not_found -> 
    compute_name ()
(*e: function [[Ebuffer.get_name]] *)

(*s: function [[Ebuffer.new_minor_mode]] *)
let new_minor_mode name hooks  = {
    min_name = name;
    min_map = Keymap.create ();
    min_hooks = hooks;
    min_vars = Store.new_store ()
  }
(*e: function [[Ebuffer.new_minor_mode]] *)

(*s: function [[Ebuffer.new_major_mode]] *)
let new_major_mode name hook_opt = {
    maj_name = name;
    maj_map = Keymap.create ();
    maj_hooks = (match hook_opt with None -> [] | Some hook -> [hook]);
    maj_vars = Store.new_store ();
  }
(*e: function [[Ebuffer.new_major_mode]] *)

(*s: constant [[Ebuffer.fondamental_mode]] *)
let fondamental__mode = new_major_mode "Fondamental" None (* no hooks *)
(*e: constant [[Ebuffer.fondamental_mode]] *)


  
(*s: constant [[Ebuffer.tab_size]] *)
let tab_size = ref 9
(*e: constant [[Ebuffer.tab_size]] *)

(*s: function [[Ebuffer.create]] *)
let create name filename text local_map =
  let name = get_unique_name name in
  let buf =
    { 
      buf_text = text;

      buf_name = name;
      buf_filename = filename;

      buf_point = Text.new_point text;
      buf_start = Text.new_point text;

      buf_last_saved = Text.version text;
      buf_modified = 0;

      buf_map = local_map;

      buf_syntax_table = default_syntax_table;
      buf_map_partial = true;
      buf_vars = Store.new_store ();
      buf_major_mode = fondamental__mode;
      buf_minor_modes = [];

      buf_sync = false;
      buf_mark = None;
      buf_shared = 0;
      buf_finalizers = [];
      buf_history_pos = [||];

      (*s: [[Ebuffer.create()]] buffer other fields setup *)
      buf_charreprs = Array.init 256 (fun i -> String.make 1 (Char.chr i));
      (*e: [[Ebuffer.create()]] buffer other fields setup *)

    } in
  (*s: [[Ebuffer.create()]] adjust editor global fields *)
  Hashtbl.add (Globals.editor()).edt_buffers name buf;
  (*e: [[Ebuffer.create()]] adjust editor global fields *)
  (*s: [[Ebuffer.create()]] adjust charreprs *)
  for i=0 to 25 do
    let s = Bytes.make 2 '^' in
    Bytes.set s 1 (Char.chr (97+i));    
    buf.buf_charreprs.(i) <- (Bytes.to_string s);
  done;
  (*x: [[Ebuffer.create()]] adjust charreprs *)
  buf.buf_charreprs.(9) <- String.make !tab_size ' ';
  (*e: [[Ebuffer.create()]] adjust charreprs *)
  (*s: [[Ebuffer.create()]] run hooks *)
  let hooks = try Var.get_global create_buf_hook with Not_found -> [] in
  Hooks.exec_hooks hooks buf;
  (*e: [[Ebuffer.create()]] run hooks *)
  buf
(*e: function [[Ebuffer.create]] *)

(*s: function [[Ebuffer.kill]] *)
let kill buf =
  let edt = Globals.editor() in
  Hashtbl.remove edt.edt_buffers buf.buf_name;
  buf.buf_filename |> Option.iter (fun filename ->
    Hashtbl.remove edt.edt_files filename
  );
  List.iter (fun f -> f () ) buf.buf_finalizers;
(* TODO Gc.compact (); this cause some segfault under plan9 with ocaml light *)
  buf.buf_shared <- -1
(*e: function [[Ebuffer.kill]] *)

open Options
  
(*s: constant [[Ebuffer.save_buffer_hooks]] *)
let save_buffer_hooks = define_option ["save_buffer_hooks"] "" 
    (list_option string_option)
  [ ]
(*e: constant [[Ebuffer.save_buffer_hooks]] *)
  
(*s: constant [[Ebuffer.saved_buffer_hooks]] *)
let saved_buffer_hooks = Store.create_abstr "saved_buffer_hooks"
(*e: constant [[Ebuffer.saved_buffer_hooks]] *)

(*s: function [[Ebuffer.save]] *)
let save buf =
  Hooks.exec_named_buf_hooks_with_abort !!save_buffer_hooks buf;

  let filename =
    match buf.buf_filename with
      None -> raise Not_found
    | Some name -> name
  in
  let outc = open_out filename in
  Text.save buf.buf_text outc;
  close_out outc;
  buf.buf_last_saved <- Text.version buf.buf_text;

  let hooks = try Var.get_var buf saved_buffer_hooks with Not_found -> [] in
  Hooks.exec_hooks hooks buf
(*e: function [[Ebuffer.save]] *)
 
(*s: function [[Ebuffer.read]] *)
let read filename local_map =
  let edt = Globals.editor() in
  let filename = Utils.normal_name edt.edt_dirname filename in
  try
    Hashtbl.find edt.edt_files filename
  with Not_found ->
    let text =
      try
        let inc = open_in filename in
        let text = Text.read inc in         
        close_in inc; 
        text
      with exn -> 
        Error.error_exn (spf "error reading file %s" filename) exn;
        Text.create ""
    in
    let buf = create filename (Some filename) text local_map in
    Hashtbl.add edt.edt_files filename buf;
    buf
(*e: function [[Ebuffer.read]] *)

(*s: function [[Ebuffer.find_buffer_opt]] *)
let find_buffer_opt name =
  try Some (Hashtbl.find (Globals.editor()).edt_buffers name)
  with Not_found -> None
(*e: function [[Ebuffer.find_buffer_opt]] *)

(*s: constant [[Ebuffer.help_buffer_content]] *)
let help_buffer_content = 
"Welcome to Efuns, a small demo editor written in Ocaml.

Fabrice Le Fessant
PARA/SOR Project
INRIA Rocquencourt
"
(*e: constant [[Ebuffer.help_buffer_content]] *)

(*s: function [[Ebuffer.default]] *)
let default name =
  try
    Hashtbl.find (Globals.editor()).edt_buffers name
  with Not_found ->
    let str = 
      if name = "*help*" 
      then help_buffer_content
      else ""
    in
    create name None (Text.create str) (Keymap.create ())
(*e: function [[Ebuffer.default]] *)
      
(*s: function [[Ebuffer.compute_representation]] *)
let compute_representation buf n =
  Text.compute_representation buf.buf_text buf.buf_charreprs n
(*e: function [[Ebuffer.compute_representation]] *)

(*s: exception [[Ebuffer.BufferAlreadyOpened]] *)
exception BufferAlreadyOpened
(*e: exception [[Ebuffer.BufferAlreadyOpened]] *)

(*s: function [[Ebuffer.change_name]] *)
let change_name buf filename =
  let edt = Globals.editor() in
  Hashtbl.remove edt.edt_buffers buf.buf_name;
  buf.buf_filename |> Option.iter (fun filename ->
    Hashtbl.remove edt.edt_files filename
  );
  let filename = 
    if Filename.is_relative filename 
    then Filename.concat edt.edt_dirname filename
    else filename
  in
  if Utils.hashtbl_mem edt.edt_files filename
  then raise BufferAlreadyOpened;
  let filename = Utils.normal_name edt.edt_dirname filename in
  let name = get_unique_name filename in
  Hashtbl.add edt.edt_buffers name buf;
  Hashtbl.add edt.edt_files filename buf;
  buf.buf_filename <- Some filename;
  buf.buf_name <- name
(*e: function [[Ebuffer.change_name]] *)
  
(*s: function [[Ebuffer.set_mark]] *)
let set_mark buf point =
  let text = buf.buf_text in
  buf.buf_modified <- buf.buf_modified + 1;
  match buf.buf_mark with
  | None ->
      let mark = Text.dup_point text point in
      buf.buf_mark <- Some mark
  | Some mark ->
      Text.goto_point text mark point
(*e: function [[Ebuffer.set_mark]] *)

(*s: function [[Ebuffer.get_mark]] *)
let rec get_mark buf point =
  match buf.buf_mark with
  | None -> 
      set_mark buf point;
      get_mark buf point
  | Some mark -> mark
(*e: function [[Ebuffer.get_mark]] *)

(*s: function [[Ebuffer.remove_mark]] *)
let remove_mark buf =
  buf.buf_mark |> Option.iter (fun mark ->
    buf.buf_mark <- None;
    Text.remove_point buf.buf_text mark;
    buf.buf_modified <- buf.buf_modified + 1
  )
(*e: function [[Ebuffer.remove_mark]] *)

(*s: constant [[Ebuffer.modes_old]] *)
let modes_old = ref []
(*e: constant [[Ebuffer.modes_old]] *)
(*s: constant [[Ebuffer.regexp_alist]] *)
let regexp_alist = ref []
(*e: constant [[Ebuffer.regexp_alist]] *)

(*s: function [[Ebuffer.set_major_mode]] *)
let set_major_mode buf mode =
  Logs.info (fun m -> m "setting %s major mode" mode.maj_name);
  buf.buf_modified <- buf.buf_modified + 1;
  buf.buf_major_mode <- mode;
  mode.maj_hooks |> List.iter (fun f -> 
    try f buf 
    with exn -> Error.error_exn "set_major_mode" exn
  )
(*e: function [[Ebuffer.set_major_mode]] *)

(*s: function [[Ebuffer.set_minor_mode]] *)
let set_minor_mode buf mode =
  buf.buf_minor_modes <- mode :: buf.buf_minor_modes;
  buf.buf_modified <- buf.buf_modified + 1;
  mode.min_hooks |> List.iter (fun f -> 
    try f buf 
    with exn -> Error.error_exn "set_minor_mode" exn
  ) 
(*e: function [[Ebuffer.set_minor_mode]] *)

(*s: function [[Ebuffer.del_minor_mode]] *)
let del_minor_mode buf minor =
  buf.buf_minor_modes <- 
    List.fold_right (fun mode list -> 
      if Eq.phys_equal mode minor then begin
        buf.buf_modified <- buf.buf_modified + 1;
        list
      end else (mode :: list)
    ) buf.buf_minor_modes []
(*e: function [[Ebuffer.del_minor_mode]] *)
  
(*s: function [[Ebuffer.has_minor_mode]] *)
let has_minor_mode buf minor =
  List.memq minor buf.buf_minor_modes
(*e: function [[Ebuffer.has_minor_mode]] *)

(*s: constant [[Ebuffer.suffix_reg]] *)
let suffix_reg = Str.regexp "\\(.*\\)<[0-9]+>$"
(*e: constant [[Ebuffer.suffix_reg]] *)
  
(*s: function [[Ebuffer.set_buffer_mode]] *)
let set_buffer_mode buf =
  let buf_name = 
    match buf.buf_filename with
      None -> 
        (try
           if Str.string_match suffix_reg buf.buf_name 0 
           then Str.matched_group 1 buf.buf_name 
           else buf.buf_name 
         with exn -> 
           Error.error_exn "set_buffer_mode" exn;
           buf.buf_name
         )
    | Some file_name -> file_name 
  in 
  let modes_alist = Var.get_var buf modes_alist in
  (* must use != here, because modes_alist contain functional values *)
  if (Eq.phys_not_equal !modes_old modes_alist) then begin
    regexp_alist := modes_alist |> List.map (fun (file_reg, major) ->
      Str.regexp file_reg, major
    );
    modes_old := modes_alist;
  end;
  try
    !regexp_alist |> List.iter (fun (regexp, major) ->
      if Str.string_match regexp buf_name 0 
      then
        try
          set_major_mode buf major;
          raise Exit
        with 
        | Exit -> raise Exit
        | exn -> 
          Error.error_exn "set_buffer_mode" exn;
          raise Exit
    ) 
  with Exit -> ()
(*e: function [[Ebuffer.set_buffer_mode]] *)
      
(*s: function [[Ebuffer.get_binding]] *)
let get_binding buf keylist =
  let binding = ref Unbound in
  try
    (*s: [[Ebuffer.get_binding()]] minor mode key search *)
    buf.buf_minor_modes |> List.iter (fun minor ->
      let b = Keymap.get_binding minor.min_map keylist in
      match b with
        Prefix _map -> binding := b
      | Function _f -> binding := b; raise Exit
      | Unbound -> ()
    ); 
    (*e: [[Ebuffer.get_binding()]] minor mode key search *)
    (*s: [[Ebuffer.get_binding()]] major mode key search *)
    (let b = Keymap.get_binding buf.buf_major_mode.maj_map keylist in
      match b with
        Prefix _map -> binding := b
      | Function _f -> binding := b; raise Exit
      | Unbound -> ());
    (*e: [[Ebuffer.get_binding()]] major mode key search *)
    (let b = Keymap.get_binding buf.buf_map keylist in
      match b with
      | Prefix _map -> binding := b;
      | Function _f -> binding := b; raise Exit
      | Unbound -> ()
    );
    (*s: [[Ebuffer.get_binding()]] if partial map *)
    if buf.buf_map_partial then
      (let b = Keymap.get_binding (Globals.editor()).edt_map keylist in
        match b with
        | Prefix _map -> binding := b;
        | Function _f -> binding := b; raise Exit
        | Unbound -> ()
      );
    (*e: [[Ebuffer.get_binding()]] if partial map *)
    !binding
  with Exit -> !binding
(*e: function [[Ebuffer.get_binding]] *)

(*s: function [[Ebuffer.message]] *)
(* todo: vs Message.message? *)
let message _buf m =
  let name = "*Messages*" in
  try
    let buf = Hashtbl.find (Globals.editor()).edt_buffers name in
    Text.insert_at_end buf.buf_text (m^"\n");
  with Not_found ->
    create name None (Text.create (m^"\n")) (Keymap.create ()) |> ignore
(*e: function [[Ebuffer.message]] *)


(*s: function [[Ebuffer.fondamental_mode]] *)
let fondamental_mode frame =
  set_major_mode frame.frm_buffer fondamental__mode
[@@interactive]
(*e: function [[Ebuffer.fondamental_mode]] *)

      
(*s: toplevel [[Ebuffer]] starting hook *)
let _ =
  Hooks.add_start_hook (fun () ->
    Var.set_global create_buf_hook [set_buffer_mode];
    Var.set_global modes_alist []
  )
(*e: toplevel [[Ebuffer]] starting hook *)
(*e: core/ebuffer.ml *)
