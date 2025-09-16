(*s: features/system.ml *)
open Common
open Eq.Operators
open Efuns

type caps = < Cap.fork; Cap.exec; Cap.wait; Cap.kill; Cap.chdir >

type end_action = (Efuns.buffer -> int -> unit)

(* Takes pwd in parameter. The alternative is to do
 * a Unix.chdir just before the command.
 *)

(*s: function [[System.open_process]] *)
let open_process (caps : < caps; .. >) (pwd : string) (cmd : string) : Proc.pid * in_channel * out_channel =
  let (in_read, in_write) = Unix.pipe() in
  let (out_read, out_write) = Unix.pipe() in
  let inchan = Unix.in_channel_of_descr in_read in
  let outchan = Unix.out_channel_of_descr out_write in
  match CapUnix.fork caps () with
  | 0 ->
      if out_read <> Unix.stdin then begin
        Unix.dup2 out_read Unix.stdin; 
        Unix.close out_read 
      end;
      if in_write <> Unix.stdout ||  in_write <> Unix.stderr then begin
        if in_write <> Unix.stdout 
        then Unix.dup2 in_write Unix.stdout;
        if in_write <> Unix.stderr 
        then Unix.dup2 in_write Unix.stderr; 
        Unix.close in_write 
      end;
      List.iter Unix.close [in_read;out_write];
      (* I prefer to do it here than in the caller *)
      CapSys.chdir caps pwd;
      CapUnix.execv caps "/bin/sh" [| "/bin/sh"; "-c"; cmd |];
      (* never here! *)
      (* nosemgrep: do-not-use-exit *)
      exit 127 (* for ocaml light *)
  | pid -> 
      Unix.close out_read;
      Unix.close in_write;
      (pid, inchan, outchan)
(*e: function [[System.open_process]] *)

(*s: function [[System.system]] *)
let system (caps : < caps; .. >) (pwd : string) (buf_name : string) (cmd : string) end_action =
  let (pid,inc,outc) = open_process caps pwd cmd in
  let text = Text.create "" in
  let curseur = Text.new_point text in
  let buf = Ebuffer.create buf_name None text (Keymap.create ()) in
  (* !!! *)
  buf.buf_sync <- true;

  let ins = Unix.descr_of_in_channel inc in
  let tampon = Bytes.create 1000 in
  let active = ref true in
  let edt = Globals.editor () in
  Concur.add_reader ins (fun () ->
    let _pos,str = Text.delete_res text curseur
                    (Text.point_to_eof text curseur) in
    let len = input inc tampon 0 1000 in
    Mutex.lock edt.edt_mutex;
    if len =|= 0 then begin
      let _pid,status = CapUnix.waitpid caps [Unix.WNOHANG] pid in
      (match status with 
      | Unix.WEXITED s -> 
          Text.insert_at_end text (spf "Exited with status %d\n" s); 
          close_in inc;
          close_out outc;
          (try end_action buf s with _ -> ())
      | _ -> Text.insert_at_end text "Broken pipe" 
      );
      Text.set_position text curseur (Text.size text);
      active := false;
      (* redraw screen *)
      Top_window.update_display ();

      Mutex.unlock edt.edt_mutex;
      Concur.remove_reader ins; (* Kill self *)
    end
    else
      Text.insert_at_end text (Bytes.sub_string tampon 0 len);

    Text.set_position text curseur (Text.size text);
    Text.insert text curseur str;
    buf.buf_modified <- buf.buf_modified +1;

    (* redraw screen *)
    Top_window.update_display ();
    Mutex.unlock edt.edt_mutex
  );

  let lmap = buf.buf_map in
  Keymap.add_binding lmap [NormalMap, XK.xk_Return] (fun frame ->
    let point = frame.frm_point in
    Text.insert text point "\n";
    Text.fmove text point 1;
    if !active then (* to avoid a segmentation fault in Ocaml *) begin
      let str = Text.sub text curseur 
          (Text.point_to_eof text curseur) in
      Text.set_position text curseur (Text.size text);
      (* synchronize viewpoint *)
      output_substring outc str 0 (String.length str);
      flush outc
    end
  );
  (*s: [[System.system()]] set finalizer, to intercept killed frame *)
  buf.buf_finalizers <- (fun () -> 
    (try 
       CapUnix.kill caps pid Sys.sigkill;
       CapUnix.waitpid caps [] pid |> ignore;
     with _ -> ()
    );
    Concur.remove_reader ins
  ) :: buf.buf_finalizers;
  (*e: [[System.system()]] set finalizer, to intercept killed frame *)
  buf
(*e: function [[System.system]] *)

(*s: function [[System.start_command]] *)
let start_command (caps : < caps; Cap.env; .. >) (pwd : string) (buf_name : string)
  window cmd end_action_opt =
  let end_action =
    match end_action_opt with
    | None  -> (fun _buf _status -> ())
    | Some f -> f
  in
  let buf = system caps pwd buf_name cmd end_action in
  let frame = Frame.create caps window None buf in
  frame
(*e: function [[System.start_command]] *)

(*s: constant [[System.shell_hist]] *)
let shell_hist = ref []
(*e: constant [[System.shell_hist]] *)
(*s: function [[System.shell_command]] *)
let shell_command (frm : Frame.t) =
  Select.select_string frm "Run command:" shell_hist "" (fun cmd -> 
    let pwd = (Globals.editor()).edt_dirname in
    start_command frm.caps pwd "*Command*" (Multi_frames.cut_frame frm) cmd None |> ignore)
[@@interactive]
(*e: function [[System.shell_command]] *)
  
(*e: features/system.ml *)
