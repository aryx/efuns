(*s: efuns_client.ml *)
  
(*s: constant [[Efuns_client.pos]] *)
let pos = ref 0
(*e: constant [[Efuns_client.pos]] *)
let line = ref 0
(*s: constant [[Efuns_client.string]] *)
let string = ref ""
(*e: constant [[Efuns_client.string]] *)
(*s: constant [[Efuns_client.filename]] *)
let filename = ref None
(*e: constant [[Efuns_client.filename]] *)
  
(*s: toplevel [[Efuns_client._1]] *)
let _ = Arg.parse [
    "-pos", Arg.Int (fun i -> pos := i)," <int>: set position";
    "-line", Arg.Int (fun i -> line := i)," <int>: set line";
    "-str", Arg.String (fun s -> string := s)," <string>: search string";
  ] (fun name -> filename := Some name)
  "efuns_server: connect to efuns"
(*e: toplevel [[Efuns_client._1]] *)
  
(*s: constant [[Efuns_client.pos]]([[(client/efuns_client.ml)]]) *)
let pos = !pos    
(*e: constant [[Efuns_client.pos]]([[(client/efuns_client.ml)]]) *)
(*s: constant [[Efuns_client.filename]]([[(client/efuns_client.ml)]]) *)
let filename = match !filename with
    None -> failwith "efuns_server: filename missing"
  | Some s -> s
(*e: constant [[Efuns_client.filename]]([[(client/efuns_client.ml)]]) *)
(*s: constant [[Efuns_client.string]]([[(client/efuns_client.ml)]]) *)
let string = !string
(*e: constant [[Efuns_client.string]]([[(client/efuns_client.ml)]]) *)
    
(*s: constant [[Efuns_client.filename]] ([[client/efuns_client.ml]])2 *)
let filename = 
  if Filename.is_relative filename 
  then Filename.concat (Sys.getcwd ()) filename 
  else filename
(*e: constant [[Efuns_client.filename]] ([[client/efuns_client.ml]])2 *)
    
(*s: constant [[Efuns_client.efuns_property]] *)
let _efuns_property = "_EFUNS_SERVER"  
(*e: constant [[Efuns_client.efuns_property]] *)

(*s: constant [[Efuns_client.display]] *)
let display = 
  ""
  (*Xlib.openDisplay "" *)
(*e: constant [[Efuns_client.display]] *)
(*s: constant [[Efuns_client.root]] *)
(*let root = display.dpy_roots.(0).scr_root*)
(*e: constant [[Efuns_client.root]] *)
(*s: constant [[Efuns_client.atom]] *)
(*let atom = X.internAtom display efuns_property false*)
(*e: constant [[Efuns_client.atom]] *)
(*s: constant [[Efuns_client.socket_name]] *)
let user = 
  try Sys.getenv "USER" 
  with _ -> "noname"

let socket_name = 
  (Printf.sprintf "/tmp/efuns-server.%s.%s:0" user display)
  (* (Xlib.getWholeProperty display root atom).gp_value *)
(*e: constant [[Efuns_client.socket_name]] *)
(*s: toplevel [[Efuns_client._2]] *)
let _ =  if not (Sys.file_exists socket_name) then raise Not_found
(*e: toplevel [[Efuns_client._2]] *)
(*s: toplevel [[Efuns_client._3]] *)
let (_inc,outc) = Unix.open_connection (Unix.ADDR_UNIX socket_name)
let _ =  
  output_value outc (Server.LoadFile (filename,pos,!line, string)); 
  flush outc
(*e: toplevel [[Efuns_client._3]] *)
  
(*e: efuns_client.ml *)
