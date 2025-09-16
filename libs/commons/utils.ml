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

(*
  Useful functions.
*)

let count_char_sub str pos len char =
  let pos_end = pos + len in
  let rec occur pos n =
    if pos = pos_end then (n, 0) else
    let next_pos = 
      try
        String.index_from str pos char
      with
        Not_found -> -1
    in
    if next_pos = -1 || next_pos >= pos_end then (n,pos_end - pos) else
    occur (next_pos + 1) (n + 1)
  in
  occur pos 0 

let count_char str char =
  let len = String.length str in
  count_char_sub str 0 len char

let rec list_nth n list =
  match n, list with
    0, (ele :: _) -> ele
  | _, [] -> raise Not_found
  | _, (_:: tail) -> list_nth (n-1) tail


let rec mem_assq x = function
  | [] -> false
  | (a, _b) :: l -> a == x || mem_assq x l

let rec removeq x = function
  | [] -> []
  | (a, _b as pair) :: l -> if a == x then l else pair :: removeq x l

let rec list_removeq list ele =
  match list with
    e :: tail when e == ele -> list_removeq tail ele
  | e :: tail -> e :: (list_removeq tail ele)
  | _ -> []

let remove_assocq ele list =
  List.fold_left (fun list ((e,_) as head) ->
                    if e == ele then list
                    else head ::  list) [] list

let list_remove list ele =
  List.fold_left (fun list e ->
                    if e = ele then list
                    else e ::  list) [] list

let remove_assoc ele list =
  List.fold_left (fun list ((e,_) as head) ->
                    if e = ele then list
                    else head ::  list) [] list
      

(*
let rec longname dirname name =
  if is_relative name then
    longname "" (Filename.concat dirname name)
  else
    let tokens = decomp name in
    let rec iter longname tokens =
      match tokens with
        [] -> ""
      | [
    let len = String.length name in
    let rec iter i n =
    if i+n+1 >= len || name.[i+n] = '/' then
      if i+n+1 >= len then
        String.sub name i n
      else
        (String.sub name i (n-1)
      
  in
  iter 0 0

let longname curdirname name =
  let rec 
  
*)

let file_list dirname =
  try
    let dir = Unix.opendir dirname in
    let rec iter list =
      try
        let file = Unix.readdir dir in
        iter (file :: list)
      with
        _ -> list
    in
    let list = iter [] in
    Unix.closedir dir;
    list
  with
    _ -> []

let string_ncmp s1 s2 n =
  let sz1 = String.length s1 in
  let sz2 = String.length s2 in
  if sz1 < n || sz2 < n then s1 = s2
  else
    let s1' = String.sub s1 0 n in
    let s2' = String.sub s2 0 n in
    s1' = s2'

let completion list start =
  let n = String.length start in
  if n = 0 then list else
  List.fold_left (fun list s ->
    if string_ncmp start s n then
      s :: list
    else
      list
    ) [] list

let common_suffix list start =
  let newlist = completion list start in
  let lenl = List.length newlist in
  let len = String.length start in
  match newlist with
    [] -> "",0
  | ele :: tail ->
      let lenele = String.length ele in
      if tail = [] then
        String.sub ele len (lenele - len),1
      else
        let maxlen = List.fold_left (fun len e -> min len (String.length e))
            lenele tail in
        let suffix = ref "" in
        try
          for i = len to maxlen - 1  do
            let c = ele.[i] in
            List.iter (fun e -> if not (e.[i] == c) then raise Exit) tail;
            suffix := !suffix ^ (String.make 1 c)
          done;
          raise Exit
        with
          _ -> !suffix, lenl

let read_string inc =
  let len = in_channel_length inc in
  let str = Bytes.create len in
  let curs = ref 0 in
  let left = ref len in
  try
    while !left > 0 do
      let read = input inc str !curs !left in
      if read = 0 then raise End_of_file;
      left := !left - read;
      curs := !curs + read;
    done;
    Bytes.to_string str
  with End_of_file ->
    Bytes.sub_string str 0 !curs

let list_of_hash t =
  let l = ref [] in
  Hashtbl.iter (fun key ele -> l := (key,ele) :: !l) t;
  !l

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
        [] -> raise Not_found
      | dir::rem ->
          let fullname = Filename.concat dir name in
          if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end
      
let string_to_path str =
  let len = String.length str in
  let rec iter start pos =
    if pos >= len then
      [String.sub str start (len - start)]
    else
      if str.[pos] = ' ' || str.[pos] = ':' then
        (String.sub str start (pos - start)) :: (iter (pos+1) (pos+1))
      else
        iter start (pos+1)
  in
  iter 0 0

let path_to_string path =
  let s =   List.fold_left (fun str dir -> str ^ ":" ^ dir) "" path in
  if String.length s > 0 then 
    let len = String.length s in
    String.sub s 1 (len-1)
  else ""
  
let hash_add_assoc hash list =
  List.iter (fun (abbrev, repl) -> Hashtbl.add hash abbrev repl) list 


            (* Simplify a filename before printing it *)

let homedir =
  try
    (* nosemgrep: use-caps *)
    Sys.getenv "HOME"
  with
    Not_found -> ""

let known_dirs = ref [ homedir, "~"]
  
let check_prefix filename prefix =
  let len = String.length prefix in
  let n = String.length filename - len in
  if n < 0 then raise Not_found;
  for i = 0 to len-1 do
    if filename.[i] <> prefix.[i] then raise Not_found
  done;
  String.sub filename len n
      
let replace_prefix filename prefixes =
  prefixes |> List.fold_left (fun filename (dir,repl) ->
    try
      let fin = check_prefix filename dir in
      repl ^ fin
    with Not_found -> filename
  ) filename 
  
let filename_to_string filename =
(* replace known dirs by ~ *)
  replace_prefix filename !known_dirs
  
let string_to_filename filename =
(* replace known ~ by dirs *)
  replace_prefix filename (List.map (fun (a,b) -> b,a) !known_dirs)
  

let exns = ref []
let register_exn f = 
  exns := f :: !exns

let printexn exn =
  let rec iter exns =
    match exns with
    | [] -> Printexc.to_string exn
    | f :: tail ->
        try f exn 
        with  _ -> iter tail
  in
  iter !exns
  
let catchexn s f =
  try f () 
  with e -> 
    Printf.printf "Uncaught exception in %s: %s" s (printexn e);
    print_newline () 
  
let vcatchexn s f =
  try Some (f ()) 
  with  e -> 
    Printf.printf "Uncaught exception in %s: %s" s (printexn e);
    print_newline ();
    None

  
(*
let format_to_string action arg =
  let string = ref "" in
  let (p,f) = Format.get_formatter_output_functions () in
  Format.set_formatter_output_functions 
    (fun str pos len ->
      let s = String.sub str pos len in
      string := !string ^ s)
  (fun () -> ());
  try  
    action arg;
    Format.print_flush ();
    Format.set_formatter_output_functions p f;
    !string
  with
    e -> 
      Format.print_flush ();
      Format.set_formatter_output_functions p f;
      raise e
*)
      

let do_and_format action arg =
  let string = ref "" in
  let (p,f) = Format.get_formatter_output_functions () in
  Format.set_formatter_output_functions 
    (fun str pos len ->
      let s = String.sub str pos len in
      string := !string ^ s)
  (fun () -> ());
  try  
    let v = action arg in
    Format.print_flush ();
    Format.set_formatter_output_functions p f;
    !string, v
  with
    e -> 
      Format.print_flush ();
      Format.set_formatter_output_functions p f;
      raise e
      

let format_to_string action arg =
  fst (do_and_format action arg)
  
  
open Unix
  
let is_directory filename =
  try let s = Unix.stat filename in s.st_kind = S_DIR with _ -> false

let is_link filename =
  try let s = Unix.lstat filename in s.st_kind = S_LNK with _ -> false
  
let list_dir filename =
  let dir = opendir filename in
  let list = ref [] in
  try
    while true do
      list := (readdir dir) :: !list 
      done;
    assert false
  with _ -> 
      closedir dir;
      !list
      
let load_directory filename =
  let today = localtime (time ()) in
  let s = Printf.sprintf "Directory %s :\n" filename in
  let list = list_dir filename in
      List.fold_left (fun s entry ->
          let fullname = Filename.concat filename entry in
          let stats = lstat fullname in
          let perm = stats.st_perm in
          let rights = Bytes.create 10 in
          Bytes.set rights 9 (if perm land 1 = 0 then '-' else
            if perm land 2048 = 0 then 'x' else 's');
          Bytes.set rights 8 (if perm land 2 = 0 then '-' else 'w');
          Bytes.set rights 7 (if perm land 4 = 0 then '-' else 'r');
          Bytes.set rights 6 (if perm land 8 = 0 then '-'  else
            if perm land 1024 = 0 then 'x' else 's');
          Bytes.set rights 5 (if perm land 16 = 0 then '-' else 'w');
          Bytes.set rights 4 (if perm land 32 = 0 then '-' else 'r');
          Bytes.set rights 3 (if perm land 64 = 0 then '-'  else
            if perm land 512 = 0 then 'x' else 's');
          Bytes.set rights 2 (if perm land 128 = 0 then '-' else 'w');
          Bytes.set rights 1 (if perm land 256 = 0 then '-' else 'r');
          Bytes.set rights 0 (match stats.st_kind with
              S_DIR -> 'd'
            | S_CHR -> 'c'
            | S_BLK -> 'b'
            | S_REG -> '-'
            | S_LNK -> 'l'
            | S_FIFO -> 'f'
            | S_SOCK -> 's'
          );
          let time = stats.st_mtime in
          let time = localtime time in
          let date = 
            Printf.sprintf "%3s %2d %5s" 
              (match time.tm_mon with
                0 -> "Jan"
              | 1 -> "Feb"
              | 2 -> "Mar"
              | 3 -> "Apr"
              | 4 -> "May"
              | 5 -> "Jun"
              | 6 -> "Jul"
              | 7 -> "Aug"
              | 8 -> "Sep"
              | 9 -> "Oct"
              | 10 -> "Nov"
              | 11 -> "Dec"
              | _ -> "???") 
            time.tm_mday
              (if today.tm_year = time.tm_year then 
                Printf.sprintf "%02d:%02d" 
                  time.tm_hour time.tm_min
              else
                Printf.sprintf " %4d" (time.tm_year + 1900)
            )
          in
          let owner = stats.st_uid in
          let group = stats.st_gid in
(* does not work under plan9
          let owner = try (getpwuid owner).pw_name 
            with _ -> string_of_int owner in
          let group = try (getgrgid group).gr_name
            with _ -> string_of_int group in
*)
          let owner = Printf.sprintf "uid:%d" owner in
          let group = Printf.sprintf "gid:%d" group in
          let size = stats.st_size in
          let rights = Bytes.to_string rights in
          Printf.sprintf "%s %s   %8s %8s   %8d   %s   %s%s\n"
            s rights owner group size date entry 
            (if is_link fullname then 
              Printf.sprintf " -> %s" 
                (try Unix.readlink fullname with _ -> "???") else "")
      ) s (List.sort compare list) 

      
(* This function format filenames so that directory names end with / *)
let normal_name curdir filename =
  let fullname = 
    if Filename.is_relative filename 
    then Filename.concat curdir filename
    else filename
  in

(* alt: but ocaml-light (and plan9) don't have realpath
   let name = Unix.realpath fullname in
      if is_directory name 
      then name ^ "/" 
      else name
*)
  let rec iter name list level =
    let b = Filename.basename name in
    let d = Filename.dirname name in
    if b = "." || b = "" 
    then 
      if d = "/" || d = "." 
      then list 
      else iter d list level 
    else
      if b = ".." 
      then iter d list (level+1) 
      else
        if b = "/" 
        then list 
        else
          if level > 0 
          then iter d list (level-1) 
          else iter d (b :: list) 0
  in
  let list = iter fullname [] 0 in
  match list with
  | [] -> "/"
  | _ -> 
      let name = List.fold_left (fun s name -> s ^ "/" ^ name ) "" list in
      if is_directory name 
      then name ^ "/" 
      else name

        
        
(*[to_regexp_string] replace a string with * and ? (shell regexps) to
  a string for Emacs regexp library *)
let to_regexp_string s =
  let plus = ref 0 in
  let len = String.length s in
  for i = 0 to len - 1 do
    let c = s.[i] in
    match c with
      '.' | '*' | '[' | ']' -> incr plus
    | _ -> ()
  done;
  let ss = Bytes.create (len + !plus) in
  (*: let plus = ref 0 in *)
  let rec iter i j =
    if i < len then
      let c = s.[i] in
      match c with
        '*' -> Bytes.set ss j '.'; Bytes.set ss (j+1) '*'; iter (i+1) (j+2)
      | '?' -> Bytes.set ss j '.'; iter (i+1) (j+1)
      | '.'
      | '['
      | ']' -> Bytes.set ss j '\\'; Bytes.set ss (j+1) c; iter (i+1) (j+2)
      | _ -> Bytes.set ss j c; iter (i+1) (j+1)
  in
  iter 0 0;
  Bytes.to_string ss

let hashtbl_mem h key =
  try let _ = Hashtbl.find h key in true with _ -> false

(*:
let glob_to_regexp glob =
  let lexbuf = Lexing.from_string glob in
  let rec iter s =
    let token = Lexers.lexer_glob lexbuf in
    if token = "" then s else
      iter (s^token)
  in
  "^"^(iter "")^"$"
*)

  
let list_dir_normal dir =
  try
    let rec remove_dot list =
      match list with
        [] -> []
      | file :: tail ->
          if String.length file > 0 && file.[0] = '.' then remove_dot tail else
            begin
              file :: (remove_dot tail)
            end
    in
    remove_dot (list_dir dir)
  with _ -> []
