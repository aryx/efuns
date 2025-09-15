{
(***********************************************************************)
(*                                                                     *)
(*                           Efuns                                     *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(***********************************************************************)
(* Lexing *)
(***********************************************************************)
open Lexing

type token =
  ANY
| COMMENT
| MATHMODE
| STARTMATH
| ENDMATH
| STARTTABBING
| ENDTABBING
| KWDTABBING
| STARTARRAY
| ENDARRAY
| KWDARRAY
| STARTENV
| ENDENV
| CONDITION    
| ITEM
| PARAMETER
| COMMAND
| FONT
| EOF
| LBRACE
| RBRACE

let lexer_start = ref 0
let position lexbuf =
  let b = lexeme_start lexbuf in
  let e = lexeme_end lexbuf in
  b + !lexer_start, e - b

let lexing text start_point end_point =
  lexer_start := Text.get_position text start_point;
  Text.lexing text start_point end_point

}

rule token = parse
(* comments *)
    '%'+ [^ '\n']* '\n' { position lexbuf, COMMENT }
(* Math mode *)
  | "$" | "$$" { position lexbuf, MATHMODE }
  
  | "\\(" | "\\[" { position lexbuf, STARTMATH }
  
  |  "\\)" | "\\]" { position lexbuf, ENDMATH }

    (* Tabbing *)
  | "\\begin" ' '* "{tabbing}" { position lexbuf, STARTTABBING }
  | "\\end" ' '* "{tabbing}"   { position lexbuf, ENDTABBING }
  | "\\>" | "\\="
  | "\\kill"  { position lexbuf, KWDTABBING }

(* tables and array *)
  | "\\begin" ' '* ("{tabular}" | "{array}") { position lexbuf, STARTARRAY }
  | "\\end" ' '* ("{tabular}" | "{array}")   { position lexbuf, ENDARRAY }
  | "&"                                      { position lexbuf, KWDARRAY }
      
(* environments *)
  |   "\\begin" " "* "{" ['A'-'Z' 'a'-'z']+ '*'?"}"
      { position lexbuf, STARTENV }
  |  "\\end" " " * "{" ['A'-'Z' 'a'-'z']+ '*'? "}"
      { position lexbuf, ENDENV }    
  | "\\item" { position lexbuf, ITEM }
(* Ignore font definitions ... *)
  | "\\font" "\\" ['A'-'Z' 'a'-'z']+ ' '* '='? ' '* ['a'-'z' 'A'-'Z' '0'-'9']+
      { position lexbuf, FONT }
(* conditionals *)
  | "\\newif"
  | "\\else"
  | "\\fi" { position lexbuf, CONDITION }

(* General case for commands *)
  | '#' ['1'-'9'] { position lexbuf, PARAMETER }
  | "\\" (('@' ? ['A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])
    { position lexbuf, COMMAND }
  | '{' { position lexbuf, LBRACE }
  | '}' { position lexbuf, RBRACE }
  | eof { position lexbuf, EOF }
  | _   { position lexbuf, ANY }

{
open Options
open Efuns

let abbreviations = define_option ["tex_mode"; "abbrevs"] ""
    (list_option string2_option) []

(*  
let _ = 
  if !!abbreviations = [] then
    abbreviations =:=
      [ "\\s", "\section";
      "\\ss", "\subsection";
      "\\sss", "\subsubsection";
      "\\p", "\paragraph";
      "\\begit", "\begin{itemize}";
      "\\endit", "\end{itemize}";
      "\\startarticle", "\\documentclass[twocolumn]{article}

\\usepackage{isolatin1}
\\usepackage{francais}
\\usepackage{url}
\\usepackage{epsfig}

\\newcommand{\\comment}[1]{{\\hfuzz 4pt\\par
 \\medskip\\noindent\\hskip-\\fboxsep\\hskip-\\fboxrule\\fbox{\\parbox{\\hsize}{\\it
 #1}}\\par}}

\\newcommand{\\figurebox}[1]{#1}
\\newcommand{\\smallsf}{\\small\\sffamily}
\\newcommand{\\code}{\\texttt}
\\newcommand{\\func}{\\textsf}
\\newcommand{\\msg}{\\textsf}

\\newcommand{\\psfigure}[3]{ % {scale}{filename=label}{caption}
 \\begin{figure}[t]\\begin{center}
 \\figurebox{\\epsfig{file=figs/#2.eps,width=#1\\hsize}}
 \\begin{quote}\\let\\normalsize\\small\\caption{#3\\label{fig:#2}}\\end{quote}
 \\end{center}\\end{figure}}

\\begin{document}

\\title{^^} 

\\bibliographystyle{plain}

\\author{^^}

\\maketitle

\\abstract{^^}

^^
\\bibliography{^^}
\\end{document}
";
    ]
*)
    
type mode =
  Math_mode
| Arg_mode
| Normal_mode

 
let tex_color_buffer buf =

  let keyword_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.keyword_color) 1 0 false in
  let string_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.string_color) 1 0 false in
  let comment_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.comment_color) 1 0 false in
  let gray_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.module_color) 1 0 false in

  let text = buf.buf_text in
  let start_point = Text.new_point text in
  let curseur = Text.new_point text in
  let end_point = Text.new_point text in 
  Text.set_position text end_point (Text.size text);
  let lexbuf = lexing text start_point end_point in
  let rec iter mode lexbuf =
    let (pos,len), token = token lexbuf in
    let mode =
      match token with
        EOF -> 
          raise Exit
      | COMMENT
        ->
          Text.set_position text curseur pos;
          Text.set_attrs text curseur len comment_attr;
          mode
      | MATHMODE ->
          Text.set_position text curseur pos;
          Text.set_attrs text curseur len string_attr;
          (match mode with
              Math_mode -> Normal_mode
            | _ -> Math_mode)
      | STARTMATH ->
          Text.set_position text curseur pos;
          Text.set_attrs text curseur len string_attr;
          Math_mode          
      | ENDMATH
        ->
          Text.set_position text curseur pos;
          Text.set_attrs text curseur len string_attr;
          Normal_mode
      | STARTTABBING
      | ENDTABBING
      | KWDTABBING
        ->
          Text.set_position text curseur pos;
          Text.set_attrs text curseur len gray_attr;
          mode
      | STARTARRAY
      | ENDARRAY
      | KWDARRAY
      | STARTENV
      | ENDENV
      | CONDITION    
      | ITEM
      | PARAMETER
      | COMMAND
      | FONT ->
          Text.set_position text curseur pos;
          Text.set_attrs text curseur len keyword_attr;
          mode
      | LBRACE ->
          Text.set_position text curseur pos;
          Text.set_attrs text curseur len keyword_attr;
          Arg_mode
      | RBRACE ->
          Text.set_position text curseur pos;
          Text.set_attrs text curseur len keyword_attr;
          Normal_mode
      | _ -> 
          match mode with
            Math_mode -> 
              Text.set_position text curseur pos;
              Text.set_attrs text curseur len string_attr;
              mode
          | Arg_mode ->
              Text.set_position text curseur pos;
              Text.set_attrs text curseur len gray_attr;
              mode
          | _ -> mode
    in
    iter mode lexbuf
  in
  try
    iter Normal_mode lexbuf
  with
    _ ->
      buf.buf_modified <- buf.buf_modified + 1;
      Text.remove_point text curseur;
      Text.remove_point text end_point;
      Text.remove_point text start_point


let c_c = (ControlMap,Char.code 'c')

let structures = define_option ["tex_mode"; "structures"] ""
    (list_option Keymap.binding_option) []
  
let _ =
  if !!structures = [] then 
    structures =:=
      [
(*
      [c_c;NormalMap, Char.code '0'], "\\documentclass[twocolumn^^]{article}\n%^^\\usepackage{isolatin1}\n%^^\\usepackage{francais}\n\\usepackage{url}\n\\usepackage[dvips]{epsfig}\n\n\\begin{document}\n\\title{^^}\n\\bibliographystyle{plain}\n\author{^^}\n\\maketitle\n\\abstract{^^}\n\\section{Introduction}\n^^\n\\bibliography{^^}\n\\end{document}\n";
*)
      [c_c;NormalMap, Char.code '1'], "\\section{^^}\n^^";
      [c_c;NormalMap, Char.code '2'], "\\subsection{^^}\n^^";
      [c_c;NormalMap, Char.code '3'], "\\subsubsection{^^}\n^^";
      [c_c;ControlMap, Char.code '1'], "\\section*{^^}\n^^";
      [c_c;ControlMap, Char.code '2'], "\\subsection*{^^}\n^^";
      [c_c;ControlMap, Char.code '3'], "\\subsubsection*{^^}\n^^";
      [c_c;NormalMap, Char.code 'e'], "\\begin{^^}\n^^\n\\end{^^}\n^^";
      [c_c; NormalMap, Char.code 'i'], "\\begin{itemize}\n\\item ^^\n\\end{itemize}";    
      [c_c; NormalMap, Char.code 'v'], "\\begin{verbatim}\n ^^\n\\end{verbatim}";    
    ]
    
let tex_error_regexp = define_option ["tex_mode"; "error_regexp"] ""
    regexp_option (string_to_regex "^l.\\([0-9]+\\)")
let tex_error_regexp2 = define_option ["tex_mode"; "warning_regexp"] ""
    regexp_option (string_to_regex "^LaTeX Warning:.*line +\\([0-9]+\\)")
  
let backward_find_unclosed_paren text point =
  let rec iter n =
    if Text.bmove_res text point 1 = 0 then raise Not_found;
    let d = Text.get_char text point in
    if d = '\n' then iter_line n else
    if d = ')' then iter (n+1) else
    if d = '(' then
      (if n > 0 then iter (n-1))
    else iter n
  and iter_line n =
    let p = Text.get_position text point in
    let bol = Text.point_to_bol text point in
    Text.set_position text point (p-bol);
    if Text.sub text point 4 = "\\OT1" then
      (if Text.bmove_res text point 1 = 0 then raise Not_found; iter_line n)
    else
      (Text.set_position text point p; iter n)
  in
  iter_line 0 

let filename_after_point text point =
  let endp = Text.dup_point text point in
  let rec iter () = 
    if Text.fmove_res text endp 1 = 1 &&
      (let d = Text.get_char text endp in
        d <> '\n' && d <> ')' && d <> ' ') then iter ()
  in
  iter ();
  let filename = Text.sub text point (Text.distance text point endp) in
  Text.remove_point text endp;
  filename

  
open Compil
  
let tex_find_error text error_point = 
  let groups = 
    try
      Text.search_forward_groups text (snd !!tex_error_regexp)
        error_point 1 
    with
      Not_found ->
        Text.search_forward_groups text (snd !!tex_error_regexp2)
          error_point 1 
  in
  let point = Text.dup_point text error_point in
  let filename = 
    try
      backward_find_unclosed_paren text point;
      Text.fmove text point 1;
      filename_after_point text point
    with
      Not_found -> ""
  in
  Text.remove_point text point;
  for _i = 1 to 3 do
    Text.bmove text error_point 1;
    let bol = Text.point_to_bol text error_point in
    Text.bmove text error_point bol;
  done;
  { 
    err_msg = Text.get_position text error_point;
    err_filename = filename;
    err_line = int_of_string groups.(0) - 1;
    err_begin = 0;
    err_end = 0
  }

  
let syntax = define_option ["tex_mode"; "syntax"] 
    "Chars which should not are part of idents" 
    string_option "\'-"

let tex_hooks = define_option ["tex_mode"; "hooks"] "" 
  (list_option string_option)
  [ "accents_mode"; "fill_mode" ]
  
let install buf =
  tex_color_buffer buf; 
  (* les \ et ' font parties des mots *)
  let syntax = !!syntax in
  for i = 0 to String.length syntax - 1 do
    buf.buf_syntax_table.(Char.code syntax.[i]) <- true;
  done;
  let abbrevs = try Var.get_local buf Abbrevs.abbrev_table
    with _ -> 
        let abbrevs = Hashtbl.create 11 in
        Var.set_local buf Abbrevs.abbrev_table abbrevs;
        abbrevs
  in
  Utils.hash_add_assoc abbrevs !!abbreviations;
  Structure.install_structures buf !!structures; 
  List.iter (fun action ->
      try Action.execute_buffer_action action buf with _ -> ()
  ) !!tex_hooks;
  ()
  
let mode = Ebuffer.new_major_mode"TeX" (Some install)

let comment_string = "%"
let _ = Var.set_major_var mode Misc_features.line_comment "%"
  
let comment_region frame =
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let text = buf.buf_text in
  let len = String.length comment_string in
  let mark = 
    match buf.buf_mark with None -> failwith "Mark is not set"
    | Some p ->  p in
  let start, stop = 
    if Text.compare text point mark < 0 then point, mark else mark, point in
  let session = Text.start_session text in
  while Text.compare text start stop < 0 do
    let c = Text.get_char text start in
    Text.fmove text start 1;
    if c = '\n' then (Text.insert text start comment_string; Text.fmove text start len)
  done;
  Text.commit_session text session
  
let uncomment_region frame =
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let text = buf.buf_text in
  let mark = 
    match buf.buf_mark with None -> failwith "Mark is not set"
    | Some p ->  p in
  let len = String.length comment_string in
  let start, stop = 
    if Text.compare text point mark < 0 then point, mark else mark, point in
  let session = Text.start_session text in
  while Text.compare text start stop < 0 do
    let c = Text.get_char text start in
    Text.fmove text start 1;
    if c = '\n' && Text.sub text start len = comment_string then 
     Text.delete text point len
  done;
  Text.commit_session text session
  
let insert_return_in_tex frame =
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let text = buf.buf_text in
  let bol = Text.point_to_bol text point in
  Text.bmove text point bol;
  let comment = (Text.get_char text point = '%') && bol <> 0 in
  Text.fmove text point bol;
  Edit.insert_return frame;
  if comment 
  then Edit.insert_char frame '%'

  (* load in buffer \input{file} *)
let load_input (frame : Frame.t) buf point =
  let str = 
    let text = buf.buf_text in
    let curs = Text.dup_point text point in
    let dirname = match buf.buf_filename with
        None -> ""
      | Some filename -> Filename.dirname filename
    in
    try
      let bol = Text.point_to_bol text curs in
      Text.bmove text curs bol;
      while Text.sub text curs 7 <> "\\input{" do
        let c = Text.get_char text curs in
        if c = '\n' || Text.fmove_res text curs 1 = 0 then raise Not_found;
      done;
      Text.fmove text curs 7;
      let pos = Text.get_position text curs in
      let rec iter count =
        let c = Text.get_char text curs in
        if c = '}' then count else
        if c = '\n' || Text.fmove_res text curs 1 = 0 then raise Not_found 
        else iter (count+1)
      in
      let len = iter 0 in
      Text.set_position text curs pos;
      let s = Text.sub text curs len in
      Text.remove_point text curs;
      Filename.concat dirname (s^".tex")
    with
      e -> Text.remove_point text curs; raise e
  in
  let _ = Frame.load_file frame.caps frame.frm_window str in ()

let load_input_file frame = 
  Multi_buffers.set_previous_frame frame;
  let buf= frame.frm_buffer in
  load_input frame buf frame.frm_point

let input_regexp = Str.regexp "\\input{"
let main_buffer = ref None
let set_main_file frame = main_buffer := Some frame.frm_buffer
let to_main_file (frame : Frame.t) : unit =
  let buf_name = match !main_buffer with None -> raise Not_found |
      Some buf -> buf.buf_name in
  Multi_buffers.set_previous_frame frame;
  Frame.change_buffer frame.caps frame.frm_window buf_name
 
let load_next_input_file frame =
  let buf = match !main_buffer with
      None -> raise Not_found | Some buffer -> buffer in
  let text = buf.buf_text in
  let point = match buf.buf_mark with
      None -> Text.new_point text | Some mark -> mark in
  buf.buf_mark <- Some point;
  let len = Text.search_forward text input_regexp point in
  Text.fmove text point len;
  load_input frame buf point

let load_prev_input_file frame =
  let buf = match !main_buffer with
      None -> raise Not_found | Some buffer -> buffer in
  let text = buf.buf_text in
  let point = match buf.buf_mark with
      None -> Text.new_point text | Some mark -> mark in
  buf.buf_mark <- Some point;
  let _len = Text.search_backward text input_regexp point in
  load_input frame buf point

(*
let browse frame =
  let buf = frame.frm_buffer in
  let filename = match buf.buf_filename with
      None -> failwith "Buffer not saved"
    | Some filename -> filename in
  Server.start frame;
  let _ = Sys.command (Printf.sprintf "efuns_texbrowser %s &" filename) in
  ()
*)
  
  
let tex_mode frame = Ebuffer.set_major_mode frame.frm_buffer mode

let env_list = Options.define_option ["tex_mode";"env_list"] ""
    (list_option string_option) ["array"; "eqnarray";"figure"]
let env_hist = ref !!env_list
  
let begin_env frame = 
  let default = match !env_hist with  [] -> "" | h :: _ -> h in
  Select.select frame ("Insert env: ["^default^"] ") env_hist ""
    (fun _ -> !env_hist) (fun s -> s) (fun str ->
      let str = if str = "" then default else str in
      let buf = frame.frm_buffer in
      let text = buf.buf_text in
      let point = frame.frm_point in
      Text.insert text point (Printf.sprintf "\\begin{%s}\n\\end{%s}" str str);
      Text.fmove text point (8 + String.length str);
      ()
  )
  
(* Could be improved to avoid nested envs *)
let begin_env_regexp = Str.regexp "\\begin{\\([^}]*\\)}"
let end_env frame = 
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let point = frame.frm_point in
  let find_point = Text.dup_point text point in
  try
    let names = Text.search_backward_groups text begin_env_regexp find_point 1
    in
    Text.remove_point text find_point;
    Text.insert text point (Printf.sprintf "\\end{%s}\n" names.(0));
    Text.fmove text point (String.length names.(0) + 7)    
  with e -> Text.remove_point text find_point; raise e
  

let setup_actions () =  
  Action.define_action "tex_mode" tex_mode;
(*  define_action "tex_mode.browse" browse; *)
  Action.define_action "tex_mode.color_buffer" (fun frame -> 
      tex_color_buffer frame.frm_buffer);
  Action.define_action "tex_mode.comment_region" comment_region;
  Action.define_action "tex_mode.uncomment_region" uncomment_region;
  Action.define_action "tex_mode.char_expand_abbrev" (fun frame ->
      Abbrevs.expand_sabbrev frame; Edit.self_insert_command frame);
  Action.define_action "tex_mode.insert_return" insert_return_in_tex;
  Action.define_action "tex_mode.load_input_file" load_input_file;
  Action.define_action "tex_mode.load_next_input_file" load_next_input_file;
  Action.define_action "tex_mode.load_prev_input_file" load_prev_input_file;
  Action.define_action "tex_mode.set_main_file" set_main_file;
  Action.define_action "tex_mode.to_main_file" to_main_file;
  Action.define_action "tex_mode.find_matching_paren" (fun frame ->
      Edit.self_insert_command frame;
      Paren_mode.highlight_paren frame);
  Action.define_action "tex_mode.end_env" end_env;
  Action.define_action "tex_mode.begin_env" begin_env;

  ()  
    
let local_map = define_option ["tex_mode"; "local_map"] ""
    (list_option Keymap.binding_option) []

let setup_maps () =

  Var.set_major_var mode Compil.find_error tex_find_error;


  if !!local_map = [] then
    local_map =:= [
      [c_c;ControlMap, Char.code 'l'], "tex_mode.color_buffer" ;
      [c_c;ControlMap, Char.code 'c'], "compile" ;
      [c_c;NormalMap, Char.code '%'], "tex_mode.comment_region";
      [c_c;ControlMap, Char.code 'u'; NormalMap,Char.code '%'],
      "tex_mode.uncomment_region";
      [NormalMap, Char.code '{'], "tex_mode.char_expand_abbrev";
      [NormalMap, Char.code '*'], "tex_mode.char_expand_abbrev";
      [NormalMap, Char.code ' '], "tex_mode.char_expand_abbrev";
      [MetaMap, Char.code 'q'], "fill_paragraph";
      [NormalMap, XK.xk_Return], "tex_mode.insert_return";  

(*      
      [c_c; NormalMap,XK.xk_bracketleft], "tex_mode.begin_env";
      [c_c; NormalMap,XK.xk_bracketright], "tex_mode.end_env";
*)
      [c_c; NormalMap,XK.xk_Return], "tex_mode.load_input_file";
      [c_c; NormalMap,XK.xk_Right], "tex_mode.load_next_input_file";
      [c_c; NormalMap,XK.xk_Left], "tex_mode.load_prev_input_file";
      [c_c; NormalMap,XK.xk_Down], "tex_mode.set_main_file";
      [c_c; NormalMap,XK.xk_Up], "tex_mode.to_main_file";
      
      [NormalMap, Char.code '}'], "tex_mode.find_matching_paren";
      [NormalMap, Char.code ']'], "tex_mode.find_matching_paren";
      [NormalMap, Char.code ')'], "tex_mode.find_matching_paren";
    ];
(* TODO
  if !!interactives_map = [] then 
    interactives_map =:= [
(*      "tex_browser", "tex_mode.browse"; *)
      "color_buffer", "tex_mode.color_buffer";
    ];
*)

  let map = mode.maj_map in
  (*  Keymap.add_prefix map [c_c]; *)
   !!local_map |> List.iter (fun (keys, action) ->
      try
        let f = Action.execute_action action in
        Keymap.add_binding map keys f;
      with e ->
          Log.printf "Error for action %s" action;
          Log.exn "%s\n" e;
  
  );
  ()
  
let mode_regexp = define_option ["tex_mode"; "mode_regexp"] ""
    (list_option string_option) 
     [".*\\.tex"; ".*\\.cls"; ".*\\.sty";]


let _ =  
  Hooks.add_start_hook (fun () ->
    let alist = Var.get_global Ebuffer.modes_alist in
    Var.set_global Ebuffer.modes_alist 
      ((List.map (fun s -> s, mode) !!mode_regexp) @ alist);
    setup_actions ();
    setup_maps ();
  )
} 
