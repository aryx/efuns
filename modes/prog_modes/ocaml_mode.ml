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
open Efuns
open Options
open Ocaml_lexer
module I = Common_indenter
module Parameter = Parameter_option
open Compil (* for fields for ocaml-light *)

(***********************************************************************)
(* Paths *)
(***********************************************************************)

let ocaml_path = define_option ["ocaml_mode"; "ocaml_path"] ""
    path_option []
  
let setup_ocaml_path () =
  if !!ocaml_path = [] 
  then ocaml_path =:= !!Globals.load_path

(***********************************************************************)
(* Colors *)
(***********************************************************************)

let color_region buf start_point end_point =
  let keyword_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.keyword_color) 1 
                               (Attr.get_font !!(*keyword_*)Globals.font) false in
  let string_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.string_color) 1 
                              (Attr.get_font !!(*string_*)Globals.font)    false in
  let comment_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.comment_color) 1 
                               (Attr.get_font !!(*comment_*)Globals.font) false in
  let gray_attr = 
    Text.make_attr (Attr.get_color !!Pl_colors.module_color) 1 
                            (Attr.get_font !!(*upper_*)Globals.font) false in
  let text = buf.buf_text in
  let curseur = Text.new_point text in
  let lexbuf = Common_lexer.lexing text start_point end_point in

  let rec iter lexbuf =
    let (pos,len), token = Ocaml_lexer.token lexbuf in
    Text.set_position text curseur pos;
    (match token with
        EOF _ -> raise Exit
      | LET | IN | MATCH | TRY | WITH | FUN 
      | FUNCTION | IF | THEN | ELSE | WHILE | WHEN
      | DONE | DO | FOR | TO | DOWNTO | BEGIN | END 
      | OPEN | MODULE | STRUCT | MUTABLE
      | AND | OR | TYPE | VAL | CLASS | SIG | INHERIT | OBJECT
      | EXCEPTION | RULE | METHOD | EXTERNAL -> 
          Text.set_attrs text curseur len keyword_attr
      | EOFCOMMENT 
      | COMMENT ->
          Text.set_attrs text curseur len comment_attr
      | EOFSTRING
      | CHAR 
      | STRING ->
          Text.set_attrs text curseur len string_attr
      | UIDENT ->
          Text.set_attrs text curseur len gray_attr            
      | _ -> ()
    );
    iter lexbuf
  in
  try
    iter lexbuf
  with Exit ->
    buf.buf_modified <- buf.buf_modified + 1;
    Text.remove_point text curseur


(***********************************************************************)
(************************  abbreviations ********************)
(***********************************************************************)

let abbreviations = define_option ["ocaml_mode"; "abbrevs"] ""
    (list_option string2_option) []
  
let setup_abbrevs () = 
  if !!abbreviations = [] then
    abbreviations =:=
      [
(* Pervasives *)
      ( "nl'", "print_newline ()");
      ( "pr'i", "print_int");
(* Printexc *)
      ( "to'p","Printexc.to_string");
(* Printf *)
      ( "pr'","Printf.printf");
      ( "spr'", "Printf.sprintf");
      ( "fpr'", "Printf.fprintf");
(* List *)
      ( "rev'l","List.rev");
      ( "ite'l","List.iter");
      ( "len'l","List.length");
      ( "lfol'l","List.fold_left");
      ( "rfol'l","List.fold_right");
      ( "map'l","List.map");
      ( "mem'l","List.mem");
      ( "memq'l","List.memq");
      ( "mass'l","List.mem_assoc");
      ( "assq'l","List.assq");
      ( "spl'l","List.split");
      ( "com'l","List.combine");
(* Array *)
      ( "ite'a","Array.iter");
      ( "len'a","Array.length");
      ( "set'a","Array.set");
      ( "get'a","Array.get");
      ( "cre'a","Array.create");
      ( "ini'a","Array.init");
      ( "cop'a","Array.copy");
      ( "sub'a","Array.sub");
      ( "map'a","Array.map");
      ( "con'a", "Array.concat");
(* Hashtbl *)
      ( "ite'h","Hashtbl.iter");
      ( "fin'h","Hashtbl.find");
      ( "add'h","Hashtbl.add");
      ( "rem'h","Hashtbl.remove");
      ( "cre'h","Hashtbl.create");
      ( "cle'h","Hashtbl.clear");
(* Queue *)
      ( "ite'q","Queue.iter");
      ( "len'q","Queue.length");
      ( "cre'q","Queue.create");
      ( "add'q","Queue.add");
      ( "get'q","Queue.take");
      ( "tak'q","Queue.take");
      ( "cle'q","Queue.clear");
      ( "emp'q","Queue.Empty");
(* Filename *)
      ( "con'f","Filename.concat");
      ( "che'f","Filename.check_suffix");
      ( "cho'f","Filename.chop_suffix");
      ( "bas'f","Filename.basename");
      ( "dir'f","Filename.dirname");
(* String *)
      ( "len's","String.length");
      ( "set's","String.set");
      ( "get's","String.get");
      ( "cre's","String.create");
      ( "mak's","String.make");
      ( "sub's","String.sub");
      ( "cop's","String.copy");
      ( "bli's","String.blit");
      ( "con's","String.concat");
      ( "low's","String.lowercase");
      ( "upp's","String.uppercase");
      ( "cap's","String.capitalize");
      ( "unc's","String.uncapitalize");
(* Char *)
      ( "upp'c","Char.uppercase");
      ( "low'c","Char.lowercase");
(* Mutex *)
      ( "loc'm","Mutex.lock");
      ( "unl'm","Mutex.unlock");
      ( "cre'm","Mutex.create");
      ( "cre'c","Condition.create");
      ( "wai'c","Condition.wait");
      ( "cre't","Thread.create");
      ( "len'w","Weak.length");
      ( "set'w","Weak.set");
      ( "get'w","Weak.get");
      ( "a'","Array");
      ( "c'","Char");
      ( "d'","Digest");
      ( "f'","Filename");
      ( "h'","Hashtbl");
      ( "l'","List");
      ( "m'","Map");
      ( "o'","Obj");
      ( "p'","Printexc");
      ( "q'","Queue");
      ( "r'","Random");
      ( "s'","String");
      ( "t'","Thread");
      ( "w'","Weak");
    ]
    

(***********************************************************************)
(**********************  indentations *******************)
(***********************************************************************)
  
let start_regexp = define_option ["ocaml_mode"; "start_regexp"]
    "" regexp_option (
    string_to_regex "^\\(let\\|module\\|type\\|exception\\|open\\)")

let indentation = define_option ["ocaml_mode"; "indentation"] ""
  int_option 2
  
let rec pop_to_top stack =
  match stack with
    [] -> ([],0)
  | (STRUCT,indent) :: _ -> stack, indent+ !!indentation
  | (SIG,indent) :: _ -> stack, indent+ !!indentation
  | (LBRACE,indent) :: _ -> stack, indent+ !!indentation
  | (OBJECT,indent) :: _ -> stack, indent+ !!indentation
  | _ :: stack -> pop_to_top stack

let pop_to_kwds = I.pop_to_kwds SEMISEMI

let token_offset prev_tok =
  match prev_tok with
  
  | CHAR | GREATERRBRACE | GREATERRBRACKET | FALSE | FLOAT | INFIXOP0
  | INFIXOP1 | INFIXOP2 | INFIXOP3 | INFIXOP4 | INT | LESS | LESSMINUS
  | LIDENT | DONE | END | BARRBRACKET | UIDENT | UNDERSCORE | STRING 
  | PREFIXOP | QUESTION | QUOTE | RBRACE  | RBRACKET | RULE | PARSE
    ->  !!indentation
  
  | AMPERAMPER | AMPERSAND | AND | AS | ASSERT | BAR | BARBAR | BEGIN
  | CLASS | COLON | COLONCOLON | COLONEQUAL | COLONGREATER | COMMA 
  |   CONSTRAINT | DO | DOT | DOTDOT | DOWNTO | ELSE | EQUAL | EXCEPTION 
  | EXTERNAL | FOR | FUN | FUNCTION | FUNCTOR | GREATER | IF | IN 
  | INCLUDE | INHERIT | INITIALIZER | LAZY | LBRACE | LBRACELESS 
  | LBRACKET | LBRACKETBAR | LBRACKETLESS | LET | LPAREN | MATCH 
  | METHOD | MINUSGREATER | MODULE | MUTABLE | NEW | OBJECT | OF | OPEN
  | OR | PARSER | PRIVATE
  | REC | RPAREN | SEMI | SEMISEMI
  | SHARP | SIG | STAR | STRUCT | SUBTRACTIVE | THEN | TO | TRUE | TRY
  | TYPE | VAL | VIRTUAL | WHEN | WHILE | WITH
    -> 0
  
  | _ -> 0

let rec parse lexbuf prev_tok stack eols indent indents =
  let _, token = Ocaml_lexer.token lexbuf in
  match token with
  | EOL pos -> parse lexbuf prev_tok stack (pos::eols) indent indents
  | EOF pos -> I.add indent  (pos :: eols) indents

  | EOFSTRING -> (0,[0]) :: (I.add indent eols indents)
  | EOFCOMMENT -> ( !!indentation,[0]) :: (I.add 0 eols indents)

  | COMMENT -> parse lexbuf prev_tok stack [] indent 
        (I.add indent eols indents)
  | LET ->
      (* 
  indentation des LETs: Il faut savoir s'il s'agit d'un LET avec ou sans IN.
   Pour cela, on regarde simplement le token precedent.
   Voici ceux qui ne peuvent pas introduire un TOP-LET
*)
      begin
        match prev_tok with
          IN | THEN | COLONCOLON | INFIXOP0 | INFIXOP1 |
          INFIXOP2 | INFIXOP3 | INFIXOP4 | SUBTRACTIVE | STAR | EQUAL | LESS |
          GREATER | OR | BARBAR | AMPERAMPER | AMPERSAND | COLONEQUAL |
          LESSMINUS | LPAREN | LBRACKET | LBRACKETBAR | MATCH | TRY | IF |
          WHILE | DO | TO | DOWNTO | BEGIN | MINUSGREATER | WHEN | COMMA |
          SEMI | QUESTION | QUOTE | BAR ->
            (* On reste dans le bloc precedent, donc avec la meme indentation *)
            parse lexbuf token ((token,indent) :: stack) [] (indent+ !!indentation) 
            (I.add indent eols indents)
        
        | ELSE ->
            (* On reste dans le bloc precedent, mais avec une indentation plus
        petite car on est sorti du IF THEN ELSE *)
            parse lexbuf token ((token,indent- !!indentation) :: stack) [] indent 
              (I.add (indent- !!indentation) eols indents)
        
        | _ ->
            (* On est dans un nouveau LET toplevel *)
            let (stack, indent) = pop_to_top stack in
            parse lexbuf token ((token,indent) :: stack) [] (indent+ !!indentation) 
            (I.add indent eols indents)
      
      end
  
  | VAL | EXTERNAL | TYPE | EXCEPTION | OPEN
  | INCLUDE | CLASS | RULE | METHOD | INITIALIZER | VIRTUAL ->
      (* On est dans une pharse toplevel *)
      let (stack, indent) = pop_to_top stack in
      parse lexbuf token ((token,indent) :: stack) [] (indent+ !!indentation) 
      (I.add indent eols indents)
  
  | SEMISEMI ->
      let (stack,indent) = pop_to_top stack in
      parse lexbuf token stack [] indent (I.add indent eols indents)
  
  | MODULE ->
      if prev_tok = LET then
        (* LET MODULE *) 
        parse lexbuf token stack [] indent (I.add indent eols indents)
      else
        (* On est dans une pharse toplevel *)
      let (stack, indent) = pop_to_top stack in
      parse lexbuf token ((token,indent) :: stack) [] (indent+ !!indentation) 
      (I.add indent eols indents)
  
  | EQUAL ->
      let (_stack',(_kwd,_indent')) = pop_to_kwds [BAR] stack in
      parse lexbuf token stack [] indent (I.add indent eols indents)
  
  | AND ->
      let (stack,(kwd,indent)) = pop_to_kwds 
          [LET;TYPE;RULE;CLASS] stack in
      parse lexbuf token ((kwd,indent)::stack)
      [] (indent+ !!indentation) (I.add indent eols indents) 
  | OR ->
      let (_stack',(_kwd,_indent')) = pop_to_kwds  [] stack in
      parse lexbuf token stack [] indent (I.add indent eols indents)
  
  | IN -> 
      (* partially terminate a LET structure *)
      let (stack,indent) = I.pop_to LET stack in
      parse lexbuf token ((IN,indent)::stack)
      [] indent (I.add indent eols indents)
  
 
  | DO ->
(* starts a DO ... DONE structure *)
      let (stack',(_kwd,indent')) = pop_to_kwds [WHILE;FOR] stack in
      parse lexbuf DO ((DO,indent') :: stack') [] (indent'+ !!indentation) 
        (I.add indent' eols indents)
(* These keywords start multi-keywords block structures. *)

(* This symbol has different meanings in lexer files *)
  | LBRACE          (* LBRACE ... RBRACE *)
    ->
      if prev_tok = RBRACE &&
        (match stack with
            (BAR,_) :: _ 
          | (PARSE,_) :: _
            -> true
          | _ -> false) then
        parse lexbuf SEMISEMI [] [] 0 
          (I.add 0 eols indents)
      else
      let offset = token_offset prev_tok in
      parse lexbuf token ((token,indent) :: stack) [] (indent+ !!indentation) 
      (I.add (indent+offset) eols indents)

(* Terminated structures *)
  | LPAREN          (* LPAREN ... RPAREN *)
  | LBRACELESS      (* LBRACELESS ... GREATERRBRACE  *)
  | LBRACKET        (* LBRACKET ... RBRACKET  *)
  | LBRACKETBAR     (* LBRACKETBAR ... BARRBRACKET *)
  | LBRACKETLESS    (* LBRACKETLESS ... GREATERRBRACKET *)
  | BEGIN           (* BEGIN ... END  *)
    ->
      let offset = token_offset prev_tok in
      parse lexbuf token ((token,indent) :: stack) [] (indent+ !!indentation) 
      (I.add (indent+offset) eols indents)
      | COLON
    ->
      let offset = token_offset prev_tok in
      parse lexbuf token ((token,indent) :: stack) [] indent
        (I.add (indent+offset) eols indents)
  
  | STRUCT          (* STRUCT ... END *)
  | SIG             (* SIG ... END *)
  | FOR             (* FOR ... TO/DOWNTO... DO  ... DONE  *)
  | WHILE           (* WHILE ... DO ... DONE  *)
  | OBJECT
    
(* Non-terminated structures *)
  | MATCH           (*  MATCH ... WITH ...  *)
  | TRY             (*  TRY ... WITH ...  *)
  | FUNCTION        (*  FUNCTION ... MINUSGREATER ...  *)
  | FUN             (*  FUN ... MINUSGREATER ...  *)
  | PARSER          (*  PARSER ... MINUSGREATER ...  *)
  | IF              (*  TRY ... WITH ...  *)
    ->
      begin
        match prev_tok with
          ELSE ->
            (* On reste dans le bloc precedent, mais avec une indentation plus
            petite car on est sorti du IF THEN ELSE *)
            parse lexbuf token ((token,indent- !!indentation) :: stack) [] indent 
              (I.add (indent- !!indentation) eols indents)
        
        | _ ->
            parse lexbuf token ((token,indent) :: stack) [] (indent+ !!indentation) 
            (I.add indent eols indents)
      end
      
(* Deterministic Terminators *) 
  | RPAREN 
  | RBRACE
  | GREATERRBRACE 
  | RBRACKET 
  | BARRBRACKET 
  | GREATERRBRACKET ->
      (* find corresponding block delimiter *)
      let kwd = List.assoc token 
          [
          RPAREN, LPAREN;
          RBRACE, LBRACE;
          RBRACKET,LBRACKET;
          BARRBRACKET, LBRACKETBAR;
          GREATERRBRACE, LBRACELESS;
          GREATERRBRACKET, LBRACKETLESS
        ]
      in
      let (stack,indent) = I.pop_to kwd stack in
      parse lexbuf token stack [] indent (I.add indent eols indents)
      (* Non-deterministic terminators *)
  | END 
  | DONE
    ->
      let kwds = List.assoc token 
          [
          END,[BEGIN;STRUCT;SIG];
          DONE, [FOR;WHILE;DO;TO;DOWNTO]
        ] 
      in
      let (stack,(_kwd,indent)) = pop_to_kwds kwds stack in
      parse lexbuf token stack [] indent (I.add indent eols indents)
  | WITH ->
      let (stack,(kwd,indent)) = pop_to_kwds [MATCH;TRY;LBRACE] stack in
      if kwd = LBRACE then
        parse lexbuf token ((LBRACE,indent)::stack) [] (indent+ !!indentation)
        (I.add indent eols indents)        
      else
        parse lexbuf token ((WITH,indent)::stack) [] (indent+ !!indentation)
        (I.add indent eols indents)
  | BAR ->
      let (stack,(kwd,indent)) = 
        pop_to_kwds [WITH;FUNCTION;BAR;TYPE;PARSE;LPAREN;LBRACE] stack in
      let kwd = 
        match kwd with
          TYPE | LPAREN | PARSE | LBRACE -> kwd
        | _ -> BAR
      in
      parse lexbuf token ((kwd,indent)::stack) [] (indent+ !!indentation)
      (I.add indent eols indents)
  | MINUSGREATER ->
      let (stack,(kwd,indent)) =
        pop_to_kwds [WITH;FUN;BAR;FUNCTION;TYPE;LPAREN;EXTERNAL;VAL;COLON] stack in
      begin
        match kwd with
          TYPE | LPAREN | EXTERNAL | VAL | COLON ->
            let offset = token_offset prev_tok in
            parse lexbuf token ((kwd,indent)::stack) [] indent 
              (I.add (indent+offset) eols indents)
        | _ ->
            parse lexbuf token ((BAR,indent)::stack) [] 
              (if kwd = FUN then indent+ !!indentation else indent+ 2 * !!indentation)
            (I.add (indent+ !!indentation) eols indents) 
      end
  | THEN ->
      let (stack,indent) = I.pop_to IF stack in
      parse lexbuf token ((THEN,indent)::stack) [] (indent+ !!indentation)
      (I.add indent eols indents) 
  | ELSE ->
      let (stack,indent) = I.pop_to THEN stack in
      parse lexbuf token ((ELSE,indent)::stack) [] (indent+ !!indentation)
      (I.add indent eols indents) 
  | SEMI ->
      let _old_stack = stack in
(* le ; termine un THEN ... ou ELSE ... s'il n'y a pas 
   construction infinie (LET, IN, MATCH, BAR) avant *)
      let (stack1,(_,indent1)) = pop_to_kwds [THEN;ELSE] stack in
      let (stack2,(_,_)) = pop_to_kwds
          [
          LET; IN; COLONCOLON; INFIXOP0; INFIXOP0; INFIXOP1; INFIXOP2;
          INFIXOP3; INFIXOP4; SUBTRACTIVE; STAR; EQUAL; LESS; GREATER;
          OR; BARBAR; AMPERAMPER; AMPERSAND; COLONEQUAL; LESSMINUS;
          LPAREN; LBRACKET; LBRACKETBAR; MATCH; TRY; IF; WHILE; DO; TO;
          DOWNTO; BEGIN; MINUSGREATER; WHEN; COMMA; SEMI; QUESTION;
          QUOTE; BAR; LBRACE
        ]
          stack in
      let new_stack, new_indent =
        if List.length stack1 > List.length stack2 then 
(* le THEN ou ELSE est en premier *)
          stack1, indent1
        else
        (* on continue tout simplement *)
          stack, indent
      in
      parse lexbuf token new_stack [] new_indent
        (I.add indent eols indents)
  
  | PARSE           (* RULE ... PARSE ... *)
    ->
      begin
        match stack with
          (RULE,_) :: _ ->
            parse lexbuf token ((token,indent) :: stack) [] indent 
              (I.add indent eols indents)
        | _ ->
            let offset = token_offset prev_tok in
            parse lexbuf token stack [] indent 
              (I.add (indent+offset) eols indents)
      end
  (* anything else "flushes" the current eols *)
  | _ ->
      let offset = token_offset prev_tok in
      parse lexbuf token stack [] indent 
        (I.add (indent+offset) eols indents)

(* could factorize this function more between the different major modes,
 * but not worth it; it complexifies things.
 *)
let get_indentations buf start_point end_point =
  let text = buf.buf_text in
  let lexbuf = Common_lexer.lexing text start_point end_point in
  parse lexbuf SEMISEMI [] [] 0 []

(* Now, use the indentation from the parser *)

let indent_between_points = 
  I.indent_between_points get_indentations (snd !!start_regexp)

let indent_current_line =
  I.indent_current_line get_indentations (snd !!start_regexp) color_region

(***********************************************************************)
(*********************  aide a la programmation *********)
(***********************************************************************)

(* split a string (remove chr) *)
let split1 str chr =
  let list = ref [] in
  let start = ref 0 in
  for i = 0 to String.length str - 1 do
    if str.[i] = chr then
      let s = String.sub str !start (i - !start) in
      if s <> "" then list := s :: !list;
      start := i+1
  done;
  let s = String.sub str !start (String.length str - !start) in
  if s <> "" then list := s :: !list;
  List.rev !list


let parse_name str = 
  split1 str '.'

(* open a minibuffer for a long name (module+name), then look
 in the corresponding .cmi file to find the type of the value *)


let find_long_word buf point =
  buf.buf_syntax_table.(Char.code '.') <- true;
  let w = Move.current_word buf point in
  buf.buf_syntax_table.(Char.code '.') <- false;
  w  
  
(*
A rudimentary parser, to find all OPEN directives. This will be
useful when trying to discover where a name is defined.
In particular, we will open each .cmi file to find the name.
*)

let module_name buf_name = 
  Filename.chop_extension (String.capitalize_ascii buf_name)

let find_env buf point =
  let text = buf.buf_text in
  let tmp_point = Text.new_point text in
  let rec parse lexbuf stack env =
    let _, t = token lexbuf in
    match t with
      STRUCT -> parse lexbuf ((STRUCT,env) :: stack) env
    | OPEN -> 
        let (pos,len),t = token lexbuf in
        if t <> UIDENT then
          parse lexbuf stack env
        else
          (Text.set_position text tmp_point pos; 
            let ident = Text.sub text tmp_point len in
            parse lexbuf stack (ident::env))
    | END ->
        let stack,env = 
          match stack with
            (STRUCT,env) :: tail -> tail, env
          | _ :: tail -> tail, env
          | [] -> [], env
        in
        parse lexbuf stack env
    | EOFSTRING 
    | EOFCOMMENT
    | EOF _ -> env
    | _ -> parse lexbuf stack env
  in
  let end_point = Text.dup_point text point in
  let curseur = Text.new_point text in
  let lexbuf = Common_lexer.lexing text curseur end_point in
  let env = parse lexbuf [] [] in
  Text.remove_point text curseur;
  Text.remove_point text tmp_point;
  Text.remove_point text end_point;
  (module_name buf.buf_name) :: env

(* C-f1 : approximatively parse the file to find the implementation for
the word under the cursor. During parsing, an envirronment is built and
then used to find the word. 
*)
(* pad: obsolete by ocaml_merlin.ml? *)

(***********************************************************************)
(*********************  find_error  ********************)
(***********************************************************************)

let error_regexp = define_option ["ocaml_mode"; "error_regexp"] ""
    regexp_option (string_to_regex
    "File \"\\(.*\\)\", lines? \\([0-9]+\\)\\(-?[0-9]*\\), characters \\([0-9]+\\)[-]\\([0-9]*\\):")

(* less: could factorize with lisp_mode.ml *)
let find_error text error_point =
  let groups = 
    Text.search_forward_groups text (snd !!error_regexp) 
      error_point 4 in
  let error =
    { Compil.
      err_msg = Text.get_position text error_point;
      err_filename = groups.(0);
      err_line = (int_of_string groups.(1)) - 1;
      err_begin = int_of_string groups.(3);
      err_end = 
        try int_of_string groups.(4)
        with  _ -> int_of_string groups.(3)
    } in
  Text.fmove text error_point 1;
  error

(***********************************************************************)
(*********************  structures ********************)
(***********************************************************************)
open Keymap

let structures = define_option ["ocaml_mode"; "structures"] ""
    (list_option Keymap.binding_option) []
  
let setup_structures () = 
  if !!structures = [] then
    let n' = NormalMap,Char.code '\'' in
    structures =:=
    [
      [c_c; n'; NormalMap, Char.code 'b'], "begin ^^ end^^";
      [c_c; n'; NormalMap, Char.code 'f'], "for ^^ to ^^ do\n^^";
      [c_c; n'; NormalMap, Char.code 'w'], "while ^^ do\n^^\ndone";
      [c_c; n'; NormalMap, Char.code 't'], "try\n ^^ \nwith\n ^^";
      [c_c; n'; NormalMap, Char.code 'm'], "match ^^ with\n ^^";
      [c_c; n'; NormalMap, Char.code 'i'], "if ^^ then\n ^^\n else\n ^^";
      [c_c; n'; NormalMap, Char.code 'l'], "let ^^ = ^^ in\n ^^";
    ]    
  
(***********************************************************************)
(*********************  installation ********************)
(***********************************************************************)

let syntax = define_option ["ocaml_mode"; "syntax"] 
    "Chars which should not are part of idents" 
    string_option "_\'"
  
let ocaml_hooks = define_option ["ocaml_mode"; "hooks"] "" 
  (list_option string_option)
  [  "paren_mode" ]
  
let install buf =
  Color.color_buffer_buf buf; 

  let syntax = !!syntax in
  for i = 0 to String.length syntax - 1 do
    buf.buf_syntax_table.(Char.code syntax.[i]) <- true;
  done;

  let abbrevs = Hashtbl.create 11 in
  Var.set_local buf Abbrevs.abbrev_table abbrevs;
  Utils.hash_add_assoc abbrevs !!abbreviations;

  Structure.install_structures buf !!structures;

  !!ocaml_hooks |> List.iter (fun action ->
      try Action.execute_buffer_action action buf with _ -> ()
  )


let mode =  Ebuffer.new_major_mode "Ocaml" (Some install)

let ocaml_mode = 
  Major_modes.enable_major_mode mode
[@@interactive]


(***********************************************************************)
(********************* setup ********************)
(***********************************************************************)
         
let local_map = define_option ["ocaml_mode"; "local_map"] ""
    (list_option Keymap.binding_option) []

let setup_maps () =
  if !!local_map = [] 
  then local_map =:= [
(*      [c_c; ControlMap,Char.code 'e'], "ocaml_mode.eval_buffer"; *)
      [NormalMap, Char.code '.'], "ocaml_mode.char_expand_abbrev";
      [NormalMap, Char.code ';'], "ocaml_mode.char_expand_abbrev";
(* pad: electric return?
      [NormalMap, XK.xk_Return], "ocaml_mode.return_expand_abbrev";
*)
    ]

let mode_regexp = define_option ["ocaml_mode"; "mode_regexp"] ""
    (list_option string_option) 
    [".*\\.\\(ml\\|mli\\|mll\\|mly\\|mlp\\|mlg\\)$"]


(*** Ocaml minor mode (for Makefiles (!)) ***)

let minor_mode = Ebuffer.new_minor_mode "ocaml" []

let ocaml_minor_mode = 
  Minor_modes.toggle_minor minor_mode
[@@interactive]

  
let _ =  
  Hooks.add_start_hook (fun () ->
    Var.add_global Ebuffer.modes_alist (List.map (fun s ->s,mode)!!mode_regexp);
    Parameter.add_option_parameter ocaml_path;
    Parameter.add_option_parameter indentation;

    setup_ocaml_path ();
    setup_abbrevs ();
    setup_structures ();
    
(*    define_action "ocaml_mode.eval_buffer" eval_buffer; *)
    Action.define_action "ocaml_mode.char_expand_abbrev" (fun frame ->
        Abbrevs.expand_sabbrev frame; Edit.self_insert_command frame);
(* pad: ?
    Action.define_action "ocaml_mode.return_expand_abbrev"
      (fun frame -> Abbrevs.expand_sabbrev frame; insert_and_return frame); 
*)
    
    setup_maps ();
    
    Var.set_major_var mode Compil.find_error find_error;
    Var.set_major_var mode Indent.indent_func indent_between_points;
    Var.set_major_var mode Color.color_func color_region;

    Keymap.add_major_key mode [NormalMap,XK.xk_Tab] indent_current_line;
    
    !!local_map |> List.iter (fun (keys, action) ->
        try
          Keymap.add_major_key mode keys (Action.execute_action action);
        with e ->
          Log.printf "Error for action %s" action;
          Log.exn "%s\n" e;
    );
    ()
  ) 
