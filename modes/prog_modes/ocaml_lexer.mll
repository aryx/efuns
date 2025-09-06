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
open Common
open Lexing 
open Common_lexer

type token =
  AMPERAMPER
| AMPERSAND
| AND
| AS
| ASSERT
| BAR
| BARBAR
| BARRBRACKET
| BEGIN
| CHAR
| CLASS
| COLON
| COLONCOLON
| COLONEQUAL
| COLONGREATER
| COMMA
| CONSTRAINT
| DO
| DONE
| DOT
| DOTDOT
| DOWNTO
| ELSE
| END
| EOF of (Text.position)
| EQUAL
| EXCEPTION
| EXTERNAL
| FALSE
| FLOAT
| FOR
| FUN
| FUNCTION
| FUNCTOR
| GREATER
| GREATERRBRACE
| GREATERRBRACKET
| IF
| IN
| INCLUDE
| INFIXOP0
| INFIXOP1
| INFIXOP2
| INFIXOP3
| INFIXOP4
| INHERIT
| INITIALIZER
| INT
| LAZY
| LBRACE
| LBRACELESS
| LBRACKET
| LBRACKETBAR
| LBRACKETLESS
| LESS
| LESSMINUS
| LET
| LIDENT
| LPAREN
| MATCH
| METHOD
| MINUSGREATER
| MODULE
| MUTABLE
| NEW
| OBJECT
| OF
| OPEN
| OR
| PARSER
| PREFIXOP
| PRIVATE
| QUESTION
| QUOTE
| RBRACE
| RBRACKET
| REC
| RPAREN
| SEMI
| SEMISEMI
| SHARP
| SIG
| STAR
| STRING
| STRUCT
| SUBTRACTIVE
| THEN
| TO
| TRUE
| TRY
| TYPE
| UIDENT
| UNDERSCORE
| VAL
| VIRTUAL
| WHEN
| WHILE
| WITH
| COMMENT
| EOL of (Text.position)
| EOFCOMMENT
| EOFSTRING
| ERROR

(* for lexers *)
| RULE
| PARSE
  
(*  for debugging indent, see Indent.print_stack
  
let tokens = [AMPERAMPER,"AMPERAMPER"; AMPERSAND,"AMPERSAND";
    AND,"AND"; AS,"AS"; ASSERT,"ASSERT"; BAR,"BAR"; BARBAR,"BARBAR";
    BARRBRACKET,"BARRBRACKET"; BEGIN,"BEGIN"; CHAR,"CHAR";
    CLASS,"CLASS"; COLON,"COLON"; COLONCOLON,"COLONCOLON";
    COLONEQUAL,"COLONEQUAL"; COLONGREATER,"COLONGREATER";
    COMMA,"COMMA"; CONSTRAINT,"CONSTRAINT"; DO,"DO"; DONE,"DONE";
    DOT,"DOT"; DOTDOT,"DOTDOT"; DOWNTO,"DOWNTO"; ELSE,"ELSE";
    END,"END"; EQUAL,"EQUAL"; EXCEPTION,"EXCEPTION";
    EXTERNAL,"EXTERNAL"; FALSE,"FALSE"; FLOAT,"FLOAT"; FOR,"FOR";
    FUN,"FUN"; FUNCTION,"FUNCTION"; FUNCTOR,"FUNCTOR";
    GREATER,"GREATER"; GREATERRBRACE,"GREATERRBRACE";
    GREATERRBRACKET,"GREATERRBRACKET"; IF,"IF"; IN,"IN";
    INCLUDE,"INCLUDE"; INFIXOP0,"INFIXOP0"; INFIXOP1,"INFIXOP1";
    INFIXOP2,"INFIXOP2"; INFIXOP3,"INFIXOP3"; INFIXOP4,"INFIXOP4";
    INHERIT,"INHERIT"; INITIALIZER,"INITIALIZER"; INT,"INT";
    LAZY,"LAZY"; LBRACE,"LBRACE"; LBRACELESS,"LBRACELESS";
    LBRACKET,"LBRACKET"; LBRACKETBAR,"LBRACKETBAR";
    LBRACKETLESS,"LBRACKETLESS"; LESS,"LESS"; LESSMINUS,"LESSMINUS";
    LET,"LET"; LIDENT,"LIDENT"; LPAREN,"LPAREN"; MATCH,"MATCH";
    METHOD,"METHOD"; MINUSGREATER,"MINUSGREATER"; MODULE,"MODULE";
    MUTABLE,"MUTABLE"; NEW,"NEW"; OBJECT,"OBJECT"; OF,"OF";
    OPEN,"OPEN"; OR,"OR"; PARSER,"PARSER"; PREFIXOP,"PREFIXOP";
    PRIVATE,"PRIVATE"; QUESTION,"QUESTION"; QUOTE,"QUOTE";
    RBRACE,"RBRACE"; RBRACKET,"RBRACKET"; REC,"REC"; RPAREN,"RPAREN";
    SEMI,"SEMI"; SEMISEMI,"SEMISEMI"; SHARP,"SHARP"; SIG,"SIG";
    STAR,"STAR"; STRING,"STRING"; STRUCT,"STRUCT";
    SUBTRACTIVE,"SUBTRACTIVE"; THEN,"THEN"; TO,"TO"; TRUE,"TRUE";
    TRY,"TRY"; TYPE,"TYPE"; UIDENT,"UIDENT"; UNDERSCORE,"UNDERSCORE";
    VAL,"VAL"; VIRTUAL,"VIRTUAL"; WHEN,"WHEN"; WHILE,"WHILE";
    WITH,"WITH"; COMMENT,"COMMENT"; EOFCOMMENT,"EOFCOMMENT";
    EOFSTRING,"EOFSTRING"; ERROR,"ERROR"; RULE,"RULE"; PARSE,"PARSE"]

let token_to_string token =
  List.assoc token tokens
*)

(* For nested comments *)
  
let comment_depth = ref 0

(* The table of keywords *)
  
let keyword_table = [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "function", FUNCTION;
    "if", IF;
    "in", IN;
    "lazy", LAZY;
    "let", LET;
    "match", MATCH;
    "module", MODULE;
    "mutable", MUTABLE;
    "new", NEW;
    "of", OF;
    "open", OPEN;
    "or", OR;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "mod", INFIXOP3;
    "land", INFIXOP3;
    "lor", INFIXOP3;
    "lxor", INFIXOP3;
    "lsl", INFIXOP4;
    "lsr", INFIXOP4;
    "asr", INFIXOP4;
    (* for lexers *)
    "rule", RULE;
    "parse", PARSE;

    "class", CLASS;
    "constraint", CONSTRAINT;
    "functor", FUNCTOR;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "method", METHOD;
    "object", OBJECT;
    "include", INCLUDE;
    "parser", PARSER;
    "private", PRIVATE;
    "virtual", VIRTUAL;
  ] |> Hashtbl_.of_list

(* To store the position of the beginning of a string or comment *)
  
let start_pos = ref 0

}

let blank = [' ' '\009']
let return = ['\010' '\012' '\013']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal = ['0'-'9']+
let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let oct_literal = '0' ['o' 'O'] ['0'-'7']+
let bin_literal = '0' ['b' 'B'] ['0'-'1']+
let float_literal =
          ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

rule token = parse
    blank +    { token lexbuf }
  | return     { let (p,_) as pos = position lexbuf in pos, EOL p }
  | "_"        { position lexbuf, UNDERSCORE }
  | lowercase identchar *
    { position lexbuf,
      let s = Lexing.lexeme lexbuf in
      try
        Hashtbl.find keyword_table s
      with Not_found ->
          LIDENT }
  | uppercase identchar *
    { position lexbuf, UIDENT }       (* No capitalized keywords *)
  | decimal_literal | hex_literal | oct_literal | bin_literal
    { position lexbuf, INT }
  | float_literal
    { position lexbuf, FLOAT }
  | "\""
      { reset_string_buffer();
      let string_start = Lexing.lexeme_start lexbuf in
      start_pos := string_start;
      try
        string lexbuf;
        lexbuf.Lexing.lex_start_pos <-
          string_start - lexbuf.Lexing.lex_abs_pos;
        (!start_pos + !lexer_start, lexeme_end lexbuf - !start_pos), STRING
      with
        Error (pos,len,_error) -> (pos,len), EOFSTRING          
    }
  | "'" [^ '\\' '\''] "'"
      { position lexbuf, CHAR }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { position lexbuf, CHAR }
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { position lexbuf, CHAR }
  | "(*"
      { comment_depth := 1;
      start_pos := Lexing.lexeme_start lexbuf;
      try
        comment lexbuf;
        (!start_pos + !lexer_start, lexeme_end lexbuf - !start_pos), COMMENT
      with
        Error (pos,len,_error) -> (pos,len), EOFCOMMENT
    }
  | "#" [' ' '\t']* ['0'-'9']+ [' ' '\t']* "\"" [^ '\n' '\r'] *
    ('\n' | '\r' | "\r\n")
      (* # linenum "filename" flags \n *)
    { token lexbuf }
  | "#"  { position lexbuf, SHARP  }
  | "&"  { position lexbuf, AMPERSAND  }
  | "&&" { position lexbuf, AMPERAMPER  }
  | "'"  { position lexbuf, QUOTE  }
  | "("  { position lexbuf, LPAREN  }
  | ")"  { position lexbuf, RPAREN  }
  | "*"  { position lexbuf, STAR  }
  | ","  { position lexbuf, COMMA  }
  | "?"  { position lexbuf, QUESTION  }
  | "->" { position lexbuf, MINUSGREATER  }
  | "."  { position lexbuf, DOT  }
  | ".." { position lexbuf, DOTDOT  }
  | ":"  { position lexbuf, COLON  }
  | "::" { position lexbuf, COLONCOLON  }
  | ":=" { position lexbuf, COLONEQUAL  }
  | ":>" { position lexbuf, COLONGREATER  }
  | ";"  { position lexbuf, SEMI  }
  | ";;" { position lexbuf, SEMISEMI  }
  | "<"  { position lexbuf, LESS  }
  | "<-" { position lexbuf, LESSMINUS  }
  | "="  { position lexbuf, EQUAL  }
  | "["  { position lexbuf, LBRACKET  }
  | "[|" { position lexbuf, LBRACKETBAR  }
  | "[<" { position lexbuf, LBRACKETLESS  }
  | "]"  { position lexbuf, RBRACKET  }
  | "{"  { position lexbuf, LBRACE  }
  | "{<" { position lexbuf, LBRACELESS  }
  | "|"  { position lexbuf, BAR  }
  | "||" { position lexbuf, BARBAR  }
  | "|]" { position lexbuf, BARRBRACKET  }
  | ">"  { position lexbuf, GREATER  }
  | ">]" { position lexbuf, GREATERRBRACKET  }
  | "}"  { position lexbuf, RBRACE  }
  | ">}" { position lexbuf, GREATERRBRACE  }
  
  | "!=" { position lexbuf, INFIXOP0 }
  | "-"  { position lexbuf, SUBTRACTIVE }
  | "-." { position lexbuf, SUBTRACTIVE }
  
  | ['!' '?' '~'] symbolchar *
    { position lexbuf, PREFIXOP }
  | ['=' '<' '>' '|' '&' '$'] symbolchar *
    { position lexbuf, INFIXOP0 }
  | ['@' '^'] symbolchar *
    { position lexbuf, INFIXOP1 }
  | ['+' '-'] symbolchar *
    { position lexbuf, INFIXOP2 }
  | "**" symbolchar *
    { position lexbuf, INFIXOP4 }
  | ['*' '/' '%'] symbolchar *
    { position lexbuf, INFIXOP3 }
  | eof { let (pos,len) = position lexbuf in (pos,len), EOF pos }
  | _
      { position lexbuf, ERROR }

and comment = parse
    "(*"
    { comment_depth := succ !comment_depth; comment lexbuf }
  | "*)"
      { comment_depth := pred !comment_depth;
      if !comment_depth > 0 then comment lexbuf }
  | "\""
      { reset_string_buffer();
      let old_start_pos = !start_pos in
      start_pos := Lexing.lexeme_start lexbuf;
      string lexbuf;
      string_buff := initial_string_buffer;
      start_pos := old_start_pos;
      comment lexbuf }
  | "''"
      { comment lexbuf }
  | "'" [^ '\\' '\''] "'"
      { comment lexbuf }
  | "'\\" ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment lexbuf }
  | eof
      { raise
        (Error (!start_pos + !lexer_start, lexeme_end lexbuf - !start_pos,
          Unterminated_comment)) }
  | _
      { comment lexbuf }

and string = parse
    '"'
    { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
    { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
      string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
      string lexbuf }
  | eof
      { raise
        (Error (!start_pos + !lexer_start, lexeme_end lexbuf - !start_pos,
          Unterminated_string)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
      string lexbuf }
