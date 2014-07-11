{
(*
 * Copyright (c) 2010-2012, Marwan Burelle, the LSE Team and contributors
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL MARWAN BURELLE BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *)

  open Parser
  open Lexing
  exception Error_lex of string * position * position
  exception Unfinished_comment_at_eof of (position * position)

  let ht_kwd = Hashtbl.create 37
  let kwd = [
    ("int",      TINT);
    ("char",     TCHAR);
    ("float",    TFLOAT);
    ("void",     VOID);
    ("enum",     ENUM);
    ("struct",   STRUCT);
    ("union",    UNION);
    ("packed",   PSTRUCT);
    ("sizeof",   SIZEOF);
    ("static",   STATIC);
    ("volatile", VOLATILE);
    ("const",    CONST);
    ("local",    LOCAL);
    ("inline",   INLINE);
    ("if",       IF);
    ("else",     ELSE);
    ("while",    WHILE);
    ("do",       DO);
    ("for",      FOR);
    ("switch",   SWITCH);
    ("case",     CASE);
    ("break",    BREAK);
    ("continue", CONTINUE);
    ("default",  DEFAULT);
    ("typedef",  TYPEDEF);
    ("class",    CLASS);
    ("macro",    MACRO);
    ("init",     INIT);
    ("del",      DEL);
    ("return",   RETURN);
    ("import",   IMPORT);
    ("include",  INCLUDE);
    ("open_import",     OPEN);
    ("goto",     GOTO);
    ("as",       AS);
    ("interface",INTERFACE);
    ("support",  SUPPORT);
    ("property", PROPERTY)
  ]

  let _ = List.iter (fun (k,v) -> Hashtbl.add ht_kwd k v) kwd

  let ht_sym = Hashtbl.create 47
  let sym = [
    ("(",  LPAREN);
    (")",  RPAREN);
    ("[",  LBRACE);
    ("]",  RBRACE);
    ("{",  LCBRACE);
    ("}",  RCBRACE);
    (";",  DCOM);
    (",",  COM);
    ("&",  ESP);
    ("*",  STAR);
    (":",  DD);
    (":=", DDEQ);
    (".",  POINT);
    ("->", ARR);
    ("<",  LT);
    ("<=", LE);
    (">",  GT);
    (">=", GE);
    ("==", EQ);
    ("!=", NEQ);
    ("!",  LNOT);
    ("&&", LAND);
    ("||", LOR);
    ("|",  OR);
    ("^",  XOR);
    ("~",  NOT);
    ("+",  ADD);
    ("-",  MIN);
    ("/",  DIV);
    ("%",  MOD);
    ("++", PPLUS);
    ("--", MMIN);
    ("?",  ASK);
    ("<<", LSR);
    (">>", RSR);
    ("::", NSSEP);
    ("({", LCMPB);
    ("})", RCMPB);
    ("#", SHARP);
  ]

  let _ = List.iter (fun (k,v) -> Hashtbl.add ht_sym k v) sym

  let strbuf = Buffer.create 42
  let asmbuf = Buffer.create 42


  let octC2octML src =
    let s = String.make ((String.length src) + 1) 'o' in
      begin
        String.blit src 1 s 2 ((String.length src) - 1);
        s.[0] <- '0';
        s;
      end

  let new_line lexbuf =
    let p = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <- {
	p with
	  pos_lnum = p.pos_lnum + 1;
	  pos_bol  = p.pos_cnum;
      }

  let char_for_backslash = function
    | 'n' -> '\010'
    | 'r' -> '\013'
    | 'b' -> '\008'
    | 't' -> '\009'
    | '0' -> '\000'
    | ' ' -> '\032'
    | 'a' -> '\007'
    | 'v' -> '\011'
    | 'f' -> '\012'
    | 'e' -> '\027'
    | c   -> c

  let char_dec_code lexbuf s =
    let c = 100 * (Char.code(s.[0]) - 48) +
           10 * (Char.code(s.[1]) - 48) +
                (Char.code(s.[2]) - 48) in
  if (c < 0 || c > 255) then
    raise (Error_lex (Lexing.lexeme lexbuf,
		      Lexing.lexeme_start_p lexbuf,
		      Lexing.lexeme_end_p lexbuf))
  else Char.chr c

  let char_hexa_code s =
    let d1 = Char.code (s.[0]) in
    let val1 =
      if d1 >= 97 then d1 - 87
      else if d1 >= 65 then d1 - 55
      else d1 - 48
    in
    let d2 = Char.code (s.[1]) in
    let val2 =
      if d2 >= 97 then d2 - 87
      else if d2 >= 65 then d2 - 55
      else d2 - 48
    in
      Char.chr (val1 * 16 + val2)


let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) then
    raise (Error_lex (Lexing.lexeme lexbuf,
		      Lexing.lexeme_start_p lexbuf,
		      Lexing.lexeme_end_p lexbuf))
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 =
    if d1 >= 97 then d1 - 87
    else if d1 >= 65 then d1 - 55
    else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 =
    if d2 >= 97 then d2 - 87
    else if d2 >= 65 then d2 - 55
    else d2 - 48
  in
    Char.chr (val1 * 16 + val2)

let comm_count = ref 0
let lastcom =
  let loc = ref (dummy_pos, dummy_pos) in
    function
      | Some l -> loc := l; l
      | None -> !loc

let location lex =
  (Lexing.lexeme_start_p lex, Lexing.lexeme_end_p lex)

let asmon = ref false

}

let ident = ['a'-'z''A'-'Z''_'] ['a'-'z''A'-'Z''0'-'9''_']*
let symbr = "(" | ")" | "[" | "]" | "}" | ";" | "," | "&"
  | "*" | ":" | "." | "->" | "<" | "<=" | ">" | ">=" | "=="
  | "!=" | "!" | "({" | "})" | "&&" | "||" | "|" | "^" | "~"
  | "+" | "-" | "/" | "%" | "++" | "--" | "?" | "<<" | ">>"
  | "::" | "#" | ":="

let assign = "+="| "-="| "*="| "/="| "%="| "<<="| ">>="| "&="| "^="| "|="

let dec_n = ['0'-'9']+
let hex_n = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let oct_n = '0' ['0'-'7']+
let bin_n = '0' ['b' 'B'] ['0'-'1']+
let float_n =
  ['0'-'9']+ ('.' ['0'-'9']* )? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

rule token = parse
    [' ' '\t']				{ token lexbuf }
  | ['\n']				{ new_line lexbuf;
                                          token lexbuf }
  | eof					{ EOF }
  | "/*"
      {
        ignore (lastcom (Some (location lexbuf)));
        block_comment lexbuf
      }
  | "//"				{ line_comment lexbuf }
  | "@" (ident as i)                    { LABEL i }
  | "`" (ident as i)                    { ETAG i }
  | "asm"                               { asmon := true;
                                          ASM }
  | "{"
      {
        if !asmon then
          begin
            Buffer.reset asmbuf;
            asm_bloc asmbuf lexbuf;
            asmon := false;
            ASMBLOCK (Buffer.contents asmbuf)
          end
        else
          LCBRACE
      }
  | symbr as s				{ Hashtbl.find ht_sym s }
  | assign as a				{ ASSOP a }
  | oct_n
      {
	INT (Int64.of_string (octC2octML (Lexing.lexeme lexbuf)))
      }
  | dec_n | hex_n | bin_n
	{
	  INT (Int64.of_string(Lexing.lexeme lexbuf))
	}
  | float_n
      {
	FLOAT (float_of_string (Lexing.lexeme lexbuf))
      }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
      { CHAR(Lexing.lexeme_char lexbuf 1) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ' '0' 'v' 'a' 'f' 'e'] "'"
      { CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { CHAR(char_for_decimal_code lexbuf 2) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { CHAR(char_for_hexadecimal_code lexbuf 3) }
  | "'\\" _
      { let l = Lexing.lexeme lexbuf in
        let esc = String.sub l 1 (String.length l - 1) in
	  raise (Error_lex (esc,
			    Lexing.lexeme_start_p lexbuf,
			    Lexing.lexeme_end_p lexbuf))
      }
  | '"'
      {
	Buffer.reset strbuf;
	bl_string (strbuf) lexbuf;
	STR (Buffer.contents strbuf)
      }
  | ident as i
      {
	try Hashtbl.find ht_kwd i with
	    Not_found -> ID(i)
      }
  | "="                                 { ASSIGN }

and bl_string bs = parse
  | "\\" (['\\' '\'' '"' 'n' 't' 'b' 'r' ' ' '0' 'v' 'a' 'f' 'e'] as c)
      {
        Buffer.add_char bs (char_for_backslash c);
        bl_string bs lexbuf
      }
  | "\\" (['0'-'9'] ['0'-'9'] ['0'-'9'] as s)
      {
        Buffer.add_char bs (char_dec_code lexbuf s);
        bl_string bs lexbuf
      }
  | "\\" 'x' (['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] as s)
      {
        Buffer.add_char bs (char_hexa_code s);
        bl_string bs lexbuf
      }
  | "\\\\"				{ Buffer.add_string bs "\\\\";
					  bl_string bs lexbuf }
  | "\\\""				{ Buffer.add_string bs "\\\"";
					  bl_string bs lexbuf }
  | '"'					{ () }
  | '\n' as c				{ new_line lexbuf;
					  Buffer.add_char bs c;
					  bl_string bs lexbuf }
  | _ as c				{ Buffer.add_char bs c;
					  bl_string bs lexbuf}

and block_comment = parse
  | "/*"
      {
        incr comm_count;
        ignore (lastcom (Some (location lexbuf)));
        block_comment lexbuf
      }
  | "*/"
      {
        if !comm_count = 0 then
          token lexbuf
        else
          begin
            decr comm_count;
            block_comment lexbuf
          end
      }
  | '\n'				{ new_line lexbuf;
                                          block_comment lexbuf }
  | eof
      {
        raise (Unfinished_comment_at_eof (lastcom None))
      }
  | _					{ block_comment lexbuf }

and line_comment = parse
    '\n'				{ new_line lexbuf;
                                          token lexbuf }
  | _					{ line_comment lexbuf }

and asm_bloc ba = parse
  | [' ''\t']*"}"                       { () }
  | '\n'[' ''\t']*                      { new_line lexbuf;
                                          Buffer.add_char ba '\n';
                                          Buffer.add_char ba '\t';
                                          asm_bloc ba lexbuf }
  | _ as c                              { Buffer.add_char ba c;
                                          asm_bloc ba lexbuf }
