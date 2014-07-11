%{

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

 open Ast

 let dumpos = (Lexing.dummy_pos, Lexing.dummy_pos)

 let build_block data loc = {
   cont = data;
   loc = loc;
   info = None;
 }

 let opstr = function
   | None -> ""
   | Some s -> s

 let dumf = {
   fname = "";
   fnspace = [];
   fparams = [];
   finline = false;
   frtype = Void (build_block () dumpos);
   fbody = [];
   floc = dumpos;
   finfo = None;
   fexport = true;
   fasname = None;
 }

 let build_innerbox
     ?(c=None) ?(d=None) ?(p=None)
     ?(interf=[])
     name nloc nsp members
     =
   {
     bname = build_block name nloc;
     bnspace = nsp;
     members = members;
     constructor = c;
     destructor = d;
     parent = p;
     interf = interf;
   }

 let build_vd ?(vi=None) mods vt vn loc =
   {
     vmods = mods;
     vtype = vt;
     vname = vn;
     vinit = vi;
     vloc = loc;
     vinfo = None;
   }

 (* let stmoption = function *)
 (*   | None -> EmptyStatement dumpos; *)
 (*   | Some s -> s *)

 let print_loc l1 l2 =
   Printf.eprintf
     "syntax error: from line %d, char %d to line %d char %d\n"
     l1.Lexing.pos_lnum
     (l1.Lexing.pos_cnum - l1.Lexing.pos_bol)
     l2.Lexing.pos_lnum
     (l2.Lexing.pos_cnum - l2.Lexing.pos_bol)

 let class_init = ref None
 let class_del  = ref None

%}

%token <int64> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STR
%token <string> ID
%token <string> ASSOP
%token <string> ETAG
%token TCHAR
%token LPAREN RPAREN LBRACE RBRACE LCBRACE RCBRACE
%token DCOM COM ESP STAR DD POINT ARR NSSEP SHARP DDEQ
%token LE LT GT GE EQ NEQ LNOT LAND LOR OR XOR NOT ASSIGN
%token ADD MIN DIV MOD PPLUS MMIN ASK LSR RSR
%token IF ELSE WHILE DO FOR SWITCH CASE BREAK CONTINUE DEFAULT
%token STRUCT PSTRUCT UNION ENUM TFLOAT TINT VOID SIZEOF TYPEDEF
%token STATIC VOLATILE CONST INLINE LOCAL
%token CLASS INIT DEL RETURN IMPORT INCLUDE OPEN
%token INTERFACE SUPPORT
%token PROPERTY
%token LCMPB RCMPB
%token GOTO
%token <string> LABEL
%token ASM
%token <string> ASMBLOCK
%token MACRO
%token AS
%token EOF

%left LOR
%left LAND
%left OR
%left XOR
%left ESP
%left EQ NEQ
%left LE LT GE GT
%left LSR RSR
%left ADD MIN
%left STAR DIV MOD

%type <TypeAlg.info expr> expr
%type <TypeAlg.info texpr> texpr
%type <TypeAlg.info Ast.source> main
%type <TypeAlg.info statement> statement
%type <TypeAlg.info fundecl> fundecl

%start main

%%

  main:
| mp=modparam l=gdecl* EOF
    {
      {
        srcmodparam = mp;
        srcgdecls = l;
        srcloc = ($startpos,$endpos);
      }
    }
| error { raise (Asttools.SyntaxError ($startpos,$endpos)) }

  modparam:
| is=imports*
    {
      {modexport=true; modimports=is}
    }

  imports:
| IMPORT m=ID DCOM
    {
      Import (build_block m ($startpos, $endpos))
    }
| INCLUDE l=separated_nonempty_list(COM,STR) DCOM
    {
      Include (build_block l ($startpos, $endpos))
    }
| OPEN m=ID DCOM
    {
      Open (build_block m ($startpos, $endpos))
    }

  gdecl:
| f=fundecl      { FDecl f }
| v=vardecl DCOM { VDecl v }
| t=tdef         { TDef t }
| b=boxed        { BDecl b }
| mc=macroclass  { MCDecl mc }
| md=mapdecl     { md }
| cd=constdecl   { cd }
| p=property     { Proper p }

    constdecl:
| SHARP v=vardecl DCOM
    {Macro (VDecl v)}
| SHARP f=fundecl
    { Macro (FDecl f) }

    mapdecl:
| v=vardecl AS m=ID DCOM
    { Map (v,m) }

  tdef:
| TYPEDEF ns=idspace ASSIGN t=texpr DCOM
  {
    let (x,ns) = ns in
    {
      tdname = x;
      tdnspace = ns;
      tdtype = t;
      tdloc = ($startpos, $endpos);
      tdinfo = None;
    }
  }

%inline vmods:
| STATIC   { "static" }
| CONST    { "const" }

  vardecl:
| x=ID DD LOCAL mods=vmods* t=texpr
  {
    {
      vmods = "local"::mods;
      vtype = t;
      vname = x;
      vinit = None;
      vloc = ($startpos, $endpos);
      vinfo = None;
    }
  }
| x=ID DDEQ LOCAL t=texpr arg=args
{
  {
    vmods = [ "local" ];
    vtype = t;
    vname = x;
    vinit = Some (SelfInit (build_block (t,arg) ($startpos, $endpos)));
    vloc = ($startpos, $endpos);
    vinfo = None;
  }
}
| x=ID DD mods=vmods* t=texpr v=ioption(preceded(ASSIGN,expr))
  {
    {
      vmods = mods;
      vtype = t;
      vname = x;
      vinit = v;
      vloc = ($startpos, $endpos);
      vinfo = None;
    }
  }

    fundecl:
| ns=idspace p=params
    inline=boption(INLINE)
    DD static=boption(STATIC) t=texpr
    b=block_statement_or_eol
    {
      let (f,ns) = ns in
      let (b,fasname) = b in
        {
          fname = f;
          fnspace = ns;
          finline = inline;
          fparams = p;
          frtype = t;
          fbody = b;
          floc = ($startpos, $endpos);
          finfo = None;
          fexport = not static;
          fasname = fasname;
        }
    }

  block_statement_or_eol:
| AS m=ID LCBRACE b=statement+ RCBRACE
    { (b, Some m) }
| LCBRACE b=statement+ RCBRACE
    { (b,None) }
| LCBRACE RCBRACE
    {
      ([EmptyStatement (build_block () ($startpos, $endpos))], None)
    }
| AS m=ID DCOM
    { ([], Some m) }
| DCOM { ([],None) }

%inline param:
| x=ID DD t=texpr
    { ([],t,x) }

| x=ID DD CONST t=texpr
    {["const"],t,x}

%inline params:
| p=delimited(LPAREN,separated_list(COM,param),RPAREN) { p }

%inline idspace:
| x=ID
    { (x, []) }
| ns=ID NSSEP x=ID
    { (x, [ns]) }

    texpr:
| t=texpr1 VOLATILE
    {
      TModifier (build_block ("volatile",t) ($startpos, $endpos))
    }
| t=texpr1 CONST
    {
      TModifier (build_block ("const",t) ($startpos, $endpos))
    }
| t=texpr1 dims=delimited(LBRACE, separated_list(COM, cstexpr), RBRACE)
    {
      Array (build_block (t, dims) ($startpos, $endpos))
    }
| p=delimited(LT,delimited(LPAREN, separated_list(COM,texpr), RPAREN),GT) DD t=texpr
    {
      Fun (build_block (p,t) ($startpos, $endpos))
    }
| t=texpr1 { t }

    texpr1:
| VOID    { Void (build_block () ($startpos, $endpos)) }
| id=idspace
    {
      let (t,ns) = id in
      TName (build_block (ns, t) ($startpos, $endpos))
    }
| TCHAR
    {
      TChar (build_block () ($startpos, $endpos))
    }
| TINT LT s=boption(ADD) e=cstexpr GT
    {
      Num (build_block (s,e) ($startpos, $endpos))
    }
| TFLOAT LT e=cstexpr GT
    {
      TFloat (build_block e ($startpos, $endpos))
    }
| t=texpr1 STAR
    {
      TPointer (build_block t ($startpos, $endpos))
    }
| t=delimited(LPAREN, texpr, RPAREN) { t }

%inline rops:
| NOT             { "~" }
| LNOT            { "!" }

%inline lops:
| PPLUS           { "++" }
| MMIN            { "--" }

%inline vexpr:
| x=INT   { VInt x }
| x=FLOAT { VFloat x }
| x=CHAR  { VChar x }
| x=STR   { VCstStr x }
| x=ETAG  { VTag x }

%inline args:
| arg=delimited(LPAREN,separated_list(COM,expr),RPAREN)
    {
      arg
    }

    expr:
| bl=delimited(LCMPB, statement+ ,RCMPB)
    {
      Compound (build_block bl ($startpos, $endpos))
    }
| e1=ternexpr ASSIGN e2=expr
    {
      AssignOp (build_block ("=", e1, e2) ($startpos, $endpos))
    }
| e1=ternexpr a=ASSOP e2=expr
    {
      AssignOp (build_block (a, e1, e2) ($startpos, $endpos))
    }
| e=ternexpr { e }

    ternexpr:
| e=expr1 ASK e1=expr1 DD e2=expr1
    {
      TerOp (build_block (e, e1, e2) ($startpos, $endpos))
    }
| e=expr1 { e }

%inline bops:
| ADD   { "+" }
| MIN   { "-" }
| STAR  { "*" }
| DIV   { "/" }
| MOD   { "%" }
| LE    { "<=" }
| LT    { "<" }
| GE    { ">=" }
| GT    { ">" }
| EQ    { "==" }
| NEQ   { "!=" }
| LAND  { "&&" }
| LOR   { "||" }
| ESP   { "&" }
| OR    { "|" }
| XOR   { "^" }
| LSR   { "<<" }
| RSR   { ">>" }

    expr1:
| e1=expr1 o=bops e2=expr1
    {
      BinOp (build_block (o,e1,e2) ($startpos, $endpos))
    }
| e=unexpr { e }

    unexpr:
| ADD e=priexpr
    {
      PreOp (build_block ("+", e) ($startpos, $endpos))
    }
| MIN e=priexpr
    {
      PreOp (build_block ("-", e) ($startpos, $endpos))
    }
| STAR e=priexpr
    {
      PreOp (build_block ("*", e) ($startpos, $endpos))
    }
| PPLUS e=priexpr
    {
      PreOp (build_block ("++", e) ($startpos, $endpos))
    }
| MMIN e=priexpr
    {
      PreOp (build_block ("--", e) ($startpos, $endpos))
    }
| ESP e=priexpr
    {
      PreOp (build_block ("&", e) ($startpos, $endpos))
    }
| o=rops e=priexpr
    {
      PreOp (build_block (o,e) ($startpos, $endpos))
    }
| LPAREN e=priexpr DD te=texpr RPAREN
    {
      Cast (build_block (te, e) ($startpos, $endpos))
    }
| SIZEOF te=delimited(LPAREN,texpr,RPAREN)
    {
      Sizeof (build_block te ($startpos, $endpos))
    }
| e=priexpr { e }

    priexpr:
| fe=priexpr arg=args
    {
      Call (build_block (fe, arg) ($startpos, $endpos))
    }
| e=priexpr o=lops
    {
      PostOp (build_block (o,e) ($startpos, $endpos))
    }
| e=priexpr dims=delimited(LBRACE,separated_list(COM,expr),RBRACE)
    {
      Index (build_block (e,dims) ($startpos, $endpos))
    }
| e=priexpr POINT f=ID
    {
      Field (build_block (e,f) ($startpos, $endpos))
    }
| e=priexpr ARR f=ID
    {
      PField (build_block (e,f) ($startpos, $endpos))
    }
| x=vexpr
    {
      Value (build_block x ($startpos, $endpos))
    }
| ns=idspace
    {
      let (x,ns) = ns in
      Id (build_block (ns, x) ($startpos, $endpos))
    }
| e=delimited(LPAREN,expr,RPAREN)
    { e }
| LOCAL
    {
        Id (build_block ([], "local") ($startpos, $endpos))
    }

%inline bcops:
| ADD   { "+" }
| MIN   { "-" }
| STAR  { "*" }
| DIV   { "/" }
| MOD   { "%" }
| LSR   { "<<" }
| RSR   { ">>" }

      cstexpr:
| x=ID
    {
      Id (build_block ([], x) ($startpos, $endpos))
    }
| x=vexpr { Value (build_block x ($startpos, $endpos)) }
| e1=cstexpr o=bcops e2=cstexpr
    {
      BinOp (build_block (o,e1,e2) ($startpos, $endpos))
    }
| SIZEOF te=delimited(LPAREN,texpr,RPAREN)
    {
      Sizeof (build_block te ($startpos, $endpos))
    }
| e=delimited(LPAREN, cstexpr, RPAREN) {e}

%inline swstate:
| CASE v=vexpr DD sl=statement*
    {
      (SwValue (build_block v ($startpos(v), $endpos(v))), sl)
    }
| DEFAULT DD sl=statement*
    {
      (Default (build_block () ($startpos, $endpos)), sl)
    }

    block:
| LCBRACE sl=list(statement) RCBRACE
    {
      Block (build_block sl ($startpos, $endpos))
    }

    forexpr:
| e=expr { e }
| e=decstatement { e }

    decstatement:
| v=vardecl
    {
      Decl (build_block v ($startpos, $endpos))
    }

    statement:
| b=block { b }
| DEL e=expr DCOM
    {
      Delete (build_block e ($startpos, $endpos))
    }
| RETURN e=expr? DCOM
      {
	Return (build_block e ($startpos, $endpos))
      }
| DCOM
      {
	EmptyStatement (build_block () ($startpos, $endpos))
      }
| e=decstatement DCOM
    {
      Expr (build_block e ($startpos, $endpos))
    }
| e=expr DCOM
    {
      Expr (build_block e ($startpos, $endpos))
    }
| IF c=delimited(LPAREN,expr,RPAREN)
    t=statement e=ioption(preceded(ELSE,statement))
    {
      let el = match e with
        | None ->
          EmptyStatement (build_block () ($startpos, $endpos))
        | Some e -> e
      in
      If (build_block (c,t,el) ($startpos, $endpos))
    }
| WHILE c=delimited(LPAREN,expr,RPAREN) t=statement
    {
      While (build_block (c, t) ($startpos, $endpos))
    }
| DO b=block WHILE
    c=delimited(LPAREN,expr,RPAREN) DCOM
    {
      Do (build_block (c, b) ($startpos, $endpos))
    }
| FOR LPAREN e1=forexpr? DCOM e2=expr? DCOM e3=expr? RPAREN t=statement
    {
      For (build_block (e1,e2,e3,t) ($startpos, $endpos))
    }
| SWITCH e=delimited(LPAREN,expr,RPAREN) LCBRACE sl=swstate+ RCBRACE
    {
      Switch (build_block (e,sl) ($startpos, $endpos))
    }
| bc=breakcont { bc }
| l=LABEL DD {Label (build_block l ($startpos, $endpos))}
| GOTO l=LABEL DCOM
    {
      Goto (build_block l ($startpos, $endpos))
    }
| asm=asmblock
    {
      Asm (build_block asm ($startpos, $endpos))
    }

%inline breakcont:
| BREAK DCOM
  {
    Break (build_block () ($startpos, $endpos))
  }
| CONTINUE DCOM
  {
    Continue (build_block () ($startpos, $endpos))
  }

%inline nsid:
| x=ID { ("",x) }
| ns=ID NSSEP x=ID { (ns,x) }

  boxed:
| STRUCT nsi=idspace LCBRACE m=smembers RCBRACE
  {
    let (n,ns) = nsi in
    Struct (
      build_block
        (false, build_innerbox
          n ($startpos(nsi), $endpos(nsi))
          ns m)
        ($startpos, $endpos)
    )
  }
| STRUCT? PSTRUCT nsi=idspace LCBRACE m=smembers RCBRACE
  {
    let (n,ns) = nsi in
    Struct (
      build_block
        (true, build_innerbox
          n ($startpos(nsi), $endpos(nsi))
          ns m)
        ($startpos, $endpos)
    )
  }
| UNION nsi=idspace LCBRACE m=smembers RCBRACE
  {
    let (n,ns) = nsi in
    Union (
      build_block
        (build_innerbox
           n ($startpos(nsi), $endpos(nsi))
           ns m)
        ($startpos, $endpos)
    )
  }
| ENUM nsi=idspace LCBRACE m=emembers RCBRACE
  {
    let (n,ns) = nsi in
    Enum (
      build_block
        (build_innerbox
           n ($startpos(nsi), $endpos(nsi))
           ns m)
        ($startpos, $endpos)
    )
  }
| CLASS nsi=idspace p=preceded(DD,ID)?
    inter=preceded(SUPPORT,nsid+)?
  LCBRACE m=clasmems RCBRACE
  {
    let (n,ns) = nsi in
    let (c,d,members) = m in
    Class (
      build_block
        (build_innerbox
           ~c:c ~d:d ~p:p
           ?interf:inter
           n ($startpos(nsi), $endpos(nsi))
           ns members)
        ($startpos, $endpos)
    )
  }
| INTERFACE nsi=idspace p=preceded(DD,nsid+)?
    LCBRACE m=imember* RCBRACE
    {
      let (n,ns) = nsi in
      let members = m in
        Interface (
          build_block
            (build_innerbox
               ?interf:p
               n ($startpos(nsi), $endpos(nsi))
               ns members)
            ($startpos, $endpos)
        )
    }

%inline smembers:
| m=list(smember) { m }

  smember:
| x=ID DD t=texpr DCOM
    {
      SField (build_vd [] t x ($startpos, $endpos))
    }

%inline emembers:
| m=nonempty_list(emember) { m }

  emember:
| n=ETAG e=preceded(ASSIGN,cstexpr)? DCOM
    {
      EField (build_block (n, e) ($startpos, $endpos))
    }

%inline cinit:
| INIT p=params LCBRACE s=statement+ RCBRACE
    {
      Some {
        dumf with
          fname = "init";
          fparams = p;
          frtype = Void (build_block () ($startpos, $endpos));
          fbody = s;
          floc = ($startpos, $endpos);
      }
    }
| INIT p=params LCBRACE RCBRACE
    {
      Some {
        dumf with
          fname = "init";
          fparams = p;
          frtype = Void (build_block () ($startpos, $endpos));
          fbody =
          [EmptyStatement (build_block () ($startpos, $endpos))];
          floc = ($startpos, $endpos);
      }
    }
| INIT p=params DCOM
    {
      Some {
        dumf with
          fname = "init";
          fparams = p;
          frtype = Void (build_block () ($startpos, $endpos));
          fbody = [] ;
          floc = ($startpos, $endpos);
      }
    }
%inline cdel:
| DEL LPAREN RPAREN LCBRACE s=statement+ RCBRACE
    {
      Some {
        dumf with
          fname = "del";
          fparams = [];
          frtype = Void (build_block () ($startpos, $endpos));
          fbody = s;
          floc = ($startpos, $endpos);
      }
    }
| DEL LPAREN RPAREN LCBRACE RCBRACE
    {
      Some {
        dumf with
          fname = "del";
          fparams = [];
          frtype = Void (build_block () ($startpos, $endpos));
          fbody =
          [EmptyStatement (build_block () ($startpos, $endpos))];
          floc = ($startpos, $endpos);
      }
    }
| DEL LPAREN RPAREN DCOM
    {
      Some {
        dumf with
          fname = "del";
          fparams = [];
          frtype = Void (build_block () ($startpos, $endpos));
          fbody = [] ;
          floc = ($startpos, $endpos);
      }
    }

  cmember:
| ns=idspace p=params
    DD t=texpr
    b=block_statement_or_eol
    {
      let (f,ns) = ns in
      let (b,fasname) = b in
      let fd =
        {
          fname = f;
          fnspace = ns;
          finline = false;
          fparams = p;
          frtype = t;
          fbody = b;
          floc = ($startpos, $endpos);
          finfo = None;
          fexport = true;
          fasname = fasname;
        }
      in [ Method fd ]
    }
| x=ID DD mods=vmods* t=texpr e=preceded(ASSIGN,expr)? DCOM
    {
      [SField (build_vd ~vi:e mods t x ($startpos, $endpos))]
    }
| x=ID DDEQ LOCAL t=texpr arg=args DCOM
    {
      [SField
          (build_vd
             ~vi:(
               Some (SelfInit (build_block (t,arg) ($startpos, $endpos)))
             )
             [ "local" ] t x ($startpos, $endpos))]
    }
| ci=cinit
    {
      match !class_init with
          None -> class_init := ci; []
        | Some _ -> raise (Asttools.SyntaxError ($startpos,$endpos))
    }
| ci=cdel
    {
      match !class_del with
          None -> class_del := ci; []
        | Some _ -> raise (Asttools.SyntaxError ($startpos,$endpos))
    }

    imember:
| f=fundecl { Method f }

  clasmems:
| l=cmember*
    {
      let ci = !class_init in
      let cd = !class_del in
        class_init := None;
        class_del  := None;
        (ci,cd,List.flatten l)
    }

%inline plist(X):
| l=loption(delimited(LPAREN,separated_nonempty_list(COM,X),RPAREN))
    { l }


mclasmem:
| n=ID p=params inline=boption(CONST) DD t=texpr
    LCBRACE b=statement* RCBRACE
    {
      {
	fname = n;
	fnspace = [];
	(* We use the inline switch, but it has a
	 * different meaning here. *)
	finline = inline;
	fparams = p;
	frtype = t;
	fbody = b;
	floc = ($startpos, $endpos);
        finfo = None;
        fexport = true;
        fasname = None;
      }
    }

macroclass:
| MACRO CLASS nsi=idspace DD t=texpr
    LCBRACE m=mclasmem+ RCBRACE
    {
      let (x,ns) = nsi in
      {
	cmacro = (build_block x ($startpos(nsi), $endpos(nsi)));
        mcnspace = ns;
	store  = t;
	mc_ops = m;
	mcloc  = ($startpos, $endpos);
      }
    }

get:
| g=ID LCBRACE b=statement+ RCBRACE
    {
      if g <> "get"
      then raise (Asttools.SyntaxError ($startpos,$endpos));
      {
        fname = "get";
        fnspace = [];
        fparams = [];
        finline = false;
        frtype = Void (build_block () ($startpos,$endpos));
        fbody = b;
        floc = ($startpos,$endpos);
        finfo = None;
        fexport = false;
        fasname = None;
      }
    }

set:
| g=ID p=params LCBRACE b=statement+ RCBRACE
    {
      if g <> "set"
      then raise (Asttools.SyntaxError ($startpos,$endpos));
      {
        fname = "set";
        fnspace = [];
        fparams = p;
        finline = false;
        frtype = Void (build_block () ($startpos,$endpos));
        fbody = b;
        floc = ($startpos,$endpos);
        finfo = None;
        fexport = false;
        fasname = None;
      }
    }

innerprop:
| g=get s=set { (g,s)}
| s=set g=get { (g,s)}

property:
| PROPERTY n=ID LPAREN this=texpr RPAREN DD t=texpr
    LCBRACE inner=innerprop RCBRACE
    {
      let (get,set) = inner in
        {
          pname = n;
          properstore = t;
          properthis = this;
          getter = (get.frtype <- t; get);
          setter = set;
          properloc = ($startpos, $endpos);
        }
    }

asmopstm:
    x = terminated(ID,DD)?
    m=STR e=delimited(LPAREN,expr,RPAREN)
    {
      {
        asmopname = x;
        asmconstraint = m;
        asmexpr = e;
        oploc = ($startpos, $endpos);
      }
    }

asmblock:
| ASM ba=ASMBLOCK
    {
      {
        asmcode = ba;
        asminputs = [];
        asmouputs = [];
        asmclobbers = [];
        asmvolatile = true;
        asminfo = None;
        asmloc = ($startpos, $endpos);
      }
    }
| ASM LPAREN
        ao=separated_list(COM,asmopstm) DCOM
    ai=separated_list(COM,asmopstm) DCOM
    clob=separated_list(COM,STR)
    RPAREN v=VOLATILE?
    ba=ASMBLOCK
    {
      {
        asmcode = ba;
        asminputs = ai;
        asmouputs = ao;
        asmclobbers = clob;
        asmvolatile = true;
        asminfo = None;
        asmloc = ($startpos, $endpos);
      }
    }
