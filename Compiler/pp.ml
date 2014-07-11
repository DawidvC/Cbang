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

let pp fmt = Format.fprintf fmt

let boxstr fmt s = pp fmt "@,@[<h>%s@]" s

let value fmt = function
  | VInt i -> pp fmt "@,%Ld" i
  | VFloat f -> pp fmt "@,%g" f
  | VChar c -> pp fmt "@,'%s'" (Str_escaping.char c)
  | VCstStr s -> pp fmt "@,@[<h>%s@]" (Str_escaping.str s)
  | VTag tag -> pp fmt "@,@[<h>`%s@]" tag

let rec print_ns fmt n = function
  | [] -> pp fmt "%s" n
  | h::t -> pp fmt "%s::" h; print_ns fmt n t

let rec expr fmt = function
  | Value v -> value fmt v.cont
  | Id {cont=(ns,x)} -> pp fmt "@,"; print_ns fmt x ns
  | BinOp {cont=(o,e1,e2)} ->
    begin
      pp fmt "@,@[<b 2>(";
      expr fmt e1;
      pp fmt "@;%s " o;
      expr fmt e2;
      pp fmt ")@]"
    end
  | AssignOp {cont=(o,e1,e2)} ->
    begin
      pp fmt "@,@[<b 2>";
      expr fmt e1;
      pp fmt "@;%s " o;
      expr fmt e2;
      pp fmt "@]"
    end
  | PreOp {cont=(o,e)} ->
    begin
      pp fmt "@,@[<h>%s" o;
      expr fmt e;
      pp fmt "@]"
    end
  | PostOp {cont=(o,e)} ->
    begin
      pp fmt "@,@[<h>";
      expr fmt e;
      pp fmt "%s@]" o
    end
  | TerOp {cont=(e1,e2,e3)} ->
    begin
      pp fmt "@,@[<b 2>";
      expr fmt e1;
      pp fmt "@,?@;";
      expr fmt e2;
      pp fmt "@,:@;";
      expr fmt e3;
      pp fmt "@]"
    end
  | Call {cont=(Id _ as f, al)} ->
    begin
      pp fmt "@,@[<h>";
      expr fmt f;
      pp fmt "(@[<b>";
      Pptools.seppp fmt "," (expr fmt) al;
      pp fmt "@])";
      pp fmt "@]"
    end
  | Call {cont=(f,al)} ->
    begin
      pp fmt "@,@[<h>(";
      expr fmt f;
      pp fmt ")(@[<b>";
      Pptools.seppp fmt "," (expr fmt) al;
      pp fmt "@])";
      pp fmt "@]"
    end
  | Index {cont=(e,el)} ->
    begin
      pp fmt "@,@[<h>";
      expr fmt e;
      pp fmt "[@[<b>";
      Pptools.seppp fmt "," (expr fmt) el;
      pp fmt "@]]";
      pp fmt "@]"
    end
  | Field {cont=(e,f)} ->
    begin
      pp fmt "@,@[<h>";
      expr fmt e;
      pp fmt ".%s@]" f
    end
  | PField {cont=(e,f)} ->
    begin
      pp fmt "@,@[<h>";
      expr fmt e;
      pp fmt "->%s@]" f
    end
  | Cast {cont=(t,e)} ->
    begin
      pp fmt "@,@[<h>(";
      expr fmt e;
      pp fmt "@,:";
      texpr fmt t;
      pp fmt ")@]"
    end
  | Sizeof {cont=t} ->
    begin
      pp fmt "@,@[<h>sizeof (";
      texpr fmt t;
      pp fmt ")@]"
    end
  | Decl {cont=vd} -> vdecl fmt vd
  | Compound {cont=sl} ->
    begin
      pp fmt "@,@[<v>({@[<v>";
      List.iter (statement fmt) sl;
      pp fmt "@]@,})@]";
    end
  | SelfInit _ -> () (* Should be handled differently *)

and vdecl ?(macro=false) fmt ?(cbi=false) vd =
  begin
    if not macro then pp fmt "@,";
    pp fmt "@[<h>";
    pp fmt "%s" vd.vname;
    begin
      match vd.vinit with
        | Some (SelfInit {cont=(_,args)}) ->
          if not cbi then
            begin
              pp fmt "@;:=@;";
              texpr fmt vd.vtype;
              pp fmt "(";
              Pptools.seppp fmt "," (expr fmt) args;
              pp fmt ")"
            end
        | _ ->
          begin
            pp fmt "@,:@;";
            Pptools.seppp fmt " " (pp fmt "%s") vd.vmods;
            texpr fmt vd.vtype;
            if not cbi then
              begin
                match vd.vinit with
                  | Some e ->
                    pp fmt "@;="; expr fmt e
                  | _ -> ()
              end;
          end;
    end;
    pp fmt "@]"
  end

and texpr fmt = function
  | Void _ -> boxstr fmt "void"
  | TName {cont=(ns,tn)} ->
    pp fmt "@;@[<h>"; print_ns fmt tn ns; pp fmt "@]"
  | TChar _ -> boxstr fmt "char"
  | Num {cont=(s,e)} ->
    begin
      pp fmt "@;@[<h>int<";
      if s then pp fmt "+";
      expr fmt e;
      pp fmt ">@]"
    end
  | TFloat {cont=e} ->
    begin
      pp fmt "@;@[<h>float<";
      expr fmt e;
      pp fmt ">@]"
    end
  | TPointer {cont=
      (
        Void _ | TName _ | Num _ | TFloat _
      ) as t} ->
    begin
      pp fmt "@;@[<h>";
      texpr fmt t;
      pp fmt "*@]"
    end
  | TPointer {cont=t} ->
    begin
      pp fmt "@;@[<h>(";
      texpr fmt t;
      pp fmt ")*@]"
    end
  | Array {cont=(t,el)} ->
    begin
      pp fmt "@;@[<h>";
      texpr fmt t;
      pp fmt "[";
      Pptools.seppp fmt "," (expr fmt) el;
      pp fmt "]@]"
    end
  | Fun {cont=(pl,rt)} ->
    begin
      pp fmt "@;@[<h><(";
      Pptools.seppp fmt "," (texpr fmt) pl;
      pp fmt ")>@;:@;";
      texpr fmt rt;
      pp fmt "@]"
    end
  | TModifier {cont=(m,t)} ->
      begin
        pp fmt "@;@[<h>";
        texpr fmt t;
        pp fmt "@;%s@]" m;
      end

and exprop ?(pref="") fmt = function
  | Some e -> pp fmt "%s" pref; expr fmt e
  | _ -> ()

and statement fmt = function
  | EmptyStatement _ -> boxstr fmt ";"
  | Delete {cont=e} ->
    begin
      pp fmt "@,@[<h>del(";
      expr fmt e;
      pp fmt ")@]";
    end
  | Expr {cont=e} ->
    begin
      pp fmt "@,@[<b 2>";
      expr fmt e;
      pp fmt "@];"
    end
  | Block {cont=sl} ->
    begin
      pp fmt "@,@[<v>{@[<v 1>";
      List.iter (statement fmt) sl;
      pp fmt "@]@,}@]"
    end
  | If {cont=(e,s,EmptyStatement _)} ->
    begin
      pp fmt "@,@[<v 2>if@[<b 1>@;(";
      expr fmt e;
      pp fmt ")@]";
      statement fmt s;
      pp fmt "@]";
    end
  | If {cont=(e,s1,s2)} ->
    begin
      pp fmt "@,@[<v 2>if@[<b 1>@;(";
      expr fmt e;
      pp fmt ")@]";
      statement fmt s1;
      pp fmt "@]@,@[<v 2>else";
      statement fmt s2;
      pp fmt "@]"
    end
  | While {cont=(e,s)} ->
    begin
      pp fmt "@,@[<v 2>while@[<b 1>@;(";
      expr fmt e;
      pp fmt ")@]";
      statement fmt s;
      pp fmt "@]";
    end
  | Do {cont=(e,s)} ->
    begin
      pp fmt "@,@[<v 2>do";
      statement fmt s;
      pp fmt "while@[<b 1>@;(";
      expr fmt e;
      pp fmt ")@];@]";
    end
  | For {cont=(e1,e2,e3,s)} ->
    begin
      pp fmt "@,@[<v 2>for@[<b 1>@;(";
      Pptools.seppp fmt ";" (exprop fmt) [e1;e2;e3];
      pp fmt ")@]";
      statement fmt s;
      pp fmt "@]";
    end
  | Switch {cont=(e,sl)} ->
    let rec case (c,sl) =
      pp fmt "@,@[<v 2>@[<h>";
      begin
        match c with
          | Default _ -> boxstr fmt "default"
          | SwValue {cont=v} ->
            pp fmt "@[<h>case "; value fmt v; pp fmt "@]"
      end;
      pp fmt ": @]";
      List.iter (statement fmt) sl;
      pp fmt "@]"
    in
      begin
        pp fmt "@,@[<v 2>switch@[<b 1>@;(";
        expr fmt e;
        pp fmt ")@]@,{@[<v 1>";
        List.iter case sl;
        pp fmt "@]@,}@]"
      end
  | Continue _ -> boxstr fmt "continue"
  | Break _ -> boxstr fmt "break"
  | Return {cont=eo} ->
    begin
      pp fmt "@,@[<h>return@;";
      exprop fmt eo;
      pp fmt "@,;@]";
    end
  | Label {cont=label} ->
    pp fmt "@,@[<h>@@%s:@]" label
  | Goto {cont=label} ->
    pp fmt "@,@[<h>goto @@%s;@]" label
  | Asm {cont=asm} -> asmblock fmt asm

and asmoperand fmt asmop =
  let name =
    match asmop.asmopname with
      | Some x -> x^":"
      | None -> ""
  in
    begin
      pp fmt "@[<h>%s@;%S(" name asmop.asmconstraint;
      expr fmt asmop.asmexpr;
      pp fmt ")@]"
    end

and asmblock fmt asm =
  begin
    pp fmt "@,@[<v>@[<h>asm (@[<b>";
    Pptools.seppp fmt "," (asmoperand fmt) asm.asmouputs;
    pp fmt ";@;";
    Pptools.seppp fmt "," (asmoperand fmt) asm.asminputs;
    pp fmt ";@;";
    Pptools.seppp fmt "," (
      pp fmt "%S"
    ) asm.asmclobbers;
    pp fmt "@])%s@]"
      (if asm.asmvolatile then " volatile" else "");
    pp fmt "@,:{%s@,}@]" asm.asmcode;
  end

let param fmt (ml,t,p) =
  begin
    pp fmt "@,%s:@;" p;
    List.iter (pp fmt "%s@;") ml;
    texpr fmt t;
  end

let body fmt ?(cbi=false) b fasname =
  match (cbi,b,fasname) with
    | (_,_,Some f) ->
      pp fmt "@[<h>@;as@;%s;@]" f
    | (true,_,_) | (_,[],_) ->
      pp fmt ";"
    | (_,[EmptyStatement _],_) ->
      pp fmt "@;{}"
    | _ ->
      pp fmt "@,{@[<v 1>";
      List.iter (statement fmt) b;
      pp fmt "@]@,}"

let fdecl ?(cbi=false) ?(macro=false) ?(inl="inline") fmt f =
  if (not cbi) || (f.fexport && not f.finline) then
    begin
      if not macro then pp fmt "@,";
      pp fmt "@[<v>@[<h>";
      print_ns fmt f.fname f.fnspace;
      pp fmt "(@[<h>";
      Pptools.seppp fmt "," (param fmt) f.fparams;
      pp fmt "@])";
      if f.finline then boxstr fmt inl;
      pp fmt "@;:";
      if not f.fexport then
        pp fmt "@;static";
      texpr fmt f.frtype;
      pp fmt "@]";
      body fmt ~cbi f.fbody f.fasname;
      pp fmt "@]@,";
    end

let typedef fmt td =
  begin
    pp fmt "@,@[<h>typedef@;%s@;=" td.tdname;
    texpr fmt td.tdtype;
    pp fmt ";@]@,"
  end

let rec member fmt ?(cbi=false) = function
  | Method f -> fdecl fmt ~cbi f
  | SField v -> pp fmt "@,@[<h>"; vdecl fmt ~cbi v; pp fmt ";@]"
  | EField {cont=(ename,eopt)} ->
    begin
      pp fmt "@,@[<h>%s" ename;
      exprop ~pref:" = " fmt eopt;
      pp fmt ";@]";
    end

let constructor fmt ?(cbi=false) f =
  begin
    pp fmt "@,@[<v>@[<h>%s@;" f.fname;
    pp fmt "(@[<h>";
    Pptools.seppp fmt "," (param fmt) f.fparams;
    pp fmt "@])@]";
    body fmt ~cbi f.fbody None;
    pp fmt "@]@,";
  end

let destructor fmt ?(cbi=false) f =
  begin
    pp fmt "@,@[<v>@[<h>%s@;" f.fname;
    pp fmt "(@[<h>";
    Pptools.seppp fmt "," (param fmt) f.fparams;
    pp fmt "@])@]";
    body fmt ~cbi f.fbody None;
    pp fmt "@]@,";
  end

let innerbox fmt ?(cbi=false) b =
  begin
    pp fmt "@,{@[<v 1>";
    begin
      match b.constructor with
        | Some c ->
          constructor fmt ~cbi c
        | _ -> ()
    end;
    begin
      match b.destructor with
        | Some c ->
          destructor fmt ~cbi c
        | _ -> ()
    end;
    List.iter (member ~cbi fmt) b.members;
    pp fmt "@]@,}"
  end

let boxdecl ?(cbi=false) fmt = function
  | Class {cont=b} ->
    begin
      pp fmt "@,@[<v>@[<h>class@;" ;
      print_ns fmt b.bname.cont b.bnspace;
      begin
        match b.parent with
          | Some p -> pp fmt ":@;%s@;" p
          | _ -> ()
      end;
      pp fmt "@]";
      innerbox fmt ~cbi b;
      pp fmt "@]@,";
    end
  | Interface {cont=b} ->
      pp fmt "@,@[<v>@[<h>interface@;" ;
      print_ns fmt b.bname.cont b.bnspace;
      begin
        match b.parent with
          | Some p -> pp fmt ":@;%s@;" p
          | _ -> ()
      end;
      pp fmt "@]";
      innerbox fmt ~cbi b;
      pp fmt "@]@,";
  | Struct {cont=(p,b)} ->
    begin
      pp fmt "@,@[<v>@[<h>struct@;" ;
      if p then pp fmt "packed@;";
      print_ns fmt b.bname.cont b.bnspace;
      pp fmt "@]";
      innerbox fmt ~cbi b;
      pp fmt "@]@,";
    end
  | Union {cont=b} ->
    begin
      pp fmt "@,@[<v>@[<h>union@;" ;
      print_ns fmt b.bname.cont b.bnspace;
      pp fmt "@]";
      innerbox fmt b;
      pp fmt "@]@,";
    end
  | Enum {cont=b} ->
    begin
      pp fmt "@,@[<v>@[<h>enum@;" ;
      print_ns fmt b.bname.cont b.bnspace;
      pp fmt "@]";
      innerbox fmt ~cbi b;
      pp fmt "@]@,";
    end

let mclassdecl fmt ?(cbi=false) mc =
  begin
    pp fmt "@,@[<v>@[<h>macro class@;%s : " mc.cmacro.cont;
    texpr fmt mc.store;
    pp fmt "@]@,{@[<v 1>";
    List.iter (fdecl ~inl:"const" fmt) mc.mc_ops;
    pp fmt "@]@,}@]@,"
  end

let gdecl fmt ?(cbi=false) = function
  | FDecl f -> fdecl fmt ~cbi f
  | TDef t -> typedef fmt t
  | VDecl v ->
    if (not cbi) || (not (Asttools.vdecl_static v)) then
      (pp fmt "@,@[<h>"; vdecl fmt ~cbi v; pp fmt ";@]@,")
  | Map (v,x) ->
    pp fmt "@,@[<h>"; vdecl fmt ~cbi v; pp fmt " as %s ;@]@," x
  | BDecl b -> boxdecl fmt ~cbi b
  | MCDecl m -> mclassdecl fmt ~cbi m
  | Macro (FDecl f) ->
    pp fmt "@,@[<h>#"; fdecl ~macro:true fmt ~cbi f; pp fmt "@]@,"
  | Macro (VDecl v) ->
    pp fmt "@,@[<h>#"; vdecl ~macro:true fmt  ~cbi v; pp fmt ";@]@,"
  (* TODO: properties *)
  | _ -> ()

let modparam fmt = function {modexport=exp; modimports=is} ->
  let import fmt = function
    | Import {cont=module_name} ->
      pp fmt "@,@[<h>import %s;@]@," module_name
    | Include {cont=l} ->
      pp fmt "@,@[<b 2>include@;%S" (List.hd l);
      List.iter (fun i -> pp fmt ",@;%S" i) (List.tl l);
      pp fmt ";@]@,"
    | Open {cont=mname} ->
      pp fmt "@,@[<h>open_import %s;@]@," mname
  and _ = exp in
    List.iter (import fmt) is

let source fmt ?(cbi=false) =
  function {srcmodparam=mp; srcgdecls=dl} ->
    modparam fmt mp;
    List.iter (gdecl ~cbi fmt) dl

let prettyPrint ?(cbi=false) ast =
  source Format.std_formatter ~cbi ast
