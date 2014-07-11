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

(* Builder for AST nodes *)

open Ast


let rew b info =
  Some { info with TypeAlg.rewrited=b}

let op2l = function
  | Some e -> e::[]
  | None -> []

let freshID =
  let c = ref 0 in
    fun () ->
      incr c;
      "_cbtmp"^string_of_int !c

let tinfo t b = Some {
  TypeAlg.talg = t;
  TypeAlg.const = true;
  TypeAlg.rewrited = b;
  TypeAlg.need_conv = None;
}

let vinfo ge v b = Some {
  TypeAlg.talg = TypeAlg.value ge v;
  TypeAlg.const = true;
  TypeAlg.rewrited = b;
  TypeAlg.need_conv = None;
}

class ['ge] builder r (ge:'ge) =
object (self:'self)

  constraint 'info = TypeAlg.info

  val mutable rw = r
  method rwon = rw <- true
  method rwoff = rw <- false
  method isrw = rw

  method value v loc =
    Value {cont=v; loc=loc; info = vinfo ge v rw}
  method id ?ns x info loc =
    Id {
      cont=(op2l ns,x); loc=loc;
      info= rew rw info
    }
  method binop op e0 e1 info loc =
    BinOp {
      cont=(op,e0,e1); loc=loc;
      info = rew rw info;
    }
  method preop o e info loc =
    PreOp {
      cont=(o,e); loc=loc;
      info = rew rw info;
    }
  method postop o e info loc =
    PostOp {
      cont=(o,e); loc=loc;
      info = rew rw info;
    }
  method assignop op e0 e1 info loc =
    AssignOp {
      cont=(op,e0,e1); loc=loc;
      info = rew rw info;
    }
  method terop e0 e1 e2 info loc =
    TerOp {
      cont=(e0,e1,e2); loc=loc;
      info = rew rw info;
    }
  method call ef pl info loc =
    Call {
      cont=(ef,pl); loc=loc;
      info = rew rw info;
    }
  method index te il info loc =
    Index {
      cont=(te,il); loc=loc;
      info = rew rw info;
    }
  method field e f info loc =
    Field {
      cont=(e,f); loc=loc;
      info = rew rw info;
    }
  method pfield e f info loc =
    PField {
      cont=(e,f); loc=loc;
      info = rew rw info;
    }
  method cast t e loc =
    Cast {
      cont=(self#texpr t loc,e); loc=loc;
      info = tinfo t rw;
    }
  method sizeof t loc =
    Sizeof {
      cont=self#texpr t loc; loc=loc;
      info = tinfo TypeAlg.uintDef rw;
    }
  method decl vd info loc =
    Decl {
      cont=vd; loc=loc;
      info = rew rw info;
    }

  method swvalue v info loc =
    SwValue {
      cont=v; loc=loc;
      info = rew rw info;
    }
  method swdefault info loc =
    Default {
      cont=(); loc=loc;
      info = rew rw info;
    }

  method emptystatement loc =
    EmptyStatement {
      cont=(); loc=loc;
      info = Some (TypeAlg.info TypeAlg.Void true rw);
    }
  method expr e info loc =
    Expr {
      cont=e; loc=loc;
      info = rew rw info;
    }
  method block sl info loc =
    Block {
      cont=sl; loc=loc;
      info = rew rw info;
    }
  method ifthen e t info loc =
    If {
      cont=(e,t,self#emptystatement loc);
      loc=loc;
      info = rew rw info;
    }
  method ifelse e t el info loc =
    If {
      cont=(e,t,el);
      loc=loc;
      info = rew rw info;
    }
  method whilestm e s info loc =
    While {
      cont=(e,s); loc=loc;
      info = rew rw info;
    }
  method dostm e s info loc =
    Do {
      cont=(e,s); loc=loc;
      info = rew rw info;
    }
  method forstm ?e0 ?e1 ?e2 s info loc =
    For {
      cont=(e0,e1,e2,s); loc=loc;
      info = rew rw info;
    }
  method switch e swl info loc =
    Switch {
      cont=(e,swl); loc=loc;
      info = rew rw info;
    }
  method break info loc =
    Break {
      cont=(); loc=loc;
      info = rew rw info;
    }
  method continue info loc =
    Continue {
      cont=(); loc=loc;
      info = rew rw info;
    }
  method return ?e info loc =
    Return {
      cont=e; loc=loc;
      info = rew rw info;
    }

  method label lab loc =
    Label {
      cont=lab; loc=loc;
      info = Some (TypeAlg.info TypeAlg.Void true rw)
    }

  method goto lab loc =
    Goto {
      cont=lab; loc=loc;
      info = Some (TypeAlg.info TypeAlg.Void true rw)
    }

  method asm code inputs outputs clob vol loc =
    {
      asmcode = code;
      asminputs = inputs;
      asmouputs = outputs;
      asmclobbers = clob;
      asmvolatile = vol;
      asminfo = Some (TypeAlg.info TypeAlg.Void true rw);
      asmloc = loc;
    }

  method asmoperand ?name constr (e: TypeAlg.info expr) loc =
    {
      asmopname = name;
      asmconstraint = constr;
      asmexpr = e;
      oploc = loc;
    }

  method fundecl
    ?fasname
    fname ?fnspace fparams ?(exp=false)
    finline frtype fbody floc ft =
    {
      fname = fname;
      fnspace = op2l fnspace;
      fparams = fparams;
      finline = finline;
      frtype = self#texpr frtype floc;
      fbody = fbody;
      floc = floc;
      finfo = Some (TypeAlg.info ft true rw);
      fexport = exp;
      fasname = fasname;
    }

  method vdecl
    ?vmods vtype vname ?vinit vloc =
    {
      vmods = op2l vmods;
      vtype = self#texpr vtype vloc;
      vname = vname;
      vinit = vinit;
      vloc = vloc;
      vinfo = Some (TypeAlg.info vtype true rw)
    }

  method typedef tdname ?tdnspace tdtype loc =
    {
      tdname = tdname;
      tdnspace = op2l tdnspace;
      tdtype = self#texpr tdtype loc;
      tdloc = loc;
      tdinfo = Some (TypeAlg.info tdtype true rw);
    }

  method gfdecl (f : 'info fundecl) = FDecl f
  method gtdef (td : 'info typedef) = TDef td
  method gbdecl (b : 'info boxdecl) = BDecl b
  method gmcdecl (mc : 'info mclassdecl) = MCDecl mc
  method gvdecl (vd : 'info vdecl) = VDecl vd
  method gmacro (g : 'info gdecl) = Macro g

  method gimport (m:string) (info:'info) loc =
    Import {
      cont=m; loc=loc;
      info = rew rw info;
    }
  method ginclude (m:string list) (info:'info) loc =
    Include {
      cont=m; loc=loc;
      info = rew rw info;
    }

  method gmodparam export (imports:'info import list) =
    {
      modexport = export;
      modimports = imports;
    }

  method gsource (modparam:'info modparam) gdecls loc =
    {
      srcmodparam = modparam;
      srcgdecls = gdecls;
      srcloc = loc;
    }

  method stmvdecl x t ?vinit loc =
    let info = TypeAlg.info t true rw in
    let vd = self#vdecl t x ?vinit loc in
      self#expr (self#decl vd info loc) info loc

  method addfstparam md t x fd =
    fd.fparams <- (md,self#texpr t fd.floc,x)::fd.fparams;
  method addfdvdecl x t ?vinit fd loc =
    let vstm = self#stmvdecl x t ?vinit loc in
      fd.fbody <- vstm :: fd.fbody
  method addblockvdecl x t ?vinit sb loc =
    let b = match sb with
      | Block b -> b
      | _ -> assert false
    in
    let vstm = self#stmvdecl x t ?vinit loc in
      b.cont <- vstm :: b.cont

  (* Should add modifiers here *)
  method texpr t loc =
    match t with
      | TypeAlg.Void ->
        Void  {cont=(); loc=loc; info = tinfo t rw;}
      | TypeAlg.Char m ->
        TChar {cont=(); loc=loc; info = tinfo t rw;}
      | TypeAlg.Pointer (t',m) ->
          TPointer {
            cont=self#texpr t' loc; loc=loc;
            info = tinfo t rw;
          }
      | TypeAlg.Int (b,s,m) ->
          Num {
            cont=(b,self#value (VInt s) loc); loc = loc;
            info = tinfo t rw;
          }
      | TypeAlg.Float (s,m) ->
          TFloat {
            cont=self#value (VInt s) loc; loc = loc;
            info = tinfo t rw;
          }
      | TypeAlg.TypeName (n,m)
      | TypeAlg.MacroName (n,m) ->
          TName {
            cont=([],n); loc = loc;
            info = tinfo t rw;
          }
      | TypeAlg.NSTypeName (ns,n,m) ->
          TName {
            cont=([ns],n); loc = loc;
            info = tinfo t rw;
          }
      | TypeAlg.Fun (tl,rt,m) ->
          Fun {
            cont=(
              List.map (fun x -> self#texpr x loc) tl,
              self#texpr rt loc
            ); loc = loc;
            info = tinfo t rw;
          }
      | TypeAlg.Array (ta,dl,m) ->
          Array {
            cont = (
              self#texpr ta loc,
              List.map (TypeAlg.rebuild_expr self loc) dl
            ); loc = loc;
            info = tinfo t rw;
          }
      | TypeAlg.Ref t' ->
          TPointer {
            cont=self#texpr t' loc; loc=loc;
            info = tinfo t rw;
          }
      | TypeAlg.Modifiers (t,_) ->
        self#texpr t loc
      | _ -> assert false

  method compound t stmlist loc =
    Compound {
      cont = stmlist;
      loc = loc;
      info = Some (TypeAlg.info t true rw);
    }

  method dynamic_cast rt obj get_semfun target res_type itag loc =
    let tmp = freshID () in
    let t = target in
    let info = TypeAlg.info t true rw in
    let rinfo = TypeAlg.info res_type true rw in
    let vtmp = self#id tmp info loc in
    let ptmp =
      if rt
      then self#pfield vtmp "real_this"
        (TypeAlg.info TypeAlg.objbase true true) loc
      else vtmp
    in
    self#compound res_type
      [
        self#stmvdecl tmp t ~vinit:obj loc;
        self#expr (
          self#call
            ( get_semfun vtmp )
            [ptmp; self#value (VTag itag) loc]
            rinfo loc
        ) rinfo loc;
      ] loc

  method compoundwraper obj f vtbl param ft rt loc =
    let tmp = freshID () in
    let t = TypeAlg.typeof obj in
    let info = TypeAlg.info t true rw in
    let rinfo = TypeAlg.info rt true rw in
    let finfo = TypeAlg.info ft true rw in
    let vtmp = self#id tmp info loc in
      self#compound rt
        [
          self#stmvdecl tmp t ~vinit:obj loc;
          self#expr (
            self#call (
              self#pfield (
                self#pfield vtmp vtbl info loc
              ) f finfo loc
            ) (vtmp::param) rinfo loc
          ) rinfo loc;
        ] loc

  (* Interface call rewrite *)
  (* compound should be good, simple call should be check *)

  method compound_interwraper obj f ivtbl param ft rt loc =
    let tmp = freshID () in
    let t = TypeAlg.typeof obj in
    let info = TypeAlg.info t true rw in
    let rinfo = TypeAlg.info rt true rw in
    let finfo = TypeAlg.info ft true rw in
    let vtmp = self#id tmp info loc in
    let real_this = self#pfield vtmp "real_this" info loc in
      self#compound rt
        [
          self#stmvdecl tmp t ~vinit:obj loc;
          self#expr (
            self#call (
              self#pfield (
                self#pfield vtmp ivtbl info loc
              ) f finfo loc
            ) (real_this::param) rinfo loc
          ) rinfo loc;
        ] loc

end

class ['ge] rwbuilder ge =
object
  inherit ['ge] builder true ge
end

class ['ge] nwbuilder ge =
object
  inherit ['ge] builder false ge
end
