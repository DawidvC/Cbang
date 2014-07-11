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

(* AST rewriter generic mechanism *)

open Ast

(*
  The idea is to provide traversal functions that take care of deep
  recursion and apply your rewriter facilities to AST's nodes.

  The mechanism is simple: each encounter node is given to your
  function (if some) and then the depth first traversal continue.

  Double stop mechanism: there's two to stop traversal depending on
  your own functions and the state of tree.
  - If your rewriter returns true (meaning that it has changed the
  tree) we stop here (or at least go to another branch)
  - If the tree is marked as rewrited (inside info) we stop without
  even passing it to your rewriter.

  Beware that some construction don't have the rewrited flag
  (basically global declarations.)

  Your rewriter don't have to care about all nodes, you just match
  what you care of, and default return false.

  We also provide a do nothing rewriters record so just have to
  copy-override it, providing only the fields you need.

  Oh, and if you need to apply something in postfix (after traversal)
  it's also possible ;)

  UPDATE: we work with ref now, so you can replace the node and not
  just modify its content.

*)

(* The only, important, missing features is multiple replacement for
   statements. Actually the only solution to replace a statement with
   more than one (or, eventualy to suppress it) you'll need to build
   your own mechanism (at least until you reach your statement ... )
*)

(*
  TO CHECK: we (silently) assume that all info were updated in
  locblock nodes, this need intensive testing !
  Note that newly created nodes using module AstBuilder have an info.
*)

type 'ge rewriters = {
  expr : (string -> 'ge -> TypeAlg.info expr ref -> bool) option;
  swtest : (string -> 'ge -> TypeAlg.info swtest ref -> bool) option;
  statement :
    (string -> 'ge -> TypeAlg.info statement ref -> bool) option;
  fundecl : (string -> 'ge -> TypeAlg.info fundecl -> bool) option;
  texpr : (string -> 'ge -> TypeAlg.info texpr ref -> bool) option;
  vdecl : (string -> 'ge -> TypeAlg.info vdecl -> bool) option;
  member : (string -> 'ge -> TypeAlg.info member ref -> bool) option;
  innerboxdecl :
    (string -> 'ge -> TypeAlg.info innerboxdecl -> bool) option;
  boxdecl : (string -> 'ge -> TypeAlg.info boxdecl ref -> bool) option;
  mclassdecl :
    (string -> 'ge -> TypeAlg.info mclassdecl -> bool) option;
  typedef : (string -> 'ge -> TypeAlg.info typedef -> bool) option;
  import : (string -> 'ge -> TypeAlg.info import ref -> bool) option;
  gdecl : (string -> 'ge -> TypeAlg.info gdecl ref -> bool) option;
  source : (string -> 'ge -> TypeAlg.info source ref -> bool) option;
}

(* do nothing predefined rewriters block *)
(* you can use it if you have only few rewriters (or none with
   post/pre mechanism.)
   For example, you juste want to rewrite expression using function
   foo and vdecl using bar, you can define your rewriter as:

   * let my_rewriters = {
   *   AstRewriter.dont with
   *     expr = foo;
   *     vdecl = bar;
   * }

*)
let dont = {
  expr = None;
  swtest = None;
  statement = None;
  fundecl = None;
  texpr = None;
  vdecl = None;
  member = None;
  innerboxdecl = None;
  boxdecl = None;
  mclassdecl = None;
  typedef = None;
  import = None;
  gdecl = None;
  source = None;
}

(* tools *)

let testapply ns ge x = function
  | Some f -> f ns ge x
  | None -> false

let opapply f ns ge pr po = function
  | Some e -> f ns ge pr po e
  | None -> ()

let testrw info = info.TypeAlg.rewrited

(* entry points *)

let rec expr ns ge prefix postfix e =
  try
    let cont = expr ns ge prefix postfix in
      if not (testrw (Asttools.expr_get_info !e)) then
        begin
          if not (testapply ns ge e prefix.expr) then
            begin
              match !e with
                | BinOp ({cont=(o,e0,e1)} as b)
                | AssignOp ({cont=(o,e0,e1)} as b) ->
                    let re0 = ref e0 and re1 = ref e1 in
                      cont re0; cont re1;
                      b.cont <- (o,!re0,!re1)
                | Field ({cont=(e,f)} as b)
                | PField ({cont=(e,f)} as b) ->
                    let re = ref e in
                      cont re;
                      b.cont <- (!re,f)
                | PreOp ({cont=(o,e)} as b)
                | PostOp ({cont=(o,e)} as b) ->
                    let re = ref e in
                      cont re;
                      b.cont <- (o,!re)
                | TerOp ({cont=(e0,e1,e2)} as b) ->
                    let re0 = ref e0 and re1 = ref e1 and re2 = ref e2 in
                      cont re0; cont re1; cont re2;
                      b.cont <- (!re0,!re1,!re2)
                | Index ({cont=(e,el)} as b)
                | Call ({cont=(e,el)} as b)->
                    let re = ref e and rel = List.map (ref) el in
                      cont re;
                      List.iter cont rel;
                      b.cont <- (!re, List.map (!) rel)
                | Cast ({cont=(t,e)} as b) ->
                    let rt = ref t and re = ref e in
                      texpr ns ge prefix postfix rt;
                      cont re;
                      b.cont <- (!rt,!re)
                | Sizeof ({cont=t} as b) ->
                    let rt = ref t in
                      texpr ns ge prefix postfix rt;
                      b.cont <- !rt
                | Decl {cont=vd} ->
                    vdecl ns ge prefix postfix vd;
                | _ -> ()
            end;
          ignore (testapply ns ge e postfix.expr)
        end
  with
    | x ->
      begin
        let debug:Debug_printer.printer = ge#debug in
          debug#locprint 1 (Asttools.expr_get_loc !e) ""
            ("@[<b 2>AST Rewriter internal error in expression:@;"
             ^^"%t@]") (fun f -> Pp.expr f !e);
          raise x
      end

and swtest ns ge prefix postfix sw =
  if not (testrw (Asttools.swtest_get_info !sw)) then
    ignore (testapply ns ge sw prefix.swtest)

and statement ns ge prefix postfix s =
  let cont = statement ns ge prefix postfix in
    if not (testrw (Asttools.stm_get_info !s)) then
      begin
        if not (testapply ns ge s prefix.statement) then
          begin
            match !s with
              | Delete ({cont=e} as b)
              | Expr ({cont=e} as b) ->
                  let re = ref e in
                    expr ns ge prefix postfix re;
                    b.cont <- !re
              | Block ({cont=sl} as b)->
                  let rsl = List.map (ref) sl in
                    List.iter cont rsl;
                    b.cont <- List.map (!) rsl
              | If ({cont=(e,s0,s1)} as b) ->
                  let re = ref e and rs0 = ref s0 and rs1 = ref s1 in
                    expr ns ge prefix postfix re;
                    cont rs0;
                    cont rs1;
                    b.cont <- (!re,!rs0,!rs1)
              | While ({cont=(e,s)} as b)
              | Do ({cont=(e,s)} as b) ->
                  let re = ref e and rs = ref s in
                    expr ns ge prefix postfix re;
                    cont rs;
                    b.cont <- (!re,!rs)
              | For ({cont=(e0,e1,e2,s)} as b) ->
                  let re0 = ref e0 and re1 = ref e1 and re2 = ref e2
                                                    and rs = ref s in
                    List.iter (
                      fun re -> (
                        match !re with
                          | Some e ->
                              begin
                                let re0 = ref e in
                                  expr ns ge prefix postfix re0;
                                  re := Some !re0
                              end
                          | None -> ()
                      )
                    ) [re0;re1;re2];
                    cont rs;
                    b.cont <- (!re0,!re1,!re2,!rs)
              | Switch ({cont=(e0,swl)} as b) ->
                  let re0 = ref e0
                  and rswl = List.map (fun (x,y) -> (ref x, ref y)) swl in
                    expr ns ge prefix postfix re0;
                    List.iter (
                      fun (sw,sl) ->
                        let rsl = List.map (ref) !sl in
                          swtest ns ge prefix postfix sw;
                          List.iter cont rsl;
                          sl := List.map (!) rsl;
                    ) rswl;
                    b.cont <- (!re0, List.map (fun (x,y) -> (!x,!y)) rswl);
              | Return ({cont=Some e0} as b) ->
                  let re0 = ref e0 in
                    expr ns ge prefix postfix re0;
                    b.cont <- Some !re0
              | Asm {cont=asm} ->
                List.iter (
                  asmoperand ns ge prefix postfix) asm.asminputs;
                List.iter (
                  asmoperand ns ge prefix postfix) asm.asmouputs;
              | _ -> ()
          end;
        ignore (testapply ns ge s postfix.statement)
      end

and asmoperand ns ge prefix postfix asmop =
  let re = ref asmop.asmexpr in
    expr ns ge prefix postfix re;
    asmop.asmexpr <- !re

and fundecl ns ge prefix postfix fd =
  if not (testapply ns ge fd prefix.fundecl) then
    begin
      let rfp = List.map (fun (a,t,b) -> (a,ref t,b)) fd.fparams in
      let rfrt = ref fd.frtype in
      let rfb = List.map (ref) fd.fbody in
        List.iter (fun (_,t,_) ->
                     texpr ns ge prefix postfix t) rfp;
        fd.fparams <- List.map (fun (a,rt,b) -> (a,!rt,b)) rfp;
        texpr ns ge prefix postfix rfrt;
        fd.frtype <- !rfrt;
        List.iter (statement ns ge prefix postfix) rfb;
        fd.fbody <- List.map (!) rfb;
    end;
  ignore (testapply ns ge fd postfix.fundecl)

and vdecl ns ge prefix postfix vd =
  if not (testapply ns ge vd prefix.vdecl) then
    begin
      let rvt = ref vd.vtype in
        texpr ns ge prefix postfix rvt;
        vd.vtype <- !rvt;
        match vd.vinit with
          | Some e ->
              let re = ref e in
                expr ns ge prefix postfix re;
                vd.vinit <- Some !re;
          | _ -> ()
    end;
  ignore (testapply ns ge vd postfix.vdecl)

and texpr ns ge prefix postfix t =
  try
    let cont = texpr ns ge prefix postfix in
      if not (testrw (Asttools.texpr_get_info !t)) then
        begin
          if not (testapply ns ge t prefix.texpr) then
            begin
              match !t with
                | Num ({cont=(s,e)} as b) ->
                    let re = ref e in
                      expr ns ge prefix postfix re;
                      b.cont <- (s,!re)
                | TFloat ({cont=e} as b) ->
                    let re = ref e in
                      expr ns ge prefix postfix re;
                      b.cont <- !re
                | TPointer ({cont=t} as b) ->
                    let rt = ref t in
                      cont rt;
                      b.cont <- !rt
                | Array ({cont=(t,el)} as b) ->
                    let rt = ref t and rel = List.map (ref) el in
                      cont rt;
                      List.iter (expr ns ge prefix postfix) rel;
                      b.cont <- (!rt, List.map (!) rel)
                | Fun ({cont=(tl,t)} as b) ->
                    let rt = ref t and rtl = List.map (ref) tl in
                      List.iter cont rtl;
                      cont rt;
                      b.cont <- (List.map (!) rtl, !rt)
                | _ -> ()
            end;
          ignore (testapply ns ge t postfix.texpr)
        end
  with
    | x ->
      begin
        let debug : Debug_printer.printer = ge#debug in
          debug#locprint 1
            (Asttools.texpr_get_loc !t)
            "" ("@[<b 2>AST Rewriter internal error in type:@;"
                ^^"%t@]") (fun f -> Pp.texpr f !t);
          raise x
      end

and member ns ge prefix postfix m =
  match !m with
    | (Method _ | SField _) ->
        begin
          if not (testapply ns ge m prefix.member) then
            begin
              match !m with
                | Method f ->
                    fundecl ns ge prefix postfix f
                | SField vd ->
                    vdecl ns ge prefix postfix vd
                | _ -> ()
            end;
          ignore (testapply ns ge m postfix.member)
        end
    | EField ({cont=(f,e); info=Some info} as b) ->
        if not (testrw info) then
          begin
            if not (testapply ns ge m prefix.member) then
              begin
                match e with
                  | Some e0 ->
                      let re0 = ref e0 in
                        expr ns ge prefix postfix re0;
                        b.cont <- (f,Some !re0)
                  | _ -> ()
              end;
            ignore (testapply ns ge m postfix.member)
          end
    | _ -> ()

and innerboxdecl ns ge prefix postfix ib =
  if not (testapply ns  ge ib prefix.innerboxdecl) then
    begin
      let rbm = List.map (ref) ib.members in
        List.iter (member ns ge prefix postfix) rbm;
        ib.members <- List.map (!) rbm;
        opapply fundecl ns ge prefix postfix ib.constructor;
        opapply fundecl ns ge prefix postfix ib.destructor;
    end;
  ignore (testapply ns  ge ib postfix.innerboxdecl)

and boxdecl ns ge prefix postfix bd =
  if not (testrw (Asttools.boxdecl_get_info !bd)) then
    begin
      if not (testapply ns ge bd prefix.boxdecl) then
        begin
          match !bd with
            | Struct {cont=(_,ib)} ->
                innerboxdecl ns ge prefix postfix ib
            | Union {cont=ib}
            | Enum {cont=ib}
            | Interface {cont=ib}
            | Class {cont=ib} ->
                innerboxdecl ns ge prefix postfix ib
        end;
      ignore (testapply ns ge bd postfix.boxdecl)
    end

and mclassdecl ns ge prefix postfix mc =
  if not (testapply ns ge mc prefix.mclassdecl) then
    begin
      let rms = ref mc.store in
        texpr ns ge prefix postfix rms;
        mc.store <- !rms;
        List.iter (fundecl ns ge prefix postfix) mc.mc_ops;
    end;
  ignore (testapply ns ge mc postfix.mclassdecl)

and typedef ns ge prefix postfix td =
  if not (testapply ns ge td prefix.typedef) then
    begin
      let rtdt = ref td.tdtype in
        texpr ns ge prefix postfix rtdt;
        td.tdtype <- !rtdt
    end;
  ignore (testapply ns ge td postfix.typedef)

and import ns ge prefix postfix i =
  let testrw = match !i with
    | Import {info = Some i}
    | Include {info = Some i} -> i.TypeAlg.rewrited
    | _ -> false
  in
    if not testrw then
      begin
        ignore (testapply ns ge i prefix.import);
        ignore (testapply ns ge i postfix.import);
      end

and modparam ns ge prefix postfix mp =
  let ri = List.map (ref) mp.modimports in
    List.iter (import ns ge prefix postfix) ri;
    mp.modimports <- List.map (!) ri;

(* and mapdecl ns ge prefix postfix md = *)

and gdecl ns ge prefix postfix gd =
  if not (testapply ns ge gd prefix.gdecl) then
    begin
      match !gd with
        | FDecl fd  -> fundecl ns ge prefix postfix fd
        | TDef td   -> typedef ns ge prefix postfix td
        | BDecl bd  ->
            let rbd = ref bd in
              boxdecl ns ge prefix postfix rbd;
              gd := BDecl !rbd
        | VDecl vd  -> vdecl   ns ge prefix postfix vd
        | Macro mgd  ->
            let rgd = ref mgd in
              gdecl ns ge prefix postfix rgd;
              gd := Macro !rgd
        | MCDecl mc -> mclassdecl ns ge prefix postfix mc
        | Map _ -> (* ugly *) ()
        | Proper _ -> (* ugly *) ()
    end;
  ignore (testapply ns ge gd postfix.gdecl)

let global ns ge prefix postfix src =
  if not (testapply ns ge src prefix.source) then
    let rgl = List.map (ref) (!src).srcgdecls in
      modparam ns ge prefix postfix (!src).srcmodparam;
      List.iter (gdecl ns ge prefix postfix) rgl;
      (!src).srcgdecls <- List.map (!) rgl
