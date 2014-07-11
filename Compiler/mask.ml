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

(* Management of Integer Mask on return/assignements *)

open Ast

type conv_kind = No_conv | Mask of int64 | CCast

let nwbuilder ge = new AstBuilder.nwbuilder ge

let need_mask = function
  | (TypeAlg.Int (_,s1,_), TypeAlg.Int (_,s2,_))
      when s1 < s2 ->
      if TypeAlg.not_multiple s1 then Mask s1
      else CCast
  | _ -> No_conv

let mask_type t1 t2 =
  TypeAlg.Fun ([TypeAlg.Int (true,6L, TypeAlg.stdmeta ());t2],
               t1,TypeAlg.stdmeta ())

let mask_id (ge:'ge) t1 t2 loc =
  let builder:'ge AstBuilder.builder = ge#builder in
    builder#id "MASK" (TypeAlg.info (mask_type t1 t2) true true) loc

let call_mask ns (ge:'ge) e t1 t2 loc =
  let builder = nwbuilder ge in
  begin
    match need_mask (t1,t2) with
      | Mask size ->
          begin
            builder#call
              (mask_id ge t1 t2 loc)
              [builder#value (VInt size) loc; e]
              (TypeAlg.info t1 false true) loc
          end
      | CCast ->
          begin
            builder#cast t1 e loc
          end
      | No_conv -> e
  end


let mask_assign ns ge re =
  begin
    match !re with
      | AssignOp ({cont=(o,e1,e2)} as b) ->
          let t1 = TypeAlg.typeof e1 in
          let t2 = TypeAlg.typeof e2 in
          let loc = Asttools.expr_get_loc e2 in
            begin
              b.cont <- (o,e1,call_mask ns ge e2 t1 t2 loc);
              true
            end
      | Cast ({cont=(te,e); info=Some {TypeAlg.talg=t1}} as b) ->
          let t2 = TypeAlg.typeof e in
          let loc = Asttools.expr_get_loc e in
            begin
              b.cont <- (te,call_mask ns ge e t1 t2 loc);
              true
            end
      | Call ({cont=(f,al)} as b) ->
          let lt1 =
            match (ge#get_type_env ns)#canon (TypeAlg.typeof f) with
              | TypeAlg.Fun (plt,_,_) -> plt
              | _ -> assert false
          in
          let aux t1 e =
            let t2 = TypeAlg.typeof e in
            let loc = Asttools.expr_get_loc e in
              call_mask ns ge e t1 t2 loc
          in
            begin
              b.cont <- (f,List.map2 aux lt1 al);
              true
            end
      | _ -> false
  end

let mask_vdecl ns ge vd =
  begin
    let t1 = TypeAlg.typeof_texpr vd.vtype in
    match vd.vinit with
      | Some e ->
          let t2 = TypeAlg.typeof e in
          let loc = Asttools.expr_get_loc e in
            begin
              vd.vinit <- Some (call_mask ns ge e t1 t2 loc);
              true
            end
      | _ -> false
  end

let mask_ret ns ge rs =
  begin
    match !rs with
      | Return ({cont=Some e; info=Some {TypeAlg.talg=t1}} as b) ->
          let t2 = TypeAlg.typeof e in
          let loc = Asttools.expr_get_loc e in
          begin
            b.cont <- Some (call_mask ns ge e t1 t2 loc);
            true
          end
      | _ -> false
  end

let add_integer_mask (ns,ge) ast =
  let postfix =
    {
      AstRewriter.dont with
        AstRewriter.expr = Some mask_assign;
        AstRewriter.vdecl = Some mask_vdecl;
        AstRewriter.statement = Some mask_ret;
    }
  in
    AstRewriter.global ns ge AstRewriter.dont postfix ast
