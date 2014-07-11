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

(* This a wraper for rewriting various call *)

(*
 * its use facitlites from the AstRewriter module, but let box rewrite
 * their themsleves.
 *)

open Ast

let rewrite_call (ns:string) ge re =
  match !re with
    | Call {cont=(Field {cont =(_ as e,_)}, _)}
        (* Match Call based on field for method invocation *)
        (* First find the type, and the corresponding box *)
      ->
        begin
          let t = TypeAlg.typeof e in
            try
              let box = (ge#get_type_env ns)#get_box_from_type t in
                box#rewrite_call ge#mangler ge#builder re;
                true
            with
              | Types.Not_boxed ->
                  let debug : Debug_printer.printer = ge#debug in
                  begin
                    debug#locprint 1 (Asttools.expr_get_loc e) ns
                      ("@[<b 2>Call Rewritter, expression is "
                       ^^"not a box:@;%t@]") (fun f -> Pp.expr f e);
                    false
                  end
        end
    | _ -> false (* default case do nothing *)

let delete (ns:string) (ge:'ge) rs =
  match !rs with
    | Delete ({cont=e;info=Some info} as b) ->
      let builder:'ge AstBuilder.builder = ge#builder in
      let box =
        (ge#get_type_env ns)#get_box_from_type (TypeAlg.typeof e)
      in
      let del =
        builder#id (box#get_del_name ge#mangler) info b.loc
      in
        begin
          rs := builder#expr
            (builder#call del [e] info b.loc)
            info b.loc;
          true
        end
    | _ -> false

let no_class ns ge bd = true

let postfix =
  {
    AstRewriter.dont with
      AstRewriter.expr = Some rewrite_call;
      AstRewriter.statement = Some delete;
  }

let prefix =
  {
    AstRewriter.dont with
      AstRewriter.boxdecl = Some no_class;
  }

let in_method ns ge fd =
  AstRewriter.fundecl ns ge prefix postfix fd

let do_it (ns,ge) ast =
  AstRewriter.global ns ge prefix postfix ast
