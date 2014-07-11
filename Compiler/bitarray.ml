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

(* Integer as Bit array *)

open Ast

(* global macro *)
let gen_macro fmt =
  begin
    Format.fprintf fmt "@,@[<h>#define BITPUT(X,i,b)@;";
    Format.fprintf fmt "((b)?((X) |= (1<<(i))):((X) &= ~(1<<(i))))@]";
    Format.fprintf fmt "@,@[<h>#define BITGET(X,i)@;";
    Format.fprintf fmt "( !!((X)&(1<<(i))) )@]@,";
  end

let bitput_type =
  TypeAlg.Fun (
    [
      TypeAlg.Int (true,64L, TypeAlg.stdmeta ());
      TypeAlg.Int (true,8L, TypeAlg.stdmeta ());
      TypeAlg.Int (true,1L, TypeAlg.stdmeta ());
    ], TypeAlg.Int (true,1L, TypeAlg.stdmeta ()),
    TypeAlg.stdmeta ()
  )

let bitget_type =
  TypeAlg.Fun (
    [
      TypeAlg.Int (true,64L, TypeAlg.stdmeta ());
      TypeAlg.Int (true,8L, TypeAlg.stdmeta ());
    ], TypeAlg.Int (true, 1L, TypeAlg.stdmeta ()),
    TypeAlg.stdmeta ()
  )

let bitfinfo t =
  TypeAlg.info t true true

let bitput ns (ge:'ge) loc =
  let builder:'ge AstBuilder.builder = ge#builder in
    begin
      builder#id "BITPUT" (bitfinfo bitput_type) loc
    end

let bitget ns (ge:'ge) loc =
  let builder:'ge AstBuilder.builder = ge#builder in
    begin
      builder#id "BITGET" (bitfinfo bitget_type) loc
    end

(* We trap to use case assignements and read access *)

let bit_transform ns (ge:'ge) re =
  let builder:'ge AstBuilder.builder = new AstBuilder.nwbuilder ge in
    match !re with
      | AssignOp ({cont=("=",Index ({cont=(e,d::[])} as bi),v)} as b)
        ->
        begin
          match TypeAlg.typeof e with
            | TypeAlg.Int _ ->
              re := builder#call (bitput ns ge bi.loc)
                [ e; d; v]
                (TypeAlg.info (TypeAlg.Int (true,1L, TypeAlg.stdmeta ())) true true)
                b.loc;
              false; (* Continue, in order to proceed parameters *)
            | _ -> false
        end
      | Index ({cont=(e,d::[])} as b) ->
        begin
          match TypeAlg.typeof e with
            | TypeAlg.Int _ ->
              re := builder#call (bitget ns ge b.loc)
                [e; d]
                (TypeAlg.info (TypeAlg.Int (true,1L, TypeAlg.stdmeta ())) true true)
                b.loc;
              false; (* Continue, in order to proceed parameters *)
            | _ -> false
        end
      | _ -> false

let rewrite (ns,ge) ast =
  let prefix =
    {
      AstRewriter.dont with
        AstRewriter.expr = Some bit_transform;
    }
  in
    AstRewriter.global ns ge prefix AstRewriter.dont ast
