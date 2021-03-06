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

(* expand class field access *)

open Ast

let fbuilder  = new AstBuilder.nwbuilder

let field_access ns (ge:'ge) re =
  match !re with
    | Field ({cont=(e,f);info=Some info} as b)
      ->
      begin
        match info.TypeAlg.talg with
          | TypeAlg.Fun _ -> false
          | _
            ->
            begin
              try
                let box =
                  (ge#get_type_env ns)#get_box_from_type
                    (TypeAlg.typeof e)
                in
                if not (box#is_pointer) then false
                else
                  begin
                    re := ge#builder#pfield e f info b.loc;
                    true
                  end
              with
                | _ -> false
            end
      end
    | Id ({cont=(_,x);info = Some info} as b)
      ->
      begin
        try
          let builder:'ge AstBuilder.builder = ge#builder in
          let fbuilder = fbuilder ge in
          let box = (ge#get_type_env ns)#get_current_box in
          if box#have_field x then
            begin
              if box#field_is_method x then
                begin
                  let binfo = TypeAlg.info box#btype false true in
                  let this =
                    builder#id "this" binfo b.loc
                  in
                  re:= fbuilder#field this x info b.loc;
                  true
                end
              else
                begin
                  let binfo = TypeAlg.info box#btype false true in
                  let this =
                    builder#id "this" binfo b.loc
                  in
                  re:= fbuilder#pfield this x info b.loc;
                  true
                end
            end
          else false
        with
          | _ -> false
      end
    | _ -> false

let posfix = {
  AstRewriter.dont with
    AstRewriter.expr = Some field_access;
}

let in_method ns ge fd =
  AstRewriter.fundecl ns ge AstRewriter.dont posfix fd

let do_it (ns,ge) ast =
  AstRewriter.global ns ge AstRewriter.dont posfix ast
