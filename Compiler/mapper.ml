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

(* Mapper handling *)

open Ast

(* THIS IS WORK IN PROGRESS *)

module StrSet = Set.Make(String)

let dumpos = (Lexing.dummy_pos, Lexing.dummy_pos)

let build_block data loc = {
  cont = data;
  loc = loc;
  info = None;
}

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

exception Getter_type_mismatch of location * TypeAlg.type_alg
exception Setter_type_mismatch of location * TypeAlg.type_alg

class ['mang,'builder] property_group ns ge ctx checker =
object (self)

  (* Local context *)
  val mutable propctx    = ""

  (* Handling the type variable *)
  val mutable tvar       = ""
  method set_tvar name   = tvar <- name
  method get_tvar        = tvar

  (* Property parameters *)
  val params             = Queue.create ()

  method virtual set_params :
    (TypeAlg.info Ast.texpr * string) list -> unit

  method set_params pl   =
    let syms = ge#get_sym_env ns in
      begin
        List.iter (
          fun (t,(x:string)) ->
            Queue.push (x,TypeAlg.fromTExpr t) params
        ) pl;
        syms#restore_context propctx;
        Queue.iter (
          fun (x,t) ->
            syms#add x (Symbol.factory ns x false t None)
        ) params;
        syms#save_context propctx;
      end

  (* get/set stored operations *)
  val mutable t_getter   = TypeAlg.Error
  val mutable t_setter   = TypeAlg.Error
  val mutable ast_getter = dumf
  val mutable ast_setter = dumf

  method getter fd =
    let gte = Generic_types.make ns ge propctx checker in
    let tf = TypeAlg.Fun ([], TypeAlg.fromTExpr fd.frtype,
                          TypeAlg.stdmeta ()) in
    let tc = gte#check fd in
      begin
        if tf <> tc then
          raise (Getter_type_mismatch (fd.floc,tc));
        t_getter <- tf;
        ast_getter <- fd;
      end

  method setter fd =
    let gte = Generic_types.make ns ge propctx checker in
    let tf =
      TypeAlg.Fun (
        [TypeAlg.TypeName (tvar, TypeAlg.stdmeta ())],
        TypeAlg.Void,
        TypeAlg.stdmeta ()
      )
    in
    let tc = gte#check fd in
      begin
        if tf <> tc then
          raise (Setter_type_mismatch (fd.floc,tc));
        t_setter <- tf;
        ast_setter <- fd;
      end

  initializer
    begin
      propctx <- ctx^(string_of_int (Oo.id self));
    end

end

class ['mang,'builder] mapper name store loc ns ge checker =
object (self)
  inherit ['mang,'builder]
    Box.macrobox name store loc (ge#get_type_env ns) as super

  (* Properties *)
  val properties        = Hashtbl.create 13
  val mutable propcount = 0

  method new_property () =
    let p = new property_group ns ge name checker in
      begin
        Hashtbl.add properties propcount p;
        propcount <- propcount + 1;
      end

end

