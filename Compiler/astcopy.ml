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

(* Backup AST for interface production *)

(* AST backup only deal with external part (no expr, no stm) *)

open Ast

let dup b = { b with info = None }

let rec texpr = function
  | Void b -> Void (dup b)
  | TName b ->
    TName (dup b)
  | Num b ->
    Num (dup b)
  | TFloat b ->
    TFloat (dup b)
  | TChar b ->
    TChar (dup b)
  | TPointer b ->
    TPointer {b with cont = texpr b.cont; info=None }
  | Array b ->
    let (t,dl) = b.cont in
      Array { b with cont=(texpr t,dl); info=None }
  | Fun b ->
    let cont =
      let (tin,tout) = b.cont in
        (List.map texpr tin, texpr tout)
    in
      Fun { b with cont=cont; info=None}
  | TModifier b ->
      let (m,t) = b.cont in
        TModifier { b with cont = (m, texpr t); info=None }

let vdecl vd =
  {
    vd with
      vinfo = None;
      vinit = None;
      vtype = texpr vd.vtype;
  }

let fundecl fd =
  {
    fd with
      finfo = None;
      fbody = [];
      fexport = fd.fexport;
      fparams =
      List.map (fun (sl,t,x) -> (sl, texpr t, x)) fd.fparams;
      frtype = texpr fd.frtype;
  }

let member = function
  | Method f -> Method (fundecl f)
  | SField v -> SField (vdecl v)
  | x -> x

let innerboxdecl ib =
  {
    ib with
      members = List.map member ib.members;
      constructor = (match ib.constructor with
        | None -> None
        | Some c -> Some (fundecl c));
      destructor = (match ib.destructor with
        | None -> None
        | Some c -> Some (fundecl c));
  }

let boxdecl = function
  | Struct b ->
    Struct {
      b with
        info=None;
        cont = (let (p,ib) = b.cont in (p, innerboxdecl ib));
    }
  | Union b ->
    Union {
      b with
        cont = innerboxdecl b.cont;
        info=None;
    }
  | Enum b ->
    Enum {
      b with
        cont = innerboxdecl b.cont;
        info=None;
    }
  | Class b ->
    Class {
      b with
        cont = innerboxdecl b.cont;
        info=None;
    }
  | Interface b ->
    Interface {
      b with
        cont = innerboxdecl b.cont;
        info=None;
    }

let typedef td =
  {
    td with
      tdtype = texpr td.tdtype;
      tdinfo = None;
  }

let gdecl = function
  | FDecl f -> FDecl (fundecl f)
  | TDef td -> TDef (typedef td)
  | BDecl b -> BDecl (boxdecl b)
  | VDecl v -> VDecl (vdecl v)
  | Map (v,x) -> Map (vdecl v, x)
  | x -> x

let source src =
  {
    src with
      srcgdecls = List.map gdecl src.srcgdecls;
  }
