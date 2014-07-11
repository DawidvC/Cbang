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

(* Ast functions *)

open Ast

exception SyntaxError of location
exception DEBUG_no_info of location
exception Namespace_not_found of string

let expr_get_loc = function
  | Value {loc=l}
  | Id {loc=l}
  | BinOp {loc=l}
  | PreOp {loc=l}
  | PostOp {loc=l}
  | AssignOp {loc=l}
  | TerOp {loc=l}
  | Call {loc=l}
  | Index {loc=l}
  | Field {loc=l}
  | PField {loc=l}
  | Cast {loc=l}
  | Sizeof {loc=l}
  | Decl {loc=l}
  | Compound {loc=l}
  | SelfInit {loc=l} -> l

let expr_get_info = function
  | Value {info= Some l}
  | Id {info= Some l}
  | BinOp {info= Some l}
  | PreOp {info= Some l}
  | PostOp {info= Some l}
  | AssignOp {info= Some l}
  | TerOp {info= Some l}
  | Call {info= Some l}
  | Index {info= Some l}
  | Field {info= Some l}
  | PField {info= Some l}
  | Cast {info= Some l}
  | Sizeof {info= Some l}
  | Decl {info= Some l}
  | Compound {info = Some l}
  | SelfInit {info = Some l} -> l
  | e -> raise (DEBUG_no_info (expr_get_loc e))

let swtest_get_info = function
  | Default {info=Some i}
  | SwValue {info=Some i} -> i
  | _ -> assert false

let texpr_get_info = function
  | Void {info= Some i}   | TName {info= Some i} | Num {info= Some i}
  | TFloat {info= Some i} | TChar {info= Some i} | TPointer {info= Some i}
  | Array {info= Some i}  | Fun {info= Some i} | TModifier {info= Some i}
      -> i
  | _ -> assert false

let texpr_get_loc = function
  | Void {loc=loc}   | TName {loc=loc} | Num {loc=loc}
  | TFloat {loc=loc} | TChar {loc=loc} | TPointer {loc=loc}
  | Array {loc=loc}  | Fun {loc=loc} | TModifier {loc=loc}
      -> loc


let stm_get_loc = function
  | EmptyStatement{loc=l}
  | Expr{loc=l}
  | Block{loc=l}
  | If{loc=l}
  | While{loc=l}
  | Do{loc=l}
  | For{loc=l}
  | Switch {loc=l}
  | Break{loc=l}
  | Continue{loc=l}
  | Return{loc=l}
  | Label {loc=l}
  | Goto {loc=l}
  | Delete {loc=l}
  | Asm {loc=l}
    -> l

let stm_get_info = function
  | EmptyStatement{info = Some l}
  | Expr{info = Some l}
  | Block{info = Some l}
  | If{info = Some l}
  | While{info = Some l}
  | Do{info = Some l}
  | For{info = Some l}
  | Switch {info = Some l}
  | Break{info = Some l}
  | Continue{info = Some l}
  | Label {info = Some l}
  | Goto {info = Some l}
  | Return{info = Some l}
  | Delete {info = Some l}
  | Asm {info = Some l}
    -> l
  | s -> raise (DEBUG_no_info (stm_get_loc s))

let boxdecl_get_info = function
  | Struct {info = Some info}
  | Union {info = Some info}
  | Enum {info = Some info}
  | Class {info = Some info}
  | Interface {info = Some info}
    -> info
  | _ -> assert false

let get_inner_box = function
  | Struct {cont = (_, inner )}
  | Union {cont = inner }
  | Enum {cont = inner }
  | Class {cont = inner }
  | Interface {cont = inner }
    -> inner

let get_box_loc = function
  | Struct {loc = l}
  | Union {loc = l}
  | Enum {loc = l}
  | Class {loc = l}
  | Interface {loc = l}
    -> l

let get_innerbox_name = function
  | { bname = {cont = name } } -> name

let get_box_name = function
  | Struct {cont = (_,{ bname = {cont = name} })}
  | Union {cont = { bname = {cont = name} }}
  | Enum {cont = { bname = {cont = name} }}
  | Class {cont = { bname = {cont = name} }}
  | Interface {cont = { bname = {cont = name} }}
    -> name

let is_box_exported ns b =
  match b.bnspace with
    | [] -> true
    | n::[] -> ns=n
    | _ -> false

(* let is_boxdecl_exported ns = function *)
(*   | Struct {cont = (_,ib)} *)
(*   | Union {cont = ib} *)
(*   | Enum {cont = ib} *)
(*   | Class {cont = ib} *)
(*     -> is_box_exported ns ib *)

(* let is_typedef_exported ns td = *)
(*   match td.tdnspace with *)
(*     | [] -> true *)
(*     | n::_ -> ns=n *)

let is_mclass_exported ns mc =
  match mc.mcnspace with
    | [] -> true
    | n::_ -> ns=n

let member_get_loc = function
  | Method {floc=loc}
  | SField {vloc=loc}
  | EField {loc=loc} -> loc

let print_loc fmt fname (l1, l2) =
  let line1 = l1.Lexing.pos_lnum in
  let (c1,c2) = (
    l1.Lexing.pos_cnum - l1.Lexing.pos_bol,
    l2.Lexing.pos_cnum - l1.Lexing.pos_bol
  )
  in
    begin
      Format.fprintf fmt "@[<b>";
      if fname <> "" then
        Format.fprintf fmt "File %S,@;" fname;
      Format.fprintf fmt "line %d,@;" line1;
      Format.fprintf fmt "characters %d-%d@]" c1 c2
    end

let locerrp fname (l1,l2) =
  Format.eprintf "@[<h>";
  print_loc Format.err_formatter fname (l1, l2);
  Format.eprintf ":@]@;"

let value_string = function
  | VInt i -> Int64.to_string i
  | VFloat f -> string_of_float f
  | VChar c -> Str_escaping.char c
  | VCstStr s -> Str_escaping.str s
  | VTag _ -> assert false (* Should not be call *)

let rec no_compound = function
  | Id _ -> true
  | Value _ -> true
  | Field {cont=(e,_)}
  | PField {cont=(e,_)}
  | Cast {cont=(_,e)} -> no_compound e
  | Index {cont=(e,il)} ->
    no_compound e && List.for_all no_compound il
  | _ -> false

let getFstLine (l1,_) = l1.Lexing.pos_lnum

let add_vmod x vd =
  let rec aux = function
    | h::t when h < x -> h :: aux t
    | h::_ as l when h = x -> l
    | l -> x::l
  in
    vd.vmods <- aux vd.vmods

let vdecl_static vd =
  List.mem "static" vd.vmods

(* Extracting types from params *)
let extract_types fd =
  List.map (
    fun (_,t,_) -> t
  ) fd.fparams

(* clean-up namespace in interfaces *)
let clean_supports_namespace ns ib =
  ib.interf <- List.map (
    function
      | ("",i) -> (ns,i)
      | x -> x
  ) ib.interf

open Lexing
let baseloc f =
  (
    {
      pos_fname = f;
      pos_lnum = 0;
      pos_bol = 0;
      pos_cnum = 0;
    },
    {
      pos_fname = f;
      pos_lnum = 0;
      pos_bol = 0;
      pos_cnum = 0;
    }
  )
