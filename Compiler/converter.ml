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

(* Inject type conversion in expression *)

open Ast

(* Most cases are just cast, but box can provide more adapted translation *)

let need e =
  match Asttools.expr_get_info e with
    | { TypeAlg.need_conv = None} -> false
    | _ -> true

let build_cast ns (ge:'ge) e t =
  let builder:'ge AstBuilder.builder = ge#builder in
    builder#cast t e (Asttools.expr_get_loc e)

let targetbox ns ge target =
  try
    Some ((ge#get_type_env ns)#get_box_from_type target)
  with
    | Types.Not_boxed -> None

let choose_injecter ns ge builder e target = function
  | TypeAlg.TypeName (n,_)
  | TypeAlg.NSTypeName (_,n,_)
      when n = TypeAlg.objbase_name
      ->
    begin
      try
        ((ge#get_type_env ns)#get_box_from_type target)#make_real builder e
      with
        | Types.Not_boxed
        | Types.No_such_box _
          -> assert false
    end
  | TypeAlg.TypeName (n,_)
    ->
    begin
      try
        (ge#debug : Debug_printer.printer)#print 1
          "@;conv Typename (%s)@;" n;
        ((ge#get_type_env ns)#get_box n)#converter
          ge#mangler builder e target
          (targetbox ns ge target)
      with
        | Types.No_such_box _ ->
          build_cast ns ge e target
    end
  | TypeAlg.NSTypeName (ns',n,_)
    ->
    begin
      try
        (ge#debug : Debug_printer.printer)#print 1
          "@;conv NSTypename (%s,%s)@;" ns' n;
        ((ge#get_type_env ns')#get_box n)#converter
          ge#mangler builder e target
          (targetbox ns ge target)
      with
        | Types.No_such_box _ ->
          build_cast ns ge e target
    end
  | t ->
    try
      ((ge#get_type_env ns)#get_box_from_type t)#converter
        ge#mangler builder e target
        (targetbox ns ge target)
    with | _ -> build_cast ns ge e target

let choose_assign ns ge builder e left target right torig =
  try
    ((ge#get_type_env ns)#get_box_from_type torig)#assign_converter
      ge#mangler builder e left target right
  with
    | Types.Not_boxed | Types.No_such_box _ ->
      let (op,b) = match e with
        | AssignOp ({cont = (op,_,_)} as b) -> (op,b)
        | _ -> assert false
      in
        b.cont <- (op,build_cast ns ge left target, right);
        e

let mark_done e =
  (Asttools.expr_get_info e).TypeAlg.need_conv <- None

let need_revconv ns ge t =
  try
    ((ge#get_type_env ns)#get_box_from_type t)#need_revconv
  with
    | _ -> false

let inject ns (ge:'ge) re =
  let builder:'ge AstBuilder.builder = ge#builder in
    match !re with
      | AssignOp {cont=("=",left,right)} when need left ->
          let (t0,t1) = match Asttools.expr_get_info left with
            | { TypeAlg.need_conv = Some t } -> t
            | _ -> assert false
          in
            re := choose_assign ns ge builder !re left t1 right t0;
            mark_done left;
            false
      | e when need e ->
          let (t0,t1) = match Asttools.expr_get_info e with
            | { TypeAlg.need_conv = Some t } -> t
            | _ -> assert false
          in
            if need_revconv ns ge t1 then
              begin
                (ge#debug : Debug_printer.printer)#print 1 "@;revconv@;";
                re := choose_injecter ns ge builder e t0 t1
              end
            else
              re := choose_injecter ns ge builder e t1 t0;
            mark_done e;
            false
      | _ -> false

let rewrite (ns,ge) ast =
  let prefix =
    {
      AstRewriter.dont with
        AstRewriter.expr = Some inject;
    }
  in
    AstRewriter.global ns ge prefix AstRewriter.dont ast
