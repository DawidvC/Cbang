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

(* Manage generics types for mapper *)

class ['mang,'box,'cbox,'ge] gt_env (ns:string) (g:'ge) =
object (self)

  constraint 'checker = (
    ?deconly:bool -> ?field:bool -> ?macro:bool -> ?ctx:string
    -> string -> 'ge -> TypeAlg.info Ast.fundecl
    -> TypeAlg.type_alg
  )

  val ge = Oo.copy g
  val mutable checker = (fun _ -> TypeAlg.Error)

  method virtual add:
      string -> TypeAlg.type_alg -> Ast.location -> unit

  method add x t loc =
    (ge#get_type_env ns)#pre_register_type false x loc

  method register_checker ctx (f:'checker) =
    checker <-
      f ~deconly:false ~field:true ~macro:true ~ctx:ctx ns ge

  method check fd = checker fd

  initializer
    ge#register_type_env ns (ge#get_type_env ns)#clone

end

let make ns ge ctx checker =
  let gt = new gt_env ns ge in
    gt#register_checker ctx checker;
    gt
