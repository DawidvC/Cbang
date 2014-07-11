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

(* Build_interface: interface code gen *)

open Ast

let fpf = Format.fprintf

let hasher ns iname =
  Int64.of_int (Hashtbl.hash (ns^iname))

let build_fake_interface_struct fmt ns ge box =
  let sname = box#get_struct_name ge#mangler in
  let tname = (box#get_vtbl_sname ge#mangler) ^ "_t" in
    begin
      fpf fmt "@,@[<v>@[<h>%s@]@,{@[<v 1>" sname;
      fpf fmt "@,@[<h>%s %s;@]" tname ge#mangler#ivtable_field;
      fpf fmt "@,@[<h>%s@;real_this;@]" TypeAlg.objbase_name;
      fpf fmt "@]@,};@]@,";
    end

let define_tag fmt ns ge b =
  fpf fmt "@,@[<h>#define %s %s@]"
    (b#semid ge#mangler) (Int64.to_string (hasher ns b#get_name))

let build_interface_vtbl_td fmt ns ge box =
  let svtbl = "struct " ^ (box#get_vtbl_sname ge#mangler) in
  let tname = (box#get_vtbl_sname ge#mangler) ^ "_t" in
    begin
      fpf fmt "@,@[<h>typedef %s@;*%s;@]@," svtbl tname;
    end

let build ?(deconly=false) ?(forw=false) fmt ns ge bd =
  let box = (ge#get_type_env ns)#get_interface bd.bname.cont in
    begin
      fpf fmt "@,@[<v>@[<h>/*** interface %s ***/@]" box#get_name;
      if forw then
        Build_class.build_td fmt ns ge box
      else
        begin
          if deconly then
            begin
              define_tag fmt ns ge box;
              build_interface_vtbl_td fmt ns ge box;
              build_fake_interface_struct fmt ns ge box;
              Build_class.gen_vtable_struct fmt ns ge box;
            end
        end;
      fpf fmt "@,@[<h>/*** end interface %s ***/@]@]@,"
        box#get_name;
    end
