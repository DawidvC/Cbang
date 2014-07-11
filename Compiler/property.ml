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

(* Property management *)

open Ast

let fpf fmt = Format.fprintf fmt

let prepare_ops ns ge box ops storename =
    begin
      ge#builder#addfstparam [] (TypeAlg.Ref box#btype) "this" ops;
      Formacro.make_deref_this ns ge ops;
      ops.fname <- box#field_to_macro ge#mangler ops.fname;
    end

let build_store fmt ns ge box storename =
    begin
      fpf fmt "@,@[<h>/* property %s real type */@]" box#get_name ;
      fpf fmt "@,@[<h>typedef@;%s;@]@," (
        TypeAlg.toCType storename
          ~ns:ns ~exp:true ~mang:ge#mangler box#realtype
      );
    end

let build_property fmt gen_statement ns ge prop =
  let box = (ge#get_type_env ns)#get_property prop.pname in
  let store = box#get_store in
  let mangler = ge#mangler in
  let cname = box#get_c_name mangler in
  let vname = "cbang$property$virtual$" ^ box#get_name in
  let storename = cname in
  let retvar = "__cb_" ^ box#get_name ^ "_return" in
    begin
      prepare_ops ns ge box prop.getter storename;
      prepare_ops ns ge box prop.setter storename;
      fpf fmt "@,@[<h>/* ** Property: %s ** */@]@," box#get_name;

      fpf fmt "@,@[<h>typedef@;%s;@]"
        (
          TypeAlg.toCType
            vname ~ns:ns
            ~exp:true
            ~mang:mangler store
        );

      build_store fmt ns ge box storename;
      Formacro.gen_macro_fundecl gen_statement
        fmt ns ge retvar prop.getter;
      Formacro.gen_macro_fundecl gen_statement
        fmt ns ge retvar prop.setter;
      fpf fmt "@[<v>";
      fpf fmt "@,@[<h>/* ** End Property: %s ** */@]@," box#get_name;
    end
