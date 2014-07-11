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

(* Managing gencode pass *)

open Ast

let fpf fmt = Format.fprintf fmt

let gen_import fmt ast =
  let aux = function
    | Import {cont=module_name}
    | Open {cont=module_name} ->
      fpf fmt "@,@[<h>#include \"%s.h\"@]" module_name
    | Include {cont=l} ->
      List.iter (
        fun i -> fpf fmt "@,@[<h>#include <%s>@]" i
      ) l
  in
    List.iter aux ast.srcmodparam.modimports

let gen_c_tdef fmt ns ge ast =
  let aux = function
    | MCDecl mc ->
      Gencode.gen_mcdecl ~deconly:false fmt ge ns mc
    | _ -> ()
  in
    List.iter aux ast.srcgdecls

let gen_forward fmt ns ge ast =
  let aux = function
    | FDecl fd ->
      if ge#need_forward ns fd.fname then
        Gencode.gen_fdecl ~forw:true ~deconly:true
          ?meth:(Some false)
          fmt ge ns fd
    | VDecl vd ->
      begin
        fpf fmt "@,@[<b 2>";
        Gencode.gen_decl ~deconly:false ~global:true fmt ge ns vd;
        fpf fmt ";@]";
      end
    | _ -> ()
  in
    List.iter aux ast.srcgdecls

let rec gen_gdecl ?(deconly=false) fmt ge ns = function
  | FDecl  fdecl ->
      Gencode.gen_fdecl ~deconly ?meth:(Some false) fmt ge ns fdecl
  | BDecl b ->
      Gencode.gen_boxdecl ~deconly fmt ge ns b
  | _ -> ()

(* Files generation *)

let front_comment fmt file =
  begin
    fpf fmt
      "@[<h 2>/* Generated code from cbang code */@]";
    fpf fmt
      "@,@[<b 2>/* from file:@;@[<h>%s.cb@] */@]@," file;
  end

let headify =
  let reg = Str.regexp "[- ]" in
    function s ->
      String.uppercase (Str.global_replace reg "_" s)

let out_hhead fmt file =
  begin
    front_comment fmt file;
    fpf fmt "@,@[<h>#ifndef _%s_H@,@]" (headify file);
    fpf fmt "@,@[<h>#define _%s_H@]@," (headify file);
    fpf fmt "@,@[<h>#include <stdint.h>@]@,";
    fpf fmt
      "@,@[<h>#define CBANG_NS(Module,Name)@;cbang$##Module##$##Name@]@,";
  end

let out_pervasive fmt ns ge =
  try
    fpf fmt "@,@[<h>#include %S@]@," ge#get_pervasive#get_include_path
  with Not_found -> ()

let out_hfoot fmt file =
  fpf fmt "@,@[<h>#endif@]@,"

let header_forward fmt ns ge ast =
  List.iter (Gencode.gen_forward_type fmt ge ns) ast

let header ns ge file prefix ast ast_full =
  let h_filename = prefix ^ file ^ ".h" in
  let fmt = Format.formatter_of_out_channel (open_out h_filename) in
    begin
      fpf fmt "@[<v>";
      out_hhead fmt file;
      out_pervasive fmt ns ge;
      Build_class.gen_class_base_type fmt ge;
      gen_import fmt ast_full;
      header_forward fmt ns ge ast;
      List.iter (Gencode.gen_gdecl ~deconly:true fmt ge ns) ast;
      out_hfoot fmt file;
      fpf fmt "@]@.";
    end

let out_chead fmt file =
  begin
    front_comment fmt file;
    fpf fmt "@,@[<v>#include <stdint.h>@,@]";
    fpf fmt "@,@[<h>#include \"%s.h\"@]@," file;
    fpf fmt "@,@[<h>#define MASK(Size,Value)@;";
    fpf fmt
      "( ((2 << ((Size) - 1)) - 1) & (Value) )@]@,";
    Bitarray.gen_macro fmt;
  end

let code ns ge file prefix ast =
  let c_filename = prefix ^ file ^ ".c" in
  let fmt = Format.formatter_of_out_channel (open_out c_filename) in
  begin
    fpf fmt "@[<v>";
    out_chead fmt file;
    gen_import fmt ast;
    gen_c_tdef fmt ns ge ast;
    gen_forward fmt ns ge ast;
    List.iter (gen_gdecl fmt ge ns) ast.srcgdecls;
    fpf fmt "@]@."
  end

let prefix_uniform = function
  | ("", x) | (x, "") -> (x,x)
  | (x,y) -> (x,y)

let all ?(only_head=false) ns ge cpref hpref file ast =
  let (cpref,hpref) = prefix_uniform (cpref,hpref) in
    begin
      header ns ge file hpref ast.srcgdecls ast;
      if not (only_head) then
        code ns ge file cpref ast;
    end

