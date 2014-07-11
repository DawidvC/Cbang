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

(* Semantic analysis: first pass *)

open Ast

exception Bad_type of TypeAlg.type_alg * location
exception ModuleError of string * exn * location
exception Not_found_module of string * location

let lexAndParse file =
  let lexbuf = Lexing.from_channel (open_in file) in
  lexbuf.Lexing.lex_start_p  <- {
    lexbuf.Lexing.lex_start_p with
      Lexing.pos_fname = file;
  };
  lexbuf.Lexing.lex_curr_p  <- {
    lexbuf.Lexing.lex_curr_p with
      Lexing.pos_fname = file;
  };
  Parser.main Lexer.token lexbuf

let namespace filename =
  let basename = Filename.basename filename in
  try
    Filename.chop_extension basename
  with
    | Invalid_argument _ -> basename

let register_namespace ns file ge =
  let se = new Symbol.env ns ge#mangler in
  let te = new Types.env ns ge in
  let debug : Debug_printer.printer = ge#debug in
  begin
    ge#register_file_name ns file;
    ge#register_type_env ns te;
    ge#register_sym_env ns se;
    debug#print 1 "Namespace %S registered" ns;
  end

let rec check_import ns ge = function
  | (Import {cont=name; loc=l} | Open {cont=name; loc=l}) as kind
    ->
    begin
      try
        if not (ge#namespacePresent name) then
          begin
            let filename = ImportPath.find name in
            let namespace = namespace filename in
            let ast = lexAndParse filename in
            register_namespace namespace filename ge;
            ge#debug#print 1 "found : %s (module %s)" filename namespace;
            ge#open_pervasive namespace;
            pass0 namespace ge ast;
            pass1 namespace ge ast;
          end;
        begin
          match kind with
            | Open _ -> ge#open_in name ns
            | _ -> ()
        end
      with
        | ImportPath.Not_found_in_path m
          -> raise (Not_found_module (m,l))
        | e -> raise (ModuleError (name, e, l))
    end
  | _ -> () (* Nothing todo with include *)

and precheck ns ge =
  let te = ge#get_type_env ns in function
    | TDef td
      ->
      let _ = (
        match td.tdnspace with
          | []
            -> td.tdnspace <- [ns]; true
          | namespace::[]
              when namespace = ns
              -> true
          | namespace::_
            -> raise (Typing.Bad_namespace (Some namespace, td.tdloc))
      ) in
      te#pre_register_type true td.tdname td.tdloc
    | BDecl bd
      ->
      te#pre_register_type true
        (Asttools.get_innerbox_name (Asttools.get_inner_box bd))
        (Asttools.get_box_loc bd)
    | Proper { pname = name; properloc = l}
    | MCDecl { cmacro = { cont = name }; mcloc = l }
      -> te#pre_register_type true name l
    | _ -> ()

and check  ?(deconly=false) ?(static=false) ns ge = function
  | TDef td
    ->
    if deconly then
      begin
        let _ = (
          match td.tdnspace with
            | [] -> td.tdnspace <- [ns]; true
            | namespace::[] when namespace = ns -> true
            | namespace::_ ->
              raise (Typing.Bad_namespace (Some namespace,
                                           td.tdloc))
        ) in
        let te = ge#get_type_env ns in
        let debug : Debug_printer.printer = ge#debug in
        let _ =
          debug#locprint 4 td.tdloc ns
            "typedef %s post register" td.tdname
        in
        let t = Typing.typedef ~deconly ns ge td in
        te#post_register_type td.tdname t
      end
  | BDecl bd
    -> Typing.bdecl ~deconly ns ge bd
  | VDecl vd
    ->
    let debug : Debug_printer.printer = ge#debug in
    let t = Typing.vdecl ~deconly ~global:true ns ge vd in
    let cname = ((ge#get_sym_env ns)#get vd.vname)#get_c_name in
    if deconly && static && TypeAlg.is_num t then
      begin
        debug#locprint 3 vd.vloc ns
          "%s (%s) is a numeric constant" vd.vname (cname);
        (Macro_set.get ())#add cname vd.vname
      end
  | FDecl fd
    -> ignore (Typing.fdecl ~deconly ~macro:static ns ge fd)
  | MCDecl mc
    -> Typing.mclassdecl ~deconly ns ge mc
  | Macro m
    -> check ~deconly ~static:true ns ge m
  | Map (vd,x)
    -> ignore (Typing.vdecl ~asname:x ~deconly ~global:true ns ge vd)
  | Proper p
    -> Typing.property ~deconly ns ge p

and open_pervasive ns ge srcloc =
  try
    let std = ge#get_pervasive in
    let stdname = std#get_module_name in
    let stdpath = std#get_full_filename in
    try
      let ast = lexAndParse stdpath  in
      register_namespace
        stdname stdpath ge;
      ge#debug#print 1 "adding std/pervasive module (%s) from %s"
        stdname stdpath;
      pass0 stdname ge ast;
      pass1 stdname ge ast;
      ge#open_in stdname ns;
    with
      | e -> raise (ModuleError (stdname, e, srcloc))
  with
    | Not_found -> ()

and pass0 ns ge ast =
  List.iter (check_import ns ge) ast.srcmodparam.modimports;
  List.iter (precheck ns ge) ast.srcgdecls

and pass1 ns ge ast =
  List.iter (check ~deconly:true ns ge) ast.srcgdecls

let pass2 ns ge ast =
  List.iter (check ~deconly:false ns ge) ast.srcgdecls

let twopasscheck ns ge ast =
  let debug : Debug_printer.printer = ge#debug in
    begin
      open_pervasive ns ge ast.srcloc;
      debug#print 3 "Starting pass 0";
      pass0 ns ge ast;
      debug#print 3 "Starting pass 1";
      pass1 ns ge ast;
      (ge#get_type_env ns)#post_typing;
      debug#print 3 "Starting pass 2";
      pass2 ns ge ast;
      ge#subtyper#dump_out;
    end
