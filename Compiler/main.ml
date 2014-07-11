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

(* Main: global driver *)

let ppon = ref false    (* Activate Pretty-Print*)
let cbi = ref false     (* Activate inetrface output *)
let tcon = ref true     (* Do typing/code gen *)
let dump = ref false    (* post-process AST dump *)
let debugLvl = ref 0    (* Debug level *)
let target = ref None   (* main target *)
let honly = ref false   (* gen only header *)
let gall = ref false    (* gen both files *)
let nomang = ref false  (* building without mangling *)

let stdname = ref Conf.stdname (* std module name *)
let stdpath = ref Conf.stdpath (* path to std *)
let nostd   = ref false       (* do not load std *)
let loadstd = ref true        (* load std *)

(* Output file name prefix *)
let output_c_prefix = ref ""
let output_h_prefix = ref ""

let (set_debug_level,set_debug_file,debug) =
  let lvl = ref 0 in
  let file = ref None in
  let deb = ref None in
    (
      (fun n -> lvl := n),
      (fun f -> file := Some f),
      (fun () ->
         match !deb with
           | None ->
               let d = Debug_printer.factory ?file:!file !lvl in
                 deb := Some d; d
           | Some d -> d
      )
    )

let conf_info () =
  begin
    Conf.info Format.err_formatter;
    exit 0;
  end

let args = Arg.align
  [
    ("-cbi", Arg.Set cbi, " pretty-print interface for module.");
    ("-header", Arg.Set honly,
     " generate only header (.h) file (default for .cbi file.)");
    ("-all", Arg.Set gall,
     " generate header and code file (default.)");
    ("-pp", Arg.Set ppon, " pretty-print cbang code");
    ("-no-type", Arg.Clear tcon, " no type checking, implies -pp");
    ("-I", Arg.String ImportPath.add_to_include_path,
     " add a path to the include path");
    ("-c-prefix", Arg.Set_string output_c_prefix,
     " prefix for the C generated file");
    ("-h-prefix", Arg.Set_string output_h_prefix,
     " prefix for the C generated header");
    ("-stdname", Arg.Set_string stdname,
     " name for the standard module");
    ("-stdpath", Arg.Set_string stdpath,
     " path to the standard module");
    ("-nostd", Arg.Set nostd,
     " do not load the standard module (default false)");
    ("-std", Arg.Set loadstd,
     " load the standard module (default true)");
    ("-no-mangling", Arg.Set nomang,
     " standalone C! file where no symbol are mangled");
    ("-dump-transform", Arg.Set dump,
     " DEBUG: re-dump AST after transofmation");
    ("-debug", Arg.Int set_debug_level,
     " set level of debug messages (default: 0 no messages, max 4)");
    ("-debug-file", Arg.String set_debug_file,
     " print debug message to file (default to stderr)");
    ("-std-conf", Arg.Unit conf_info,
     " print general information");
  ]

let usage =
  "cbc -pp <file.cb>
Pretty-print C! code (in C! syntax.)

cbc [options] <file.cb>
Produce C file (with header) from <file.cb>."

let buildContext debug ns file =
  let dostd =
    if !nostd then false
    else
      ( !loadstd
        || (!stdname != Conf.stdname)
        || (!stdpath != Conf.stdpath) )
  in
  let ge = new GlobalEnv.env debug in
  let builder = new AstBuilder.rwbuilder ge in
    ignore (new Operator.operator ge);
    ge#register_builder builder;
    ge#register_mangler
      (Mangler.factory ~suffix:(if !nomang then "nomang" else "") ());
    if dostd then
      ge#register_pervasive (Pervasive_handling.make !stdpath !stdname);
    Sempass.register_namespace ns file ge;
    (ns,ge)

let prettyPrint ?(cbi=false) ast =
  Format.printf "@[<v>";
  Pp.prettyPrint ~cbi ast;
  Format.printf "@]@."

let semanticPass (ns,ge) ast =
  Sempass.twopasscheck ns ge ast

let gencode ?(only_head=false) (ns,ge) file ast =
  Genpass.all ~only_head ns ge
    !output_c_prefix !output_h_prefix file ast

let dump_ast_post ast =
  begin
    Format.eprintf
      "@,@[<h>/**** Post Transformation Output: ****/@]@.";
    Format.eprintf "@[<v>";
    prettyPrint ast;
    Format.eprintf "@]@.";
  end

let copytexpr t = Astcopy.texpr t

let notdone = ref true

let is_cbi file =
  Filename.check_suffix file ".cbi"

let pipeline (debug : Debug_printer.printer) file =
  begin
    tcon := !tcon || !dump;
    ppon := !ppon || not !tcon;
    honly := !honly || ((is_cbi file) && not (!gall));
    if is_cbi file then
      debug#print 4 "file %S is an interface file." file;
    try
      let ast = ref (Sempass.lexAndParse file)
      and ns = Sempass.namespace file in
      let asti =
        if !cbi then
          Astcopy.source !ast
        else Obj.magic None
      in
        if !ppon then
          prettyPrint !ast;
        if !tcon then
          begin
            let context = buildContext debug ns file in
              Format.eprintf "@[<v>";
              semanticPass context !ast;
              Format.eprintf "@]@?";
              Field_access.do_it context ast;
              Converter.rewrite context ast;
              Mask.add_integer_mask context ast;
              Bitarray.rewrite context ast;
              Callrewrite.do_it context ast;
              Formacro.macro_call context ast;
              if !cbi then
                prettyPrint ~cbi:true asti
              else
                begin
                  if !dump then
                    dump_ast_post !ast;
                  gencode ~only_head:!honly context ns !ast;
                end;
          end;
    with
      | e -> ErrorHandling.errorpp debug file e; exit 2
  end

let annon file =
  match !target with
    | None -> target := Some file
    | _ ->
        begin
          Format.eprintf "@[<v>Don't know what to do with %s@," file;
          Format.eprintf "cbc work only with one file at a time.@]@.";
        end

let main () =
  begin
    Arg.parse args annon usage;
    ImportPath.add_to_include_path (Conf.prefix ^ "/lib/cbang");
    begin
      match !target with
        | Some f
          -> pipeline (debug ()) f
        | None
          ->
          begin
            Arg.usage args usage;
            exit 1;
          end
    end;
    exit 0;
  end

let _ =
  try
    Printexc.record_backtrace true;
    main ()
  with
    | e ->
      Printf.eprintf "Obscure Internal ERROR (too bad) !\n%!";
      Printf.eprintf "%s\n%!" (Printexc.to_string e);
      Printexc.print_backtrace stderr;
      exit 42
