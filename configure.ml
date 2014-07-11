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

(* Configure script *)

let resume = ref true

type conf = {
  mutable prefix : string;
  mutable cc : string;
  mutable stdpath : string;
  mutable cflags : string;
  mutable ldflags : string;
}

let stdconf = {
  prefix = "/usr/local";
  cc = "clang";
  stdpath = "";
  cflags = " -Wall -Wextra -std=gnu99 -O2 -D_GNU_SOURCE";
  ldflags = "";
}

let set_prefix p = stdconf.prefix <- p
let set_cc c = stdconf.cc <- c
let set_stdpath p = stdconf.stdpath <- p
let set_cflags f = stdconf.cflags <- f
let set_ldflags f = stdconf.ldflags <- f

let spec = Arg.align [
  ("-prefix", Arg.String set_prefix,
   " fix the installation prefix (default:" ^ stdconf.prefix ^ ")");
  ("-cc", Arg.String set_cc,
   " set the C-compiler (default: " ^ stdconf.cc ^ ")");
  ("-stdpath", Arg.String set_stdpath,
   " set the path for std module (default: <prefix>/lib/cbang)");
  ("-cflags", Arg.String set_cflags,
   " set CFLAGS (default: " ^ stdconf.cflags ^ ")");
  ("-ldflags", Arg.String set_ldflags,
   " set LDFLAGS (default: " ^ stdconf.ldflags ^ ")");
  ("-q", Arg.Clear resume, " quiet mode");
]

let usage =
  Sys.argv.(0) ^ " [options]\nset various build and installation options:"

let annon s =
  begin
    Format.eprintf "@[<b 2>Don't know what to do with %S@]@." s;
    Arg.usage spec usage;
    exit 3
  end

let conf_line fmt var value =
  Format.fprintf fmt "@,@[<h>%s=%s@]" var value

let out_conf fmt =
  begin
    Format.fprintf fmt "@[<v>@[<h># Autogen configurations@]";
    conf_line fmt "PREFIX" stdconf.prefix;
    conf_line fmt "CC" stdconf.cc;
    conf_line fmt "STDPATH" stdconf.stdpath;
    conf_line fmt "CFLAGS" stdconf.cflags;
    conf_line fmt "LDFLAGS" stdconf.ldflags;
    Format.fprintf fmt "@,@,@[<h># configuration for make check@]";
    conf_line fmt "CBCFLAGS" (" -stdpath " ^ (Sys.getcwd ()) ^ "/stdlib");
    conf_line fmt "CBINCLUDES" (" -I " ^ (Sys.getcwd ()) ^ "/stdlib");
    Format.fprintf fmt "@,@,@[<h># End of configurations@]@]@.";
  end

let header_build_template cout =
  let cin = open_in "Tools/Builds/cbang.mk-template" in
  let b = Buffer.create 1024 in
  begin
    Buffer.add_channel b cin (in_channel_length cin);
    Buffer.output_buffer cout b;
    close_in cin;
  end

let build_template_make () =
  let cout = open_out "Tools/Builds/cbang.mk" in
  let _ = header_build_template cout in
  let fmt = Format.formatter_of_out_channel cout in
  begin
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "@[<h># Variables@]@,";
    conf_line fmt "CBHEADER" (stdconf.prefix ^ "/lib/cbang");
    conf_line fmt "CC" stdconf.cc;
    conf_line fmt "CFLAGS" (stdconf.cflags ^ " -I${CBHEADER}");
    conf_line fmt "LDFLAGS" stdconf.ldflags;
    Format.fprintf fmt "@,@,@[<h># Rules to produce C files@]";
    Format.fprintf fmt "@,@[<h>.SUFFIXES: .cb .cbi@]";
    Format.fprintf fmt "@,@,@[<v>.cb.c:@,\tcbc $<@]";
    Format.fprintf fmt "@,@,@[<v>.cbi.h:@,\tcbc $<@]";
    Format.fprintf fmt "@,@,@[<h># END@]@,@]@.";
    close_out cout;
  end

let print_resume () =
  if !resume then
    begin
      let fmt = Format.err_formatter in
      Format.fprintf fmt "@[<v 2>@[<h>Build configuration summary:@]";
      conf_line fmt "PREFIX" stdconf.prefix;
      conf_line fmt "CC" stdconf.cc;
      conf_line fmt "STDPATH" stdconf.stdpath;
      conf_line fmt "CFLAGS" stdconf.cflags;
      conf_line fmt "LDFLAGS" stdconf.ldflags;
      Format.fprintf fmt "@]@.";
    end

let generate_conf_module () =
  let cin = open_in "Compiler/conf.ml-template" in
  let cout = open_out "Compiler/conf.ml" in
  let buf = Buffer.create 4096 in
  let tmp = Buffer.create 4096 in
  let substitute =
    let h = Hashtbl.create 13 in
    let _ = List.iter (fun (k,v) -> Hashtbl.add h k v) [
      ("PREFIX", stdconf.prefix);
      ("STDPATH", stdconf.stdpath);
      ("VERSION", "0.1-alpha");
    ] in
    fun s -> Hashtbl.find h s
  in
  begin
    Buffer.add_channel tmp cin (in_channel_length cin);
    Buffer.add_substitute buf substitute (Buffer.contents tmp);
    Buffer.output_buffer cout buf;
    close_in cin; close_out cout;
  end

let main () =
  let cout = (open_out "config.mk") in
  let fmt = Format.formatter_of_out_channel cout in
    begin
      Arg.parse spec annon usage;
      if stdconf.stdpath = "" then
        stdconf.stdpath <- stdconf.prefix ^ "/lib/cbang";
      out_conf fmt; close_out cout;
      build_template_make ();
      generate_conf_module ();
      print_resume ();
      exit 0;
    end

let _ = main ()
