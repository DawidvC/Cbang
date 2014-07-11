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

(* Rewriter for macro *)

open Ast

let _label = "_label"

let fpf fmt = Format.fprintf fmt

(*
 * Change print mode so \n will be escaped.
 * Use it in two times: first instantiate the function then activate
 * the mode by calling the instiated fun and passing true/false.
 *)
let macro_print_mode fmt =
  let (out,flush,outnewline,outspace) =
    Format.pp_get_all_formatter_output_functions fmt ()
  in  function
    | true  ->
        begin
          Format.pp_print_flush fmt ();
          Format.pp_set_all_formatter_output_functions fmt out flush
            (fun () -> out "\\\n" 0 2) outspace;
        end
    | false ->
        begin
          Format.pp_print_flush fmt ();
          Format.pp_set_all_formatter_output_functions fmt out flush
            outnewline outspace;
	end

(* Code transform inside Macro *)

let buildretvar retvar ns (ge:'ge) t loc =
  let builder:'ge AstBuilder.builder = ge#builder in
    builder#id
      retvar
      (TypeAlg.info t false true)
      loc

let buildgoto retvar (ns:string) (ge:'ge) (loc:location) =
  let builder:'ge AstBuilder.builder = ge#builder in
    builder#goto
      (retvar^_label)
      loc

(* Transform return into variable affect/goto *)
let return retvar (ns:string) (ge:'ge) rs =
  match !rs with
    | Return ({cont=Some e;info=Some info} as b) ->
      let builder:'ge AstBuilder.builder = ge#builder in
      let t = TypeAlg.typeof e in
      let var = buildretvar retvar ns ge t b.loc in
        rs :=
          builder#block
          [
            builder#expr (
              builder#assignop "=" var e
                (TypeAlg.info t true true)
                b.loc
            ) (TypeAlg.info t true true) b.loc;
            buildgoto retvar ns ge b.loc;
          ] info b.loc;
        true
    | Return b ->
      rs := buildgoto retvar ns ge b.loc;
      true
    | _ -> false

(* End of macro, labels and variable outpout *)
let buildend retvar t ns (ge:'ge) loc =
  let builder:'ge AstBuilder.builder = ge#builder in
  let var = buildretvar retvar ns ge t loc in
    (builder#goto (retvar ^ _label) loc) ::
    (builder#label (retvar ^ _label) loc) ::
      (
        if t <> TypeAlg.Void then
          [builder#expr var (TypeAlg.info t true true) loc]
        else [builder#emptystatement loc]
      )

(* Add variable decls so param are eval once *)
let eval_param ns (ge:'ge) p t loc =
  let builder:'ge AstBuilder.builder = ge#builder in
  let _p =
    builder#id ("_"^p) (TypeAlg.info t false true) loc
  in
  let eval =
    match t with
      | TypeAlg.Ref t' ->
          builder#preop "&" _p
            (TypeAlg.info
               (TypeAlg.Pointer (t', TypeAlg.stdmeta ()))
               true true)
            loc
      | _ -> _p
  in
    begin
      builder#stmvdecl p t
        ~vinit:eval loc;
    end

(* add end statement and initial variables decl *)
let fbody retvar ns (ge:'ge) fd =
  let builder:'ge AstBuilder.builder = ge#builder in
  let t = TypeAlg.typeof_texpr fd.frtype in
    begin
      fd.fbody <-
        fd.fbody @ (buildend retvar t ns ge fd.floc);
      if t <> TypeAlg.Void then
        fd.fbody <- (builder#stmvdecl retvar t fd.floc) :: fd.fbody;
      fd.fbody <-
        List.fold_left (
          fun b (_,te,p) ->
            let t = TypeAlg.typeof_texpr te in
              (eval_param ns ge p t fd.floc) :: b
        ) fd.fbody fd.fparams;
      fd.fparams <-
        List.map (
          fun (ml,t,p) -> (ml,t,"_"^p)
        ) fd.fparams;
      true;
    end

(* Prepare a function to become a macro *)
let make_macro retvar ns ge fd =
  let prefix =
    {
      AstRewriter.dont with
        AstRewriter.statement = Some (return retvar);
    }
  in
  let postfix =
    {
      AstRewriter.dont with
        AstRewriter.fundecl = Some (fbody retvar);
    }
  in
    AstRewriter.fundecl ns ge prefix postfix fd

(* change this to *this *)
let _make_deref_this ns (ge:'ge) re =
  let debug : Debug_printer.printer = ge#debug in
  let builder:'ge AstBuilder.builder = ge#builder in
    match !re with
      | Id ({cont=(_,"this");info=Some info; loc=loc} as b) ->
          debug#locprint 3 loc ""
            ("@[<b 2>Rewriting this to *this,"
             ^^" original type: %t@]") (fun f -> TypeAlg.pp f info.TypeAlg.talg);
          re :=
            builder#preop "*" !re
              (TypeAlg.info (info.TypeAlg.talg) false true)
              b.loc;
          true
      | _ -> false

let make_deref_this ns ge fd =
  let prefix =
    {
      AstRewriter.dont with
        AstRewriter.expr = Some _make_deref_this;
    }
  in
    AstRewriter.fundecl ns ge prefix AstRewriter.dont fd

(* Print out a function as a macro *)
let gen_macro_fundecl gen_statement fmt ns ge retvar fd =
  let macro_mode = macro_print_mode fmt in
  let name =
    try
        ((ge#get_sym_env ns)#get fd.fname)#get_c_name
    with
      | Symbol.No_such_symbol _ -> fd.fname
  in
    begin
      (* Prepare code *)
      make_macro retvar ns ge fd;
      fpf fmt "@]@.";
      (* switch in "macro" mode *)
      macro_mode true;
      (* the #define header *)
      fpf fmt "@[<v 2>@[<h>#define %s(%s" name (retvar^_label);
      (* Params ... should have been prepared *)
      List.iter (
        fun (_,_,p) -> fpf fmt ",@;%s" p
      ) fd.fparams;
      fpf fmt ")@]@,({@[<v>";
      List.iter (
        gen_statement fmt ge ns
      ) fd.fbody;
      fpf fmt "@]@,})@]";
      (* Go back in normal mode *)
      macro_mode false;
      fpf fmt "@.@[<v>";
    end

(* Now, transform macro-call so it include label for goto *)
let build_call_point =
  let id = ref (-1) in
    fun loc ->
      let fstl = string_of_int (Asttools.getFstLine loc) in
      let nid = incr id; string_of_int (!id) in
        begin
          "_macro$"^fstl^"$"^nid^_label
        end

let transform_call ns (ge:'ge) re =
  let builder:'ge AstBuilder.builder = ge#builder in
    match !re with
      | Call (
        {
          cont=(e,el);
          info= Some {TypeAlg.talg = TypeAlg.MacroCall t}
        }
          as b
      )
        ->
        let cp =
          builder#id (build_call_point b.loc)
            (TypeAlg.info t true true) b.loc
        in
          b.cont <- (e,cp::el);
          true
      | _ -> false

let macro_call (ns,ge) ast =
  let postfix =
    {
      AstRewriter.dont with
        AstRewriter.expr = Some transform_call;
    }
  in
    AstRewriter.global ns ge AstRewriter.dont postfix ast
