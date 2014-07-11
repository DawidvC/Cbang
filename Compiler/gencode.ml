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

(* Code generation *)

open Ast

let builder = new AstBuilder.rwbuilder

let fpf fmt = Format.fprintf fmt

let freshID =
  let c = ref 0 in
    fun () ->
      incr c;
      "_cbtmp"^string_of_int !c

let applyop f fall = function
  | Some x -> f x
  | None -> fall

let rec isCstLeftVal = function
  | Id _ -> true
  | Field {cont=(e,_)}
  | PField {cont=(e,_)}
  | Cast {cont=(_,e)} -> isCstLeftVal e
  | Index {cont=(e,il)} -> isCstLeftVal e && List.for_all isCstLeftVal il
  | _ -> false

let gen_value ge ns = function
  | VTag tag ->
    begin
      match ge#tagenv#tag_type tag with
        | TypeAlg.TypeName (t,_) ->
          ge#mangler#enum_tag "" tag t
        | TypeAlg.NSTypeName (ns,t,_) ->
          ge#mangler#enum_tag ns tag t
        | TypeAlg.Int(true,32L,_) -> tag
        | _ -> assert false
    end
  | v -> (Asttools.value_string v)

let rec enclose fmt ge ns = function
  | (Id _) as e
    -> gen_expr fmt ge ns e
  | e
    ->
    begin
      fpf fmt "@[<b 2>(";
      gen_expr fmt ge ns e;
      fpf fmt ")@]";
    end

and gen_expr fmt ge ns = function
  | SelfInit _ -> ()
  | Value {cont=v}
    -> fpf fmt "@[<h>%s@]" (gen_value ge ns v)
  | Id {cont=(lns,x)}
    ->
    let ns = match lns with
      | [] -> ns
      | ns::_ -> ns
    in
    let cname =
      try ((ge#get_sym_env ns)#get x)#get_c_name with | _ -> x
    in
    fpf fmt "@[<h>%s@]" cname
  | BinOp {cont=(o,e1,e2)}
    ->
    begin
      fpf fmt "@[<b 2>(";
      gen_expr fmt ge ns e1;
      fpf fmt ")@;%s (" o;
      gen_expr fmt ge ns e2;
      fpf fmt ")@]";
    end
  | PreOp {cont=(o,e)}
    ->
    begin
      fpf fmt "@,@[<b2>(%s" o;
      enclose fmt ge ns e;
      fpf fmt ")@]";
    end
  | PostOp {cont=(o,e)}
    ->
    begin
      enclose fmt ge ns e;
      fpf fmt "%s" o;
    end
  | TerOp {cont=(e1,e2,e3)}
    ->
    begin
      fpf fmt "@[<b 2>";
      enclose fmt ge ns e1;
      fpf fmt "@;?@;";
      enclose fmt ge ns e2;
      fpf fmt "@;:@;";
      enclose fmt ge ns e3;
      fpf fmt "@]";
    end
  | AssignOp {cont=(str, lhs, rhs)}
    ->
    begin
      gen_expr fmt ge ns lhs;
      fpf fmt "@;%s@;" str;
      gen_expr fmt ge ns rhs;
    end
  | Call {cont=(f,pl)}
    -> gen_call fmt ge ns pl f
  | Index {cont=(expr, idx_list)}
    ->
    begin
      enclose fmt ge ns expr;
      fpf fmt "@[<h>[";
      Pptools.seppp fmt "][" (gen_expr fmt ge ns) idx_list;
      fpf fmt "]@]@,";
    end
  | Field (_) | PField (_) as field
    -> gen_field_access fmt ge ns field
  | Sizeof {cont=(texpr)}
    ->
    begin
      let t = (Asttools.texpr_get_info texpr).TypeAlg.talg in
      fpf fmt "@;@[sizeof@;(@[<h>%s@])@]"
        ((ge#get_type_env ns)#get_sizeof
            (ge#get_sym_env ns)#get_mangler t)
    end
  | Cast {cont=(texpr, expr)}
    ->
    begin
      let t = (Asttools.texpr_get_info texpr).TypeAlg.talg in
      (ge#debug:Debug_printer.printer)#locprint 1
        (Asttools.expr_get_loc expr) ns
        "@[<b 2>Cast to %t@]" (fun fmt -> TypeAlg.pp fmt t);
      fpf fmt "@,@[<h>@[<h>(%s)@]@,("
        (TypeAlg.toCType ~mang:((ge#get_sym_env ns)#get_mangler)
           ~ns:ns "" t);
      gen_expr fmt ge ns expr;
      fpf fmt ")@]@,";
    end
  | Decl {cont=d}
    -> gen_decl fmt ge ns d
  | Compound {cont=sl}
    ->
    begin
      fpf fmt "@,@[<v>({@[<v 1>";
      List.iter (gen_statement fmt ge ns) sl;
      fpf fmt "@]@,})@]";
    end

and gen_field_access fmt ge ns = function
  | Field {cont=e,f}
    ->
    begin
      fpf fmt "@,@[<h>";
      enclose fmt ge ns e;
      fpf fmt ".%s@]" f;
    end
  | PField {cont=e,f}
    ->
    begin
      fpf fmt "@,@[<h>";
      enclose fmt ge ns e;
      fpf fmt "->%s@]" f;
    end
  | _ -> assert false


and genlen ns mang accu = function
  | [] -> accu
  | le::tail ->
    genlen ns mang
      (accu ^ "[" ^ (TypeAlg.cst_to_c ~mang:mang ~ns:ns le) ^ "]"
      ) tail

and gen_local_struct ?(global = false) fmt ns ge lname cbox alen =
  let mangler = (ge#get_sym_env ns)#get_mangler in
    ge#set_local_name lname;
    fpf fmt "@[<v>";
    fpf fmt "@[<h>%s%s "
      (if global then "static " else "")
      (cbox#get_struct_name mangler);
    fpf fmt "%s" lname;
    fpf fmt "%s;@]@;@]" (genlen ns mangler "" alen);

and subarray_len ?(global = false) fmt ns ge vd lname =
  begin
    let ct, len =
      match TypeAlg.typeof_vdecl vd with
        | TypeAlg.Array (t, l,_) -> (t,l)
        | t -> (t,[])
    in
      gen_local_struct ~global:global fmt ns ge lname
        ((ge#get_type_env ns)#get_class_from_type ct) len;
      len
  end

and gen_sub_local ?(global = false) exp fmt ns ge vd name lname = function
  | []
    ->
    begin
      fpf fmt "@;=@;&%s" lname;
      match vd.vinit with
        | Some (SelfInit ({ cont=(t,args) } as b))
          ->
          begin
            fpf fmt ";@]@\n@[<h>";
            gen_expr fmt ge ns
              (Build_class.gen_selfinit ns ge b.loc name t args)
          end
        | Some _
          -> assert false
        | _ -> ()
    end
  | arlen
    ->
    let mangler = (ge#get_sym_env ns)#get_mangler in
    (* TODO: following two functions are dirty, need refactor *)
    let rec pp_index n = function
      | [] -> ()
      | _::tl
        ->
        begin
          fpf fmt "[i%i]" n;
          pp_index (n + 1) tl;
        end
    in
    let rec pp_loop n = function
      | []
        ->
        begin
          fpf fmt "@[<v 2>@[<h>%s" name;
          pp_index 0 arlen;
          fpf fmt "@;=@;&%s" lname;
          pp_index 0 arlen;
          fpf fmt ";@]@]";
        end
      | le::tl
        ->
        begin
          fpf fmt "@[<v 2>@[<h>for (unsigned long i%i = 0;" n;
          fpf fmt " i%i < %s;" n (TypeAlg.cst_to_c ~mang:mangler ~ns:ns le);
          fpf fmt " ++i%i)@]@;" n;
          pp_loop (n + 1) tl;
          fpf fmt "@]";
        end
    in
      begin
        fpf fmt ";@;";
        pp_loop 0 arlen;
        fpf fmt "@]";
      end

and gen_displayed_vdecl
    ?(deconly = false) ?(infield = false) ?(global = false) exp
    fmt ns ge vd name lname arlen =
  let mangler = (ge#get_sym_env ns)#get_mangler in
  let is_local = List.mem "local" vd.vmods in
  let ta = (ge#get_type_env ns)#canon (TypeAlg.typeof_vdecl vd) in
    begin
      fpf fmt "@,@[<h>%s" (if exp && deconly then "extern " else "");
      Qualifiers.gen fmt ns ge vd.vloc vd.vmods;
      fpf fmt "%s" (TypeAlg.toCType ~exp:exp ~mang:mangler ~ns:ns name ta);
      if not (deconly || infield) then
        begin
          match vd.vinit with
            | _ when is_local ->
              gen_sub_local ~global:global exp fmt ns ge vd name lname arlen
            | Some e ->
              fpf fmt "@;=@;";
              gen_expr fmt ge ns e;
            | _ ->()
        end;
      fpf fmt "@]"
    end

and gen_decl
    ?(deconly = false) ?(infield = false) ?(global = false)
    fmt ge ns vd =
  let debug : Debug_printer.printer = ge#debug in
  let exported =
    try ((ge#get_sym_env ns)#get vd.vname)#is_exported with
      | _ -> debug#print 4 "symbol %S not found." vd.vname; false
  in
  let name =
    if exported && not infield then
      ((ge#get_sym_env ns)#get vd.vname)#get_c_name
    else vd.vname
  and is_local = List.mem "local" vd.vmods
  in
  let display = infield || exported || not deconly  in
  let lname = ge#mangler#build_local_obj_name name in
  begin
    if display then
      gen_displayed_vdecl ~deconly ~infield ~global exported
        fmt ns ge vd name lname (
          if is_local && (not deconly || not global) then
            subarray_len ~global:global fmt ns ge vd lname
          else []
        );
  end

and gen_param fmt ge ns al =
  let rec r_gen_param = function
    | [] -> ()
    | h::[]
      -> gen_expr fmt ge ns h
    | h::t
      ->
      begin
        gen_expr fmt ge ns h;
        fpf fmt ",@;";
        r_gen_param t
      end
  in
  begin
    fpf fmt "(@[<b>";
    r_gen_param al;
    fpf fmt "@])"
  end

and gen_call fmt ge ns al = function
  | (Id ({cont=(ml,n)} as b)) as x
    ->
    begin
      try
        let n =
          ((ge#get_type_env ns)#get_box n)#get_init_name
            (ge#get_sym_env ns)#get_mangler
        in
        b.cont <- (ml,n)
      with _ -> ()
    end;
    begin
      fpf fmt "@[<h>";
      gen_expr fmt ge ns x;
      gen_param fmt ge ns al;
      fpf fmt "@]";
    end
  | e
    ->
    begin
      fpf fmt "@,@[<h>(";
      gen_expr fmt ge ns e;
      fpf fmt ")";
      gen_param fmt ge ns al;
      fpf fmt "@]"
    end

and gen_cond fmt ge ns = function
  | (AssignOp _) as e
    ->
    begin
      fpf fmt "@,@[<b 2>(";
      gen_expr fmt ge ns e;
      fpf fmt ")@]"
    end
  | e
    -> gen_expr fmt ge ns e

and gen_statement fmt ge ns = function
  | EmptyStatement _
    -> fpf fmt "@[<h>;@]"

  | Expr {cont=e}
    ->
    begin
      fpf fmt "@,@[<h>";
      gen_expr fmt ge ns e;
      fpf fmt "@];"
    end

  | Block {cont=sl}
    ->
    begin
      fpf fmt "@,@[<v>{@[<v 1>";
      List.iter (gen_statement fmt ge ns) sl;
      fpf fmt "@]@,}@]" ;
    end

  | If {cont=(e,s,EmptyStatement _)}
    ->
    begin
      fpf fmt "@,@[<v 2>if@[<b 1>@;(";
      gen_cond fmt ge ns e;
      fpf fmt ")@]";
      gen_statement fmt ge ns s;
      fpf fmt "@]";
    end

  | If {cont=(e,s0,s1)}
    ->
    begin
      fpf fmt "@,@[<v 2>if@[<b 1>@;(";
      gen_cond fmt ge ns e;
      fpf fmt ")@]";
      gen_statement fmt ge ns s0;
      fpf fmt "@]@,@[<v 2>else";
      gen_statement fmt ge ns s1;
      fpf fmt "@]";
    end

  | While {cont=(e,s)}
    ->
    begin
      fpf fmt "@,@[<v 2>while@[<b 1>@;(";
      gen_cond fmt ge ns e;
      fpf fmt ")@]@;";
      gen_statement fmt ge ns s;
      fpf fmt "@]";
    end

  | Do {cont=(e,s)}
    ->
    begin
      fpf fmt "@,@[<v>do@[<v 2>@,";
      gen_statement fmt ge ns s;
      fpf fmt "@]@,@]while@[<b 1>@;(";
      gen_cond fmt ge ns e;
      fpf fmt ")@];";
    end

  | For {cont=(e0,e1,e2,s)}
    ->
    let oppp e = applyop (gen_expr fmt ge ns) () e in
    begin
      fpf fmt "@,@[<v 2>for@[<b 1>@;(";
      oppp e0;
      List.iter (fun e -> fpf fmt ";"; oppp e) [e1;e2];
      fpf fmt ")@]";
      gen_statement fmt ge ns s;
      fpf fmt "@]";
    end

  | Continue _
    -> fpf fmt "@,@[<h>continue;@]"

  | Break _
    -> fpf fmt "@,@[<h>break;@]"

  | Return {cont=e}
    ->
    begin
      fpf fmt "@,@[<h 2>return@;";
      applyop (gen_expr fmt ge ns) () e;
      fpf fmt "@;;@]";
    end

  | Switch {cont=(e, sl)}
    ->
    let rec casepp (c, sl) =
      fpf fmt "@,@[<v 2>@[<h>";
      begin
        match c with
          | Default _
            -> fpf fmt "@[<h>default@]"
          | SwValue {cont=v}
            -> fpf fmt "@[<h>case %s@]" (gen_value ge ns v)
      end;
      fpf fmt ":@,@]";
      List.iter (fun s -> gen_statement fmt ge ns s) sl;
      fpf fmt "@]";
    in
    begin
      fpf fmt "@,@[<v 2>@[<h>switch@[<b 1>@;(";
      gen_expr fmt ge ns e;
      fpf fmt ")@]@]@,{@[<v 1>";
      List.iter casepp sl;
      fpf fmt "@]@,}@]";
    end

  | Label {cont=label}
    -> fpf fmt "@;<0 -1>@[<h>%s:@]" label

  | Goto {cont=label}
    -> fpf fmt "@,@[<h>goto@;%s;@]" label

  | Asm {cont=asm}
    -> gen_asmblock fmt ge ns asm

  | Delete {cont=e}
    -> assert false

and gen_asmoperand fmt ge ns asmop =
  let name =
    match asmop.asmopname with
      | Some x -> "["^x^"]"
      | _ -> ""
  in
  begin
    fpf fmt "@[<h>%s%S(" name asmop.asmconstraint;
    gen_expr fmt ge ns asmop.asmexpr;
    fpf fmt ")@]"
  end

and gen_asmblock fmt ge ns asm =
  let asmbuilder = new Asmstring.builder asm.asmcode in
  begin
    fpf fmt "@,@[<v 2>@[<h>__asm__@;";
    if asm.asmvolatile then
      fpf fmt "volatile@;";
    fpf fmt "@] (";
    asmbuilder#build fmt;
    begin
      match (asm.asmouputs,asm.asminputs,asm.asmclobbers) with
        | ([],[],[]) -> ()
        | (ao,ai,ac)
          ->
          begin
            fpf fmt "@,@[<h>:";
            Pptools.seppp fmt "," (gen_asmoperand fmt ge ns) ao;
            fpf fmt "@]@,@[<h>:";
            Pptools.seppp fmt "," (gen_asmoperand fmt ge ns) ai;
            fpf fmt "@]@,";
            if ac <> [] then
              begin
                fpf fmt "@[<h>:";
                Pptools.seppp fmt "," (
                  fpf fmt "%S"
                ) ac;
                fpf fmt "@]@,";
              end;
          end
    end;
    fpf fmt ");@]"
  end

let gen_param fmt ge ns ((ml, _, p),t) =
  let ta = (ge#get_type_env ns)#canon t in
  List.iter (fpf fmt "%s@;") ml;
  fpf fmt "%s" (
    TypeAlg.toCType p ~ns:ns
      ~mang:((ge#get_sym_env ns)#get_mangler)
      ta
  )

let is_empty_body fd =
  match fd.fbody with
    | EmptyStatement _ :: [] -> true
    | _ -> false

let gen_fdecl ?(forw=false) ?(deconly = false) ?(meth = false)
    fmt ge ns fd =
  if (fd.fasname = None) || (fd.fbody != []) then
    begin
      let (cname,exported) =
        try
          let sym = (ge#get_sym_env ns)#get fd.fname in
          (sym#get_c_name, sym#is_exported)
        with
          | _ -> (fd.fname,false)
      in
      let exported = (exported || fd.fexport)
      in
      if forw || exported || not deconly then
        begin
          let (tpl,rt) =
            match fd.finfo with
              | Some {TypeAlg.talg = TypeAlg.Fun (tpl,rt,_)} ->
                (tpl,rt)
              | _ ->
                raise (TypeAlg.DEBUG_missing_type_info fd.floc)
          in
          let param =
            if (List.length tpl = List.length fd.fparams) then
              List.combine fd.fparams tpl
            else
              List.map (fun ((_,t,_) as x) ->
                (x,(ge#get_type_env ns)#canon
                  (TypeAlg.typeof_texpr t))) fd.fparams
          in
          let static =
            if not (exported || meth) then
              "static " ^ (
                if fd.finline then "inline "
                else ""
              )
            else ""
          in
          fpf fmt "@,@,@[<v>@[<h>%s@,%s" static
            (TypeAlg.toCType cname ~ns:ns ~exp:exported
               ~mang:((ge#get_sym_env ns)#get_mangler) rt);
          fpf fmt "(@[<b>";
          (match param with
            | [] -> fpf fmt "void"
            | _ ->
              let apply_unused ((ml, tr, param), t) =
                ((ml, tr, param ^ " __attribute__((unused))"), t) in
              let param =
                if meth && is_empty_body fd then
                  List.map apply_unused param
                else param in
              Pptools.seppp fmt ", " (gen_param fmt ge ns) param
          );
          fpf fmt "@])%s@]"
            (if fd.fbody = [] || deconly then ";" else "");
          if fd.fbody <> [] && not deconly then
            begin
              match fd.fbody with
                | [] -> ()
                | EmptyStatement _::[] ->
                  fpf fmt "@[<v>{}@]"
                | _ ->
                  begin
                    fpf fmt "@,@[<v>{@[<v 1>";
                    List.iter (gen_statement fmt ge ns) fd.fbody;
                    fpf fmt "@]@,}@]";
                  end
            end;
          fpf fmt "@]@,";
        end;
    end

let gen_typedef ?(deconly=false) fmt ge ns td =
  let export =
    true
    || match td.tdnspace with
        | [] -> false
        | _ -> true
  in
  let tdname = ge#mangler#mangle ns td.tdname in
  if export = deconly then
    begin
      let ta =
        (ge#get_type_env ns)#canon (TypeAlg.typeof_typedef td)
      in
      fpf fmt "@,@[<b 2>typedef@;";
      fpf fmt "@[<h>%s@]"
        (TypeAlg.toCType tdname ~ns:ns
           ~exp:export
           ~mang:((ge#get_sym_env ns)#get_mangler)
           ta);
      fpf fmt ";@]"
    end

let gen_field_decl fmt ge ns vd =
  let t =
    (ge#get_type_env ns)#canon (TypeAlg.typeof_texpr vd.vtype)
  in
  begin
    fpf fmt "@,@[<h>";
    gen_decl ~infield:true fmt ge ns vd;
    (match t with
      | TypeAlg.Int (_, size,_)
        ->
        if (TypeAlg.not_multiple size) then
          fpf fmt ":%Ld" size;
      | _ -> ());
    fpf fmt ";@]"
  end

let gen_struct_field fmt ge ns = function
  | SField vd
    -> gen_field_decl fmt ge ns vd
  | _ -> assert false

let gen_struct ?(deconly=false) ?(forw=false) fmt ge ns packed ibox =
  let exported = Asttools.is_box_exported ns ibox in
  if exported = deconly then
    let pack () =
      if packed then fpf fmt "@[<h>/* packed */@]";
    in
    let nse = if exported then ns else "" in
    let bname =
      ((ge#get_type_env ns)#get_box ibox.bname.cont)#get_name
    in
    let strname = ge#mangler#build_struct_name nse bname in
    let typename = ge#mangler#mangle nse bname in
    begin
      if forw then
        fpf fmt "@,@[<h>typedef @[<h>%s@]@;@[<h>%s@];@]@,"
          strname typename
      else
        begin
          fpf fmt "@,@[<v>@[<h>%s@]@,{@[<v 1>" strname;
          List.iter (
            fun f ->
	      gen_struct_field fmt ge ns f;
          ) ibox.members;
          fpf fmt "@]@,}@[<h>@;%s" (
	    if packed then ge#mangler#pack else ""
          );
          fpf fmt ";@]@,@]";
          pack ();
        end
    end

let gen_union ?(deconly=false) ?(forw=false) fmt ge ns inner =
  let exported = Asttools.is_box_exported ns inner in
  if exported = deconly then
    let nse = if exported then ns else "" in
    let bname =
      ((ge#get_type_env ns)#get_box inner.bname.cont)#get_name
    in
    let uname = ge#mangler#build_union_name nse bname in
    let typename = ge#mangler#mangle nse bname in
    begin
      if forw then
        fpf fmt "@,@[<h>typedef @[<h>%s@]@;@[<h>%s@];@]@,"
          uname typename
      else
        begin
          fpf fmt "@,@[<v>@[<h>%s@]@,{@[<v 1>" uname;
          List.iter (fun f ->
	    gen_struct_field fmt ge ns f;
          ) inner.members;
          fpf fmt "@]@,};@,@]";
        end
    end

let gen_enum_field fmt ge ns f =
  begin
    match f with
      | EField {cont=(x,eopt)}
        ->
        begin
          fpf fmt "@,@[<h>%s" (gen_value ge ns (VTag x));
          begin
            match eopt with
              | Some e
                ->
                begin
                  fpf fmt "@;=@;";
                  gen_expr fmt ge ns e;
                end
              | _ -> ()
          end;
          fpf fmt ",@]"
        end
      | _ -> assert false
  end

let gen_enum ?(deconly=false) ?(forw=false) fmt ge ns inner =
  let exported = Asttools.is_box_exported ns inner in
  if exported = deconly then
    begin
      let nse = if exported then ns else "" in
      let name =
        ((ge#get_type_env ns)#get_box inner.bname.cont)#get_name
      in
      let ename = ge#mangler#build_enum_name nse name in
      let typename = ge#mangler#mangle nse name in
      if forw then
        fpf fmt "@,@[<h>typedef @[<h>%s@]@;@[<h>%s@];@]@,"
          ename typename
      else
        begin
          fpf fmt "@,@[<v>@[<h>%s@]@,{@[<v 1>" ename;
          List.iter (fun f ->
	    gen_enum_field fmt ge ns f;
          ) inner.members;
          fpf fmt "@]@,};@]";
        end
    end

let gen_boxdecl ?(deconly=false) ?(forw=false) fmt ge ns = function
  | Struct {cont=(packed, inner)}
    -> gen_struct ~deconly ~forw fmt ge ns packed inner
  | Class {cont=bd}
    -> Build_class.build ~deconly ~forw (gen_fdecl ~forw:false) fmt ns ge bd
  | Interface {cont=bd}
    ->
    begin
      Build_interface.build ~deconly ~forw fmt ns ge bd;
      fpf fmt "@,@[<h>/* interface not yet fully implemented : %s */@]"
        bd.bname.cont
    end
  | Enum {cont=bd}
    -> gen_enum ~deconly ~forw fmt ge ns bd
  | Union {cont=bd}
    -> gen_union ~deconly ~forw fmt ge ns bd

let gen_mcdecl ?(deconly=false) fmt ge ns mc =
  begin
    if (Asttools.is_mclass_exported ns mc) = deconly then
      Macroclass.build_macro_class fmt gen_statement ns ge mc
  end

let gen_property ?(deconly=false) fmt ge ns prop =
  begin
    if deconly then
      Property.build_property fmt gen_statement ns ge prop
  end

let macro_vdecl ?(deconly=false) fmt ge ns = function
  | { vname=name; vinit=Some v} as vdecl
    ->
    if (not deconly) || (not (Asttools.vdecl_static vdecl)) then
      begin
        let macro_mode = Formacro.macro_print_mode fmt in
        let n = ((ge#get_sym_env ns)#get name)#get_c_name in
        fpf fmt "@,@]@.";
        macro_mode true;
        fpf fmt "@[<h 2>#define %s@;(" (n);
        gen_expr fmt ge ns v;
        fpf fmt ")@]";
        macro_mode false;
        fpf fmt "@.@[<v>@,";
      end
  | _ -> ()

let gen_forward_type fmt ge ns = function
  | BDecl b
    -> gen_boxdecl ~deconly:true ~forw:true fmt ge ns b
  | _ -> ()

let macro_fdecl fmt ge ns funcdecl =
  Formacro.gen_macro_fundecl gen_statement
    fmt ns ge
    ("__cb_"
     ^((ge#get_sym_env ns)#get funcdecl.Ast.fname)#get_c_name
     ^ "_return")
    funcdecl

let rec gen_gdecl ?(deconly=false) fmt ge ns = function
  | FDecl  fdecl
    -> gen_fdecl ~deconly ?meth:(Some false) fmt ge ns fdecl
  | VDecl vdecl
    ->
    if (not deconly) || (not (Asttools.vdecl_static vdecl)) then
      begin
        fpf fmt "@,@[<b 2>";
        gen_decl ~deconly ~global:true fmt ge ns vdecl;
        fpf fmt ";@]";
      end
  | TDef td
    -> gen_typedef ~deconly fmt ge ns td
  | BDecl b
    -> gen_boxdecl ~deconly fmt ge ns b
  | MCDecl mc
    -> gen_mcdecl ~deconly fmt ge ns mc
  | Macro (VDecl vd)
    -> macro_vdecl fmt ge ns vd
  | Macro (FDecl fd)
    -> macro_fdecl fmt ge ns fd
  | Proper p
    ->
    begin
      fpf fmt "@,@[<b 2>/* Property: %s */@]@," p.pname;
      gen_property ~deconly fmt ge ns p
    end
  | _ -> ()
