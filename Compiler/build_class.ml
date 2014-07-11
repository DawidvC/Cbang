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

(* Code generation for class *)

open Ast

let fpf fmt = Format.fprintf fmt

let gen_class_base_type fmt ge =
  begin
    fpf fmt "@,@[<h>#ifndef _CB_OBJ_BASE_TYPE__@]";
    fpf fmt "@,@[<h>#define _CB_OBJ_BASE_TYPE__@]";
    fpf fmt "@,@[<v>@[<v 2>@[<h>typedef struct @]{";
    fpf fmt "@,@[<h>void *(*%s)(void*, int32_t);@]" ge#mangler#semfun;
    fpf fmt "@]@,} *%s;@]" TypeAlg.objbase_name;
    fpf fmt "@,@[<h>#endif@]@,";
  end

let gen_selfinit ns ge loc name t args =
  begin
    let builder = new AstBuilder.nwbuilder ge in
    let t = (Asttools.texpr_get_info t).TypeAlg.talg in
    let (ns',tn) = TypeAlg.get_name ns t
    in
    let sym = (ge#get_sym_env ns')#get tn in
    let nid =
      builder#id name (TypeAlg.info t true false) loc
    in
    let id =
      Id {loc=loc; cont=(ns'::[],tn);
          info= Some (TypeAlg.info sym#get_type true false);}
    in
    let call =
      (builder#call id (nid::args) (TypeAlg.info t true false) loc)
    in
      call;
  end

(*
 * Interface converter
 *)

let build_swcase ns (ge:'ge) box (nsi,inter) =
  let builder:'ge AstBuilder.builder = ge#builder in
  let ibox = (ge#get_type_env nsi)#get_interface inter in
  let inter_id = ibox#semid ge#mangler in
  let swv =
    builder#swvalue (VTag inter_id)
      (TypeAlg.info (TypeAlg.Int(true,32L, TypeAlg.stdmeta ())) true true)
      box#get_loc
  in
  let rinfo = TypeAlg.info TypeAlg.voidstar true true in
  let swa = ibox#sw_accessor ge#mangler builder box#btype box#get_loc in
    ( swv, [builder#return ~e:swa rinfo box#get_loc] )

let retnull ns (ge:'ge) t loc =
  let builder:'ge AstBuilder.builder = ge#builder in
  let rinfo =
    TypeAlg.info t true true
  in
  let null =
    builder#cast
      TypeAlg.voidstar
      (builder#value (VInt 0L) loc)
      loc
  in
    builder#return ~e:null rinfo loc


let build_semfun ns (ge:'ge) box =
  let builder:'ge AstBuilder.builder = ge#builder in
  let semrtype = TypeAlg.voidstar in
  let semtype =
    TypeAlg.Fun ([TypeAlg.Int (true,32L, TypeAlg.stdmeta ())],
                 semrtype, TypeAlg.stdmeta ())
  in
  let pid =
    builder#id "inter"
      (TypeAlg.info (TypeAlg.Int (true,32L,TypeAlg.stdmeta ())) true true)
      box#get_loc
  in
  let default =
    (
      builder#swdefault
        (TypeAlg.info TypeAlg.Void true true)
        box#get_loc,
      [retnull ns ge semrtype box#get_loc]
    )
  in
  let swl =
    (List.fold_left
       (fun l i -> (build_swcase ns ge box i)::l)
       [default]
       box#interfaces)
  in
  let swstm =
    builder#switch pid swl
      (TypeAlg.info TypeAlg.Void true true)
      box#get_loc
  in
  let sf =
    builder#fundecl
      (box#get_name ^ "$" ^ ge#mangler#semfun)
      [
        ([],builder#texpr box#btype box#get_loc, "this");
        ([],builder#texpr
          (TypeAlg.Int (true,32L,TypeAlg.stdmeta ()))
          box#get_loc,
         "inter")
      ]
      false semrtype [swstm] box#get_loc semtype
  in sf

(*
 * Preparing methods for code gen
 *)

let prepare_method ns (ge:'ge) box fd =
  let builder:'ge AstBuilder.builder = ge#builder in
  let _this = builder#id "_this"
    (TypeAlg.info TypeAlg.voidstar false true)
    fd.floc
  in
  let this = builder#id "this"
    (TypeAlg.info box#btype false true)
    fd.floc
  in
  begin
    builder#addfstparam [] TypeAlg.voidstar "_this" fd;
    begin
      match fd.fbody with
        | [] | (EmptyStatement _)::[]
          -> ()
        | _
          ->
          begin
            fd.fbody <- (
              builder#expr (
                builder#assignop "=" this _this
                  (TypeAlg.info box#btype true true) fd.floc
              ) (TypeAlg.info box#btype true true) fd.floc
            ) :: fd.fbody;
            builder#addfdvdecl "this __attribute__((unused))"
              box#btype fd fd.floc;
            Field_access.in_method ns ge fd;
            Callrewrite.in_method ns ge fd;
          end;
    end;
    fd.fname <- box#get_method_pointer_name ge#mangler fd.fname;
  end

let prepare_methods ns ge box ib =
  (ge#get_type_env ns)#set_current_box box#get_name;
  List.iter (function
    | Method f when f.fbody <> []
      -> prepare_method ns ge box f
    | _ -> ()
  ) ib.members ;
  (ge#get_type_env ns)#leave_curent_box

let build_field fmt ns ge box (f,t) =
  let out =
    if box#is_local f then
      begin
        let box =
          (ge#get_type_env ns)#get_class_from_type
            ((ge#get_type_env ns)#canon t)
        in
        (box#get_struct_name ge#mangler) ^ " " ^ f
      end
    else
      TypeAlg.toCType ~mang:ge#mangler ~ns:ns f t
  in
    fpf fmt "@,@[<h>%s;@]" out

let filter fmt ns ge box ((f,t) as ft)=
  match t with
    | TypeAlg.Fun (tpl,rt,m) ->
      build_field fmt ns ge box (f,TypeAlg.Fun (TypeAlg.voidstar::tpl,rt,m))
    | _ ->
      build_field fmt ns ge box ft

let gen_vtable_struct fmt ns ge box =
  let svtbl = "struct " ^ (box#get_vtbl_sname ge#mangler) in
  begin
    fpf fmt "@,@[<v>@[<h>%s@]@,{@[<v 1>" svtbl;
    List.iter (filter fmt ns ge box) box#get_method_descr;
    fpf fmt "@]@,};@]@,";
  end

let build_td fmt ns ge box =
  let cname = box#get_c_name ge#mangler in
  let sname = box#get_struct_name ge#mangler in
    begin
      fpf fmt "@,@[<h>typedef %s@;*%s;@]@," sname cname;
    end

let build_interface_container fmt ns ge box (ns',inter) =
  let ibox = (ge#get_type_env ns')#get_interface inter in
  let sivtbl = "struct " ^ (ibox#get_vtbl_sname ge#mangler) in
  let cname = box#get_c_name ge#mangler in
    begin
      fpf fmt "@,@[<v>struct {@[<v -6>";
      fpf fmt "@,@[<h>%s *%s;@]" sivtbl ge#mangler#ivtable_field;
      fpf fmt "@,@[<h>%s real_this;@]" cname;
      fpf fmt "@]@,} %s$%s;@]" ns' inter;
    end

let build_extra_interfaces fmt ns ge box =
  fpf fmt "@,/* Adding fields for new interface implem */";
  fpf fmt "@,@[<v>@[<v 2>struct %s_s {"
    (ge#mangler#interfaces_block ns box#get_name);
  List.iter (build_interface_container fmt ns ge box) box#interfaces;
  fpf fmt "@]@,} %s;@]" (ge#mangler#interfaces_block ns box#get_name)

let build_main_struct fmt ns ge box =
  let sname = box#get_struct_name ge#mangler in
  let svtbl = "struct " ^ (box#get_vtbl_sname ge#mangler) in
  let inter =
    TypeAlg.toCType ~mang:ge#mangler ~ns:ns ge#mangler#semfun
      (TypeAlg.Fun ([
        box#btype;
        (TypeAlg.Int (true,32L,TypeAlg.stdmeta ()))
      ],TypeAlg.voidstar, TypeAlg.stdmeta ()))
  in
  begin
    fpf fmt "@,@[<v>@[<h>%s@]@,{@[<v 1>" sname;
    fpf fmt "@,@[<h>%s;@]" inter;
    fpf fmt "@,@[<h>struct %s_s *%s;@]"
      (ge#mangler#interfaces_block ns box#get_name)
      ge#mangler#interfaces_addr;
    fpf fmt "@,@[<h>%s@;*%s;@]" svtbl (ge#mangler#vtable_field);
    List.iter (build_field fmt ns ge box) box#get_field_descr;
    build_extra_interfaces fmt ns ge box;
    fpf fmt "@]@,};@]@,";
  end

let build_method gen_fdecl fmt ns ge box fd =
  begin
    gen_fdecl ?deconly:(Some false) ?meth:(Some true) fmt ge ns fd
  end

let build_methods gen_fdecl fmt ns ge box bd =
  begin
    List.iter (function
      | Method fd when fd.fbody <> []
          -> build_method gen_fdecl fmt ns ge box fd
      | _ -> ()
    ) bd.members;
  end

let build_concrete_vtbl ?(deconly=false) exp fmt ns ge box =
  let svtbl = "struct " ^ (box#get_vtbl_sname ge#mangler) in
  let cvtbl = box#get_vtbl_cname ge#mangler in
    if exp || not deconly then
      begin
        fpf fmt
          "@,@[<v 2>@[<h>%s%s@;%s"
          (if deconly then "extern " else "")
          svtbl cvtbl;
        if not deconly then
          begin
            fpf fmt "@;=@]@,{@[<v 1>@,@[<b>";
            List.iter (
              fun (m,_) ->
                if not (box#is_abstract m) then
                  fpf fmt "@;@[<h>%s,@]"
                    (box#get_method_pointer_name ge#mangler m)
                else
                  fpf fmt "@;@[<h>(void*)0@]"
            ) box#get_method_descr;
            fpf fmt "@]@]@,}";
          end
        else
          fpf fmt "@]";
        fpf fmt ";@]";
    end

let build_interface_vtbl fmt ns (ge:'ge) box (ns',inter) =
  let ibox = (ge#get_type_env ns')#get_interface inter in
  let ivtbl = box#get_interface_vtbl inter ge#mangler in
  let sivtbl = "struct " ^ (ibox#get_vtbl_sname ge#mangler) in
    begin
      fpf fmt "@,@[<v 2>@[<h>%s@;%s"
        sivtbl ivtbl;
      fpf fmt "@;=@]@,{@[<v 1>@,@[<b>";
      List.iter (
        fun (m,_) ->
          if not (box#is_abstract m) then
            fpf fmt "@;@[<h>%s,@]"
              (box#get_method_pointer_name ge#mangler m)
          else
            fpf fmt "@;@[<h>(void*)0,@]";
      ) ibox#get_method_descr;
      fpf fmt "@]@]@,}";
      fpf fmt ";@]";
    end

let finit ns ge x vinit loc =
  let t = (ge#get_type_env ns)#canon (TypeAlg.typeof vinit) in
  let builder = new AstBuilder.nwbuilder ge in
    builder#expr (
      builder#assignop "="
        (
          builder#id x (TypeAlg.info t false false) loc
        )
        vinit
        (TypeAlg.info t false false)
        loc
    ) (TypeAlg.info t false false) loc


let extra_interfaces_init ns (ge:'ge) box body loc =
  let builder:'ge AstBuilder.builder = ge#builder in
    let tinfo = TypeAlg.info TypeAlg.voidstar true true in
  let addr =
    builder#expr
      (builder#assignop "=" (
        builder#pfield (
          builder#id "this"
            (TypeAlg.info TypeAlg.voidstar true true)
            loc
        ) ge#mangler#interfaces_addr tinfo loc )
         (builder#preop "&" (
           builder#pfield (
             builder#id "this"
               (TypeAlg.info TypeAlg.voidstar true true)
               loc
           ) (ge#mangler#interfaces_block ns box#get_name) tinfo loc
          ) tinfo loc
         )
         tinfo loc
      ) tinfo loc
  in
  let init_inter b (ns',inter) =
    (builder#expr (
      builder#assignop "=" (
        builder#field (
          builder#field (
            builder#pfield (
              builder#id "this"
                (TypeAlg.info TypeAlg.voidstar true true)
                loc
            ) (ge#mangler#interfaces_block ns box#get_name) tinfo loc
          ) (ns'^"$"^inter) tinfo loc
        ) "real_this" tinfo loc
      ) (
        builder#id "this" (TypeAlg.info TypeAlg.voidstar true true) loc
      ) tinfo loc
    ) tinfo loc)
    :: ( builder#expr (
      builder#assignop "=" (
        builder#field (
          builder#field (
            builder#pfield (
              builder#id "this"
                (TypeAlg.info TypeAlg.voidstar true true)
                loc
            ) (ge#mangler#interfaces_block ns box#get_name) tinfo loc
          ) (ns'^"$"^inter) tinfo loc
        ) ge#mangler#ivtable_field tinfo loc
      ) (
        builder#preop "&" (
          builder#id
            (box#get_interface_vtbl inter ge#mangler)
            tinfo box#get_loc
        ) tinfo box#get_loc
      ) tinfo loc
      ) tinfo loc
    ) :: b
  in
  begin
    List.fold_left init_inter (addr::body) box#interfaces;
  end

let semfun_init ns (ge:'ge) box body loc =
  let t = TypeAlg.Fun ([TypeAlg.Int (true,32L,TypeAlg.stdmeta
    ())],TypeAlg.voidstar, TypeAlg.stdmeta ()) in
  let tinfo = (TypeAlg.info t true true) in
  let builder:'ge AstBuilder.builder = ge#builder in
  let expr =
    match box#interfaces with
      | [] ->
        builder#cast
          TypeAlg.voidstar
          (builder#value (VInt 0L) loc)
          loc
      | _ ->
        builder#id
          ((box#get_name ^ "$" ^ ge#mangler#semfun)) tinfo loc
  in
  let f =
    builder#pfield
      (builder#id "this"
         (TypeAlg.info TypeAlg.voidstar true true)
         loc
      ) ge#mangler#semfun tinfo loc
  in
    (builder#expr
       (builder#assignop "=" f expr tinfo loc)
       tinfo loc
    )::body

let vtbl_init ns ge box loc =
  let builder = new AstBuilder.nwbuilder ge in
  let cvtbl = box#get_vtbl_cname ge#mangler in
    builder#expr (
      builder#assignop "="
        (
          builder#pfield
            (
              builder#id "this"
                (TypeAlg.info TypeAlg.voidstar true true)
                loc
            )
            ge#mangler#vtable_field
            (TypeAlg.info TypeAlg.voidstar false true)
            loc
        )
        (
          builder#preop "&"
            (
              builder#id cvtbl
                (TypeAlg.info TypeAlg.voidstar false true)
                loc
            )
            (TypeAlg.info TypeAlg.voidstar false true)
            loc
        )
        (TypeAlg.info TypeAlg.voidstar true true)
        loc
    ) (TypeAlg.info TypeAlg.voidstar true true) loc

let replace_local ns ge new_id expression =
    let rewrite_id ns ge re =
      match !re with
        | Id b when b.cont = ([], "local") ->
            b.cont <- ([], new_id);
            true
        | _ -> false
    in
      ignore (
        AstRewriter.expr ns ge
          { AstRewriter.dont with AstRewriter.expr = Some rewrite_id; }
          AstRewriter.dont
          expression
      )

let prepare_constructor ?(deconly=false) ns ge box bd =
  let builder = new AstBuilder.nwbuilder ge in
  let fd =
    match bd.constructor with
      | Some fd
        -> fd
      | none
        ->
        {
          fname = box#get_name;
          fnspace = bd.bnspace;
          fparams =
            [([],builder#texpr TypeAlg.voidstar bd.bname.loc,"_this")];
          finline = false;
          frtype = builder#texpr box#btype bd.bname.loc;
          fbody = [];
          floc = bd.bname.loc;
          finfo = Some
            (TypeAlg.info Typing.base_cons_type false false);
          fexport = Asttools.is_box_exported ns bd;
          fasname = None;
        }
  in
  let _this = builder#id "_this"
    (TypeAlg.info TypeAlg.voidstar false true)
    fd.floc
  in
  let info = match fd.finfo with
    | Some i -> i
    | _ -> assert false
  in
  let return =
    let this = builder#id "this"
      (TypeAlg.info box#btype false true)
      fd.floc
    in
    builder#return ~e:this
      (TypeAlg.info box#btype true false)
      fd.floc
  in
  begin
    if not deconly then
      begin
        fd.finfo <- Some {info with TypeAlg.rewrited=false};
        fd.fbody <- List.fold_left (
          fun b -> function
            | SField {vname=f;
                      vinit = Some (SelfInit ({ cont=(t,args) } as bl));
                      vloc=loc}
              ->
              begin
                (if box#is_local f then
                    begin
                      let e = ref (gen_selfinit ns ge bl.loc f t args) in
                      builder#expr !e
                        (TypeAlg.info box#btype true false) loc
                    end
                 else assert false) :: b
              end
            | SField {vname=f;vinit = Some e;vloc=loc}
              ->
              let e = ref e in
              (if box#is_local f then
                  begin
                    replace_local ns ge ("&this->" ^ f) e;
                    builder#expr !e (TypeAlg.info box#btype true false) loc
                  end
               else
                  finit ns ge f !e loc) :: b
            | _ -> b
        ) fd.fbody bd.members;
        fd.fbody <- vtbl_init ns ge box fd.floc :: fd.fbody;
        fd.fbody <- extra_interfaces_init ns ge box fd.fbody fd.floc;
        fd.fbody <- semfun_init ns ge box fd.fbody fd.floc;
        builder#addfdvdecl "this" box#btype
          ?vinit:(Some _this)
          fd fd.floc;
        (ge#get_type_env ns)#set_current_box box#get_name;
        Field_access.in_method ns ge fd;
        Callrewrite.in_method ns ge fd;
        (ge#get_type_env ns)#leave_curent_box;
        fd.fbody <- fd.fbody @ [return];
      end;
    fd.fname <- box#get_init_name ge#mangler;
    fd.fexport <- Asttools.is_box_exported ns bd;
    bd.constructor <- Some fd
  end

let out_constructor ?(deconly=false) gen_fdecl fmt ns ge box bd =
  begin
    match bd.constructor with
      | Some fd ->
        gen_fdecl
          ?deconly:(Some deconly)
          ?meth:(Some false) fmt ge ns fd
      | _ -> assert false
  end

let prepare_destructor ?(deconly=false) ns ge box bd =
  match bd.destructor with
    | Some fd ->
      let builder = new AstBuilder.nwbuilder ge in
      let _this =
        builder#id
          "_this"
          (TypeAlg.info TypeAlg.voidstar false true)
          fd.floc
      in
      let this =
        builder#id
          "this"
          (TypeAlg.info box#btype false true)
          fd.floc
      in
      begin
        if not deconly then
          begin
            fd.fbody <- (
              builder#expr (
                builder#assignop "=" this _this
                  (TypeAlg.info box#btype true true) fd.floc
              ) (TypeAlg.info box#btype true true) fd.floc
            ) :: fd.fbody;
            builder#addfdvdecl
              "this"
              box#btype
              fd fd.floc;
            (ge#get_type_env ns)#set_current_box box#get_name;
            Field_access.in_method ns ge fd;
            Callrewrite.in_method ns ge fd;
            (ge#get_type_env ns)#leave_curent_box;
          end;
        fd.fname <- (box#get_del_name ge#mangler);
        fd.fexport <- Asttools.is_box_exported ns bd;
      end
    | None -> ()

let out_destructor ?(deconly=false) gen_fdecl fmt ns ge box bd =
  begin
    match bd.destructor with
      | Some fd ->
        gen_fdecl
          ?deconly:(Some deconly)
          ?meth:(Some false) fmt ge ns fd
      | _ -> ()
  end

let build ?(deconly=false) ?(forw=false) gen_fdecl fmt ns ge bd =
  let box = (ge#get_type_env ns)#get_class bd.bname.cont in
  let export = Asttools.is_box_exported ns bd in
  if export || not deconly then
    begin
      fpf fmt "@,@[<v>@[<h>/*** class %s ***/@]" box#get_name;
      if forw then
        build_td fmt ns ge box
      else
        begin
          if deconly = export then
            begin
              build_main_struct fmt ns ge box;
              gen_vtable_struct fmt ns ge box;
            end;
          if not deconly then
            begin
              prepare_methods ns ge box bd;
              build_methods gen_fdecl fmt ns ge box bd;
              List.iter
                (build_interface_vtbl fmt ns ge box)
                box#interfaces;
              if box#interfaces <> [] then
                build_method gen_fdecl fmt ns ge box
                  (build_semfun ns ge box);
            end;
          build_concrete_vtbl ~deconly export fmt ns ge box;
          prepare_constructor ~deconly ns ge box bd;
          prepare_destructor ~deconly ns ge box bd;
          out_constructor ~deconly gen_fdecl fmt ns ge box bd;
          out_destructor ~deconly gen_fdecl fmt ns ge box bd;
          fpf fmt "@,@[<h>/*** end class %s ***/@]@]@," box#get_name;
        end;
    end
