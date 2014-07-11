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

(* Typing *)

open Ast
exception NotACondition of location
exception InvalidBinOp of string * location
exception InvalidOperBinOp of string * location
exception InvalidUniOp of string * location
exception NotLeftValue of location
exception AssignMismatch of location
exception AssignMismatchExtended
  of location * TypeAlg.type_alg * TypeAlg.type_alg
exception NotAssignable of string * location
exception OpAssignMismatch of string * location
exception BranchTypeMismatch of location
exception NotAFunction of location
exception TooManyArg of location
exception NotEnoughArg of location
exception InvalidArg of int * location
exception NotAnArray of location
exception IndexNotInt of int * location
exception PointerNotArray of location
exception TooManyIndex of location
exception NotEnoughIndex of location
exception NotBoxed of location
exception NotPointerBoxed of location
exception NoSuchField of string * location
exception NoReturnExp of location
exception RetTypeMismatch of location
exception BreakOutside of location
exception ContinueOutside of location
exception UnboundId of string option * string * location
exception TypedefError of string * location
exception Cant_delete of location
exception Swtest_incompatible
  of TypeAlg.type_alg * TypeAlg.type_alg * location
exception Tag_conflict
  of string * location * string * TypeAlg.type_alg * location
exception Typename_not_exported of string * string * location
exception Symbol_not_exported of string * string * location
exception Bad_namespace of string option * location
exception Already_defined_symbol
  of string * location option * location
exception Redefine_field of string * location
exception Unknown_type_name of string * location
exception No_such_namespace of string * location
exception Method_interface_must_empty of location
exception Abstract_class_no_constructor of string * location
exception Abstract_class_no_instance of string * location
exception WrongFieldKind of location
exception Local_name_out_of_scope of location
exception Local_not_array_nor_obj of location
exception Local_not_dynarray of location
exception Macrobox_call_not_left_val of location

exception DEBUG_type_issue of TypeAlg.type_alg * location
exception DEBUG_generic_located_error of location

(* Helpers *)

let getop = function
  | Some x -> x
  | _ -> assert false

let opapply f = function
  | Some x -> Some (f x)
  | None -> None

let gen_info t c r = Some {
  TypeAlg.talg = t;
  TypeAlg.const = c;
  TypeAlg.rewrited = r;
  TypeAlg.need_conv = None;
}
let info t c = gen_info t c false
let stminfo t r = gen_info t true r

(* Specific checks *)

let chkReachable ns ge loc ret needret =
  if not (!needret || ret=None) then
    ge#emit_warning ns (Warn.StmUnreachable loc)

let chkAssign ns ge l op t1 t2 =
  try
    ge#operators#assign l ns op t1 t2
  with
    | _ -> raise (OpAssignMismatch (op,l))

let inject_conv e t0 t1 =
  match Asttools.expr_get_info e with
    | {TypeAlg.need_conv = None} as info ->
        info.TypeAlg.need_conv <- Some (t0,t1)
    | _ -> ()

let return_right_type t = function
  | Some r -> r := t
  | _ -> ()

let compatible ?(symetric=true) ?res ?(mode=Subtyping.Std)
    ns (ge:'ge) loc type0 type1 e0 e1 =
  let _chk i t0 t1 target =
    try
      (ge#subtyper:'ge Subtyping.subtyper)#is_subtype
        ~mode loc ns t0 t1
    with
      | Box.Need_conv store
      | Subtyping.Need_conv store
        ->
        begin
          (ge#debug:Debug_printer.printer)#locprint 1
            (Asttools.expr_get_loc target) ns
            "@[<b>Box need conversion (%t -> %t) for %t@]"
            (fun fmt -> TypeAlg.pp fmt t0)
            (fun fmt -> TypeAlg.pp fmt store)
            (fun fmt -> TypeAlg.pp fmt t1);
          inject_conv target (TypeAlg.unfold_mods t0) store;
          return_right_type (i,store) res;
          true
        end
  in
    try
      type0 = type1
      || (_chk 0 type0 type1 e0)
      || (symetric && _chk 1 type1 type0 e1)
    with
      | Subtyping.Need_downcast_obj 1
        ->
        begin
          inject_conv e0 (TypeAlg.unfold_mods type0) TypeAlg.objbase;
          return_right_type (0,TypeAlg.voidstar) res;
          true;
        end
      | Subtyping.Need_downcast_obj 2
        ->
        begin
          inject_conv e1 (TypeAlg.unfold_mods type1) TypeAlg.objbase;
          return_right_type (1,TypeAlg.voidstar) res;
          true;
        end
      | Subtyping.Need_downcast_obj _
        ->
        begin
          inject_conv e0 (TypeAlg.unfold_mods type0) TypeAlg.objbase;
          inject_conv e1 (TypeAlg.unfold_mods type1) TypeAlg.objbase;
          return_right_type (1,TypeAlg.voidstar) res;
          true;
        end

let chkFunCall loc floc ns ge tf targ =
  match tf with
    | TypeAlg.Fun (tp,tr, _) ->
      let rec tlistcmp n = function
        | ([],[]) ->tr
        | (_,[]) -> raise (TooManyArg loc)
        | ([],_) -> raise (NotEnoughArg loc)
        | ((arg,la,e)::al,param::pl) ->
          begin
            if arg = param
            || compatible ns ge loc arg param e e
            then
              tlistcmp (n+1) (al,pl)
            else raise (InvalidArg (n,la))
          end
      in tlistcmp 1 (targ,tp)
    | _ -> raise (NotAFunction floc)

let chkArrayIndex loc aloc ns ge ta tindex =
  let chkIndex i dloc = function
    | TypeAlg.Int _ -> ()
    | _ -> raise (IndexNotInt (i,dloc))
  in
    match ta with
      | TypeAlg.Pointer (t,_) ->
        begin
          match tindex with
            | (i,dloc)::[] -> chkIndex 1 dloc i; t
            | _ -> raise (PointerNotArray loc)
        end
      | TypeAlg.Array (t,dims,_) ->
        let rec chk n = function
	  | ([],[]) -> t
	  | (_,[]) -> raise (TooManyIndex loc)
	  | ([],_) -> raise (NotEnoughIndex loc)
	  | ((i,dloc)::ri,_::rd) ->
            chkIndex n dloc i; chk (n+1) (ri,rd)
        in chk 1 (tindex,dims)
      | TypeAlg.Int (true, _, m) ->
        begin
          match tindex with
            | (i,dloc)::[] ->
              chkIndex 1 dloc i;
              TypeAlg.Int
                (true,1L,
                 {TypeAlg.modifiers = m.TypeAlg.modifiers}
                )
            | _ -> raise (PointerNotArray loc)
        end
      | _ -> raise (NotAnArray aloc)

(* If on a macrobox, we want left value only *)
let chkMacrobox ge ns fe =
  let is_macrobox ge block =
    match TypeAlg.unfold_mods (TypeAlg.typeof block) with
      | TypeAlg.NSTypeName (namespace, name, _) ->
          begin try
            (ge#get_type_env namespace)#get_macroclass name;
            true;
          with
            | Types.No_such_macroclass _ -> false
          end
      | _ -> false
  in
    match fe with
      | Field ({cont=(block,_); loc=location}) when is_macrobox ge block ->
          if TypeAlg.is_left_val ns ge block then ()
          else raise (Macrobox_call_not_left_val location);
      | _ -> ()

let needLeft = function
  | "++" | "--" -> true
  | _ -> false

(* AST typing  *)

let macro_function ns ge sym b t =
  begin
    if sym#const then
      begin
        (ge#debug:Debug_printer.printer)#locprint 4 b.loc ns
          "found %s as macro function" sym#get_srcname ;
        let called, args = b.cont in
        let label =
          Id ({
            loc = b.loc;
            cont = ([], Formacro.build_call_point b.loc);
            info = Some (TypeAlg.info TypeAlg.Error true true);
          }) in
        b.cont <- (called, label::args);
        b.info <- (gen_info t true true)
      end
  end

(* expressions *)
let rec expr ns ge e =
  (ge#get_type_env ns)#canon (_expr ns ge e)
and _expr ns ge = function

  | Value b ->
      let t = TypeAlg.value ge b.cont in
        b.info <- info t true;
        t

  | Id ({cont=(lns,x)} as b)
    ->
    let ons = match lns with
      | [] -> None
      | ns::_ -> Some ns
    in
    let nse = match lns with
      | [] -> ns
      | n::_ -> n
    in
    let sym =
      try (ge#get_sym_env nse)#get x with
        | Asttools.Namespace_not_found n
          -> raise (No_such_namespace (n,b.loc))
        | Symbol.No_such_symbol x
          ->
          begin
            try
              ignore((ge#get_type_env ns)#get_class x);
              raise (Abstract_class_no_instance (x,b.loc))
            with
              | Types.No_such_class _
                -> raise (UnboundId (ons,x,b.loc))
          end
    in
    let symname =
      try ((ge#get_type_env ns)#get_box x)#get_name with
        | _ -> sym#get_c_name
    in
    b.info <- info sym#get_type sym#const;
    b.cont <- (lns,symname);
    ge#encounter ns sym#get_srcname;
    sym#get_type

  | BinOp ({cont=(op,e0,e1)} as b) ->
      begin
        try
          let t =
            let t0 = expr ns ge e0 in
            let t1 = expr ns ge e1 in
            let res = ref (0,t0) in
            let _ =
              let mode = Subtyping.op_cases op in
                compatible ~mode:mode ~res:res
                  ns ge b.loc t0 t1 e0 e1
            in
            let (t0,t1) = match fst !res with
              | 0 -> (snd !res, t1)
              | _ -> (t0, snd !res)
            in
              ge#operators#bin b.loc ns op t0 t1
          in
            b.info <- info t true; t
        with
          | Operator.OperandFailed 2
            -> raise (InvalidBinOp (op,b.loc))
          | Operator.OperandFailed 0
            -> raise (InvalidBinOp (op,Asttools.expr_get_loc e0))
          | Operator.OperandFailed 1
            -> raise (InvalidBinOp (op,Asttools.expr_get_loc e1))
      end

  (* TODO: property need support for op-assign operator *)
  | PreOp ({cont=(op,e)} as b)
  | PostOp ({cont=(op,e)} as b) ->
    begin
      let te = expr ns ge e in
      try
        let t =
          if (not (needLeft op)) || (TypeAlg.is_left_val ns ge e) then
            ge#operators#uni b.loc ns op te
          else raise (NotLeftValue (Asttools.expr_get_loc e))
        in
        b.info <- info t true;
        t
      with
        | Box.Need_conv store
          ->
          begin
            inject_conv e (TypeAlg.unfold_mods te) store;
            b.info <- info store true;
            store
          end
        | Operator.OperandFailed _
          -> raise (InvalidUniOp (op,b.loc))
    end

  | AssignOp ({cont=("=",lhs,rhs)} as b) ->
    begin
      let tlhs = expr ns ge lhs in
      let trhs =
        let res = ref (0,tlhs) in
        let trhs = expr ns ge rhs in
        let _ = (compatible ~res:res ns ge b.loc trhs tlhs rhs lhs) in
        match !res with
          | (0,t) -> t
          | _ -> trhs
      in
      let t =
        try
          if not (TypeAlg.is_left_val ns ge lhs) then
            raise (NotLeftValue (Asttools.expr_get_loc lhs));
          if (ge#get_type_env ns)#chkassign b.loc tlhs trhs then trhs
          else raise (AssignMismatchExtended (b.loc,tlhs,trhs))
        with
          (* TODO: do we fall here ? *)
          | Box.Need_conv t
            -> inject_conv lhs tlhs t; trhs
      in
      b.info <- info t true; t
    end

  | AssignOp ({cont=(op,lhs,rhs)} as b)
    ->
    begin
      let tlhs = expr ns ge lhs in
      let trhs = expr ns ge rhs in
      let t =
        if not (TypeAlg.is_left_val ns ge lhs) then
          raise (NotLeftValue (Asttools.expr_get_loc lhs));
        if ((ge#subtyper:'ge Subtyping.subtyper)#is_subtype
               ~mode:(Subtyping.op_cases op) b.loc ns trhs tlhs)
        then chkAssign ns ge b.loc op tlhs trhs
        else raise (AssignMismatch b.loc)
      in
      b.info <- info t true;
      t
    end

  | TerOp ({cont=(e0,e1,e2)} as b)
    ->
    begin
      let t0 = expr ns ge e0 in
      let t1 = expr ns ge e1 in
      let t2 = expr ns ge e2 in
      if not ((ge#get_type_env ns)#is_condition t0) then
        raise (NotACondition (Asttools.expr_get_loc e0));
      let t =
        let rt = ref (1,t1) in
        if t1 = t2 || compatible ~res:rt ns ge b.loc t1 t2 e1 e2 then snd (!rt)
        else raise (BranchTypeMismatch b.loc)
      in
      b.info <- info t true;
      t
    end

  (* TODO: Possible position for clever warning ... *)
  | Cast ({cont=(t,e)} as b)
    ->
    begin
      let t = texpr ns ge t in
      ignore (expr ns ge e);
      b.info <- info t (not (TypeAlg.is_left_val ns ge e));
      t
    end

  | Sizeof ({cont=t} as b)
    ->
    begin
      ignore (texpr ns ge t);
      b.info <- info TypeAlg.uintDef true;
      TypeAlg.uintDef
    end

  | Call ({cont=(fe,al)} as b)
    ->
    begin
      let chkConst =
        match fe with
          | Id {cont=(ns', fname)}
            ->
            begin
              try
                let ns' = match ns' with
                  | [] -> ns | n::_ -> n
                in
                Some ((ge#get_sym_env ns')#get fname)
              with
                | Symbol.No_such_symbol _
                | Asttools.Namespace_not_found _ -> None
            end
          | _ -> None
      in
      let tf = expr ns ge fe in
      chkMacrobox ge ns fe;
      let ta =
        List.map
          (fun e -> (expr ns ge e,Asttools.expr_get_loc e,e) ) al
      in
      let t =
        chkFunCall b.loc (Asttools.expr_get_loc fe)
          ns ge tf ta
      in
      b.info <- info t true;
      begin
        match chkConst with
          | Some sym
            -> macro_function ns ge sym b t
          | _ -> ()
      end;
      t
    end

  | Index ({cont=(e,dl)} as b)
    ->
    begin
      let ta = expr ns ge e in
      let il =
        List.map (
          fun e -> (expr ns ge e, Asttools.expr_get_loc e)
        ) dl
      in
      let t = chkArrayIndex b.loc (Asttools.expr_get_loc e) ns ge ta il in
      b.info <- info t false;
      t
    end

  | Field ({cont=(e,f)} as b)
    -> field ns ge e f b false

  | PField ({cont=(e,f)} as b)
    -> field ns ge e f b true

  | Decl ({cont=vd} as b)
    ->
    begin
      let t = vdecl ns ge vd in
      b.info <- info t true;
      t
    end

  | Compound ({cont=sl} as b)
    ->
    begin
      let rec chk = function
        | [] -> assert false
        | h::[]
          ->
          begin
            statement false false None (ref false) ns ge h;
            TypeAlg.typeof_stm h;
          end
        | h::t
          ->
          begin
            statement false false None (ref false) ns ge h;
            chk t;
          end
      in
      let t = chk sl in
      begin
        match b.info with
          | None -> b.info <- info t true
          | Some _ -> ()
      end;
      t
    end

  | SelfInit ({ cont=(t,arg) } as b)
    ->
    begin
      let t = self_init ns ge b.loc t arg in
      b.info <- info t true; t
    end

and field ns ge e f b isderef =
  begin
    let tb =
      if not isderef then expr ns ge e
      else
        match expr ns ge e with
          | TypeAlg.Pointer (t,_) -> t
          | _ -> raise (NotPointerBoxed (Asttools.expr_get_loc e))
    in
    let rec t tb =
      try
        ((ge#get_type_env ns)#get_box_from_type tb)#get_field_type f
      with
        | Box.Macro_no_such_field_use_store tb
          -> t tb
        | Types.Not_boxed
          -> raise (NotBoxed (Asttools.expr_get_loc e))
        | Box.No_such_field f
          -> raise (NoSuchField (f,b.loc))
    in
    let t = t tb in
    b.info <- info t false;
    t
  end

(* direct init of local object *)
and self_init ns ge loc t args =
  begin
    let tn =
      can_local ns ge (Asttools.texpr_get_loc t) (texpr ns ge t)
    in
    let id = Id { loc=loc; cont=([],tn); info=None; } in
    let tf = expr ns ge id in
    let ta =
      (TypeAlg.voidstar,loc,id) :: List.map
        (fun e -> (expr ns ge e,Asttools.expr_get_loc e,e) ) args
    in
    chkFunCall loc loc ns ge tf ta
  end

(* Type expressions *)
and texpr ns ge = function
  | Void b
    -> b.info <- info TypeAlg.Void true; TypeAlg.Void

  | (TName b) as t
    ->
    begin
      try
        let ta = (ge#get_type_env ns)#canon (TypeAlg.fromTExpr t) in
        b.info <- info ta true; ta
      with
        | Asttools.Namespace_not_found n
          -> raise (No_such_namespace (n,b.loc))
        | Types.Typename_not_exported (ns, t)
          -> raise (Typename_not_exported (ns, t, b.loc))
        | Types.No_such_typename n
          -> raise (Unknown_type_name (n,b.loc))
      end

  | (Num ({cont=(_,e)} as b)) as t
    ->
    begin
      ignore (expr ns ge e);
      let ta =
        try
          (ge#get_type_env ns)#canon (TypeAlg.fromTExpr t)
        with
          | Types.Typename_not_exported (ns, t)
            -> raise (Typename_not_exported (ns, t, b.loc))
      in
      b.info <- info ta true; ta
    end

  | (TFloat ({cont=e} as b)) as t
    ->
    begin
      ignore (expr ns ge e);
      let ta =
        try
          (ge#get_type_env ns)#canon (TypeAlg.fromTExpr t)
        with
            | Asttools.Namespace_not_found n
              -> raise (No_such_namespace (n,b.loc))
            | Types.Typename_not_exported (ns, t)
              -> raise (Typename_not_exported (ns, t, b.loc))
      in
      b.info <- info ta true; ta
    end

  | TChar b
    ->
    let ta = TypeAlg.Char (TypeAlg.stdmeta ())in
    b.info <- info ta true; ta

  | TPointer ({cont=tp} as b)
    ->
    let ta = TypeAlg.Pointer (texpr ns ge tp, TypeAlg.stdmeta ()) in
    b.info <- info ta true; ta

  | (Array ({cont=(tp,el)} as b))
    ->
    begin
      List.iter (fun t -> ignore (expr ns ge t)) el;
      let canon_tpa =
        try
          (ge#get_type_env ns)#canon (texpr ns ge tp)
        with
          | Types.Typename_not_exported (ns, t)
            -> raise (Typename_not_exported (ns, t, b.loc))
      in
      let ta =
        TypeAlg.Array (canon_tpa, List.map TypeAlg.cst_eval el,
                       TypeAlg.stdmeta ())
      in b.info <- info ta true; ta
    end

  | Fun ({cont=(tl,rt)} as b)
    ->
    let ta =
      TypeAlg.Fun
        (List.map (texpr ns ge) tl,texpr ns ge rt, TypeAlg.stdmeta ())
    in b.info <- info ta true; ta

  | TModifier ({ cont=(m,t) } as b)
    ->
    let ta =
      TypeAlg.Modifiers (
        texpr ns ge t,
        { TypeAlg.modifiers = [m]; }
      )
    in
    b.info <- info ta true; ta

(*
 **** REFACTOR NEEDED *****
 * Refactor done
 * REGRESSION DETECTED: new unwanted error arise.
 * NO MORE REGRESSION ? does comment above still true ?
 *)

and can_local ns ge loc = function
  | TypeAlg.Pointer _
  | TypeAlg.Array (_, [],_)
    -> raise (Local_not_dynarray loc)
  | TypeAlg.Array (t, _, _) | t
    ->
    begin
      try (ge#get_type_env ns)#check_obj_type t with
        | Not_found -> raise (Local_not_array_nor_obj loc)
    end

and decl_can_local ns ge vd t =
  if List.mem "local" vd.vmods then
    ignore (can_local ns ge vd.vloc t)

(* TODO: MORE REFACTOR TO DO HERE *)

and chk_define_conflict ?(deconly=false) ?(global=false) sym_env name loc =
  begin
    if global && deconly then
      begin
        try
          raise
            (Already_defined_symbol
               (name, (sym_env#get name)#get_location, loc))
        with
          | Symbol.No_such_symbol _ -> ()
      end;
  end

(* Variable declaration *)
and vdecl ?(asname) ?(deconly=false) ?(global=false) ?(field=false) ns ge vd =
  let exp = global && not (List.mem "static" vd.vmods) in
  let sym_env = ge#get_sym_env ns in
  chk_define_conflict ~deconly ~global sym_env vd.vname vd.vloc;
  let (sym,t) =
    if ((global || field) && not deconly) then
      let s = sym_env#get vd.vname in (s,s#get_type)
    else
      begin
        let t = texpr ns ge vd.vtype in
        (Symbol.factory ?asname ns vd.vname exp t (Some vd.vloc), t)
      end
  in
  begin
    if exp && not field then
      begin
        (ge#debug:Debug_printer.printer)#print 4
          "exporting symbol %S" vd.vname;
        sym#export;
      end;
    if (field) then
      sym_env#add_nomangling vd.vname sym
    else
      if global = deconly then
        sym_env#add vd.vname sym;
    decl_can_local ns ge vd t;
    if not (deconly || vd.vinit = None) then
      begin
        let rhs = getop vd.vinit in
        let trhs = expr ns ge rhs in
          (ge#debug:Debug_printer.printer)#locprint 1 vd.vloc ns
            "Typing decl init: %t %t"
            (fun fmt -> TypeAlg.pp fmt t) (fun fmt -> TypeAlg.pp fmt trhs);
          if not (compatible ns ge vd.vloc trhs t rhs rhs) then
            raise (AssignMismatch vd.vloc);
        (* TODO: check why we use old-fashion compatible
        if not ((ge#get_type_env ns)#compatible vd.vloc t trhs) then
           raise (AssignMismatch vd.vloc); *)
      end;
    if not (exp || sym_env#local) then
      Asttools.add_vmod "static" vd;
    vd.vinfo <- Some (TypeAlg.info t true false);
    if not global then
      vd.vname <- sym#get_c_name;
    if global && not deconly then
      ge#declare ns sym#get_srcname;
    t
  end

(* Statements typing *)

and chk_expr_condition ns ge e =
  if not ((ge#get_type_env ns)#is_condition (expr ns ge e)) then
    raise (NotACondition (Asttools.expr_get_loc e))
  else ()

and chkRet loc ns ge rt eopt =
  let t = opapply (expr ns ge) eopt in
  match (rt,t) with
    | ((None | Some TypeAlg.Void), None)
      -> TypeAlg.Void
    | (None,_)
      -> raise (NoReturnExp loc)
    | (_,None)
      -> raise (RetTypeMismatch loc)
    | (Some rt, Some t)
      ->
      begin
        let e = match eopt with Some e -> e | _ -> assert false in
        if not (compatible ns ge loc t rt e e) then
          raise (RetTypeMismatch loc);
        rt
      end

and chk_opt_expr ns ge = function
  | Some e
    -> ignore (expr ns ge e)
  | _ -> ()

and statement breakable loop ret needret ns ge stm =
  begin
    chkReachable ns ge (Asttools.stm_get_loc stm) ret needret;
    match stm with
      | Delete ({cont=e} as b)
        ->
        let t = expr ns ge e in
        begin
          try
            let box = ((ge#get_type_env ns)#get_box_from_type t) in
            if not box#can_delete then
              raise (Cant_delete b.loc)
          with
            | _ -> raise (Cant_delete b.loc)
        end;
        b.info <- stminfo t false

      | EmptyStatement b
        -> b.info <- stminfo TypeAlg.Void false

      | Expr ({cont=e} as b)
        -> b.info <- stminfo (expr ns ge e) false

      | Block ({cont=sl} as b)
        ->
        begin
          (ge#get_sym_env ns)#enter_context ();
          List.iter (statement breakable loop ret needret ns ge) sl;
          (ge#get_sym_env ns)#leave_context;
          b.info <- stminfo TypeAlg.Void false
        end

      | Return ({cont=eopt} as b)
        ->
        begin
          needret := false;
          b.info <- stminfo (chkRet b.loc ns ge ret eopt) false
        end

      | Break b
        ->
        begin
          if not (breakable || loop) then raise (BreakOutside b.loc);
          b.info <- stminfo TypeAlg.Void false
        end

      | Continue b
        ->
        begin
          if not loop then raise (ContinueOutside b.loc);
          b.info <- stminfo TypeAlg.Void false
        end

      | If ({cont=(e,s1,s2)} as b)
        ->
        begin
          let nr1 = ref !needret in
          let nr2 = ref !needret in
          chk_expr_condition ns ge e;
          statement breakable loop ret nr1 ns ge s1;
          statement breakable loop ret nr2 ns ge s2;
          needret := !nr1 || !nr2;
          b.info <- stminfo TypeAlg.Void false
        end

      | While ({cont=(e,s)} as b)
      | Do ({cont=(e,s)} as b)
        ->
        begin
          chk_expr_condition ns ge e;
          statement true true ret needret ns ge s;
          b.info <- stminfo TypeAlg.Void false
        end

      | For ({cont=(e0,e1,e2,s)} as b)
        ->
        begin
          (ge#get_sym_env ns)#enter_context ();
          List.iter (chk_opt_expr ns ge) [e0;e1;e2];
          statement true true ret needret ns ge s;
          (ge#get_sym_env ns)#leave_context;
          b.info <- stminfo TypeAlg.Void false;
        end

      | Goto b
      | Label b
        -> b.info <- stminfo TypeAlg.Void false

      | Switch ({cont=(e,case_list)} as b)
        ->
        begin
          List.iter (case ns ge loop ret needret (expr ns ge e)) case_list;
          b.info <- stminfo TypeAlg.Void false;
        end

      | Asm ({cont=asm} as b)
        ->
        begin
          asmblock ns ge asm;
          b.info <- stminfo TypeAlg.Void false;
        end
  end


(* ASM bloc *)
and asmblock ns ge asm =
  begin
    List.iter (
      fun {asmexpr = e} ->
        ignore (expr ns ge e)
    ) asm.asminputs;
    List.iter (
      function {asmexpr = e} ->
        ignore(expr ns ge e);
        if not (TypeAlg.is_left_val ns ge e) then
          raise (NotLeftValue (Asttools.expr_get_loc e));
    ) asm.asmouputs;
    asm.asminfo <- stminfo TypeAlg.Void false;
  end

(* TODO: why switches use old-fashion compatible ? *)
and swtest ns ge t = function
  | SwValue ({cont=v} as b) ->
      let t' = TypeAlg.value ge v in
        if not ((ge#get_type_env ns)#compatible b.loc t t') then
          raise (Swtest_incompatible (t,t',b.loc));
        b.info <- stminfo t false
  | Default b ->
      b.info <- stminfo t false

and case ns ge loop ret needret t (sw,sl) =
  begin
    swtest ns ge t sw;
    List.iter (statement true loop ret (ref true) ns ge) sl;
  end

(*
 * Function declarations typing
 *)

let add_parameter ?(mangle=true) ns ge (name,t) =
  (
    if mangle then (ge#get_sym_env ns)#add
    else (ge#get_sym_env ns)#add_nomangling
  ) name (Symbol.factory ns name false t None)

let buildFunType ns ge f =
  let rt = texpr ns ge f.frtype in
  let ptl =
    List.map
      (function (_,t,x) ->
        (x,(texpr ns ge t))
      ) f.fparams
  in
  let (_,tl) = List.split ptl in
    (TypeAlg.Fun (tl,rt, TypeAlg.stdmeta ()), ptl,rt)

let fbody ?(field=false) ?(ctx) ?(macro=false) ns ge f rt ptl =
  begin
    let symenv = ge#get_sym_env ns in
    let needret = ref (
      match rt with TypeAlg.Void -> false | _ -> true
    ) in
    let ret = if !needret then Some rt else None in
    begin
      match ctx with
        | None
          ->
          if macro then symenv#enter_context_suffix "_macro" ()
          else symenv#enter_context ();
        | Some c
          -> symenv#restore_context c;
    end;
    begin
      match ptl with
        | [] -> ()
        | (x,t)::other_params
          ->
          begin
            add_parameter
              ~mangle:(not (field && x = "_this")) ns ge (x,t);
            List.iter (add_parameter ns ge) other_params;
          end
    end;
    f.fparams <- List.map (
      fun (m,t,x) ->
        (m,t,(symenv#get x)#get_c_name)
    ) f.fparams;
    List.iter (statement false false ret needret ns ge) f.fbody;
    symenv#leave_context;
  end

let rec fdecl
    ?(deconly=false) ?(field=false) ?(macro=false) ?(ctx) ns ge f =
  begin
    let sym_env = ge#get_sym_env ns in
    if not deconly then
      begin
        let (ptl,rt) =
          match (sym_env#get f.fname)#get_type with
            | TypeAlg.Fun (p,r,_)
              ->
              (List.map2 (
                function (_,_,x) ->
                  fun t -> (x,t)
               ) f.fparams p, r)
            | _ -> assert false
        in
        (ge#debug: Debug_printer.printer)#print 4
          "typing: checking function %s (second pass)" f.fname;
        (ge#debug: Debug_printer.printer)#locprint 4 f.floc ns
          "symbol: %s (%s)" (sym_env#get f.fname)#get_srcname
          (sym_env#get f.fname)#get_c_name;
        if not field then ge#declare ns f.fname;
        fbody ~macro ~field ?ctx ns ge f rt ptl;
      end;
    if deconly then
      if Entry_point.is_entry f then
        try
          Entry_point.entry_check ns ge (texpr ns ge) buildFunType f
        with
          | Entry_point.Bad_namespace (x,y) -> raise (Bad_namespace (x,y))
      else
        begin
          begin
            try
              raise (
                Already_defined_symbol
                  (f.fname, (sym_env#get f.fname)#get_location, f.floc)
              );
            with Symbol.No_such_symbol _ ->
              (ge#debug: Debug_printer.printer)#print 4
                "typing: checking function %s (first pass)" f.fname
          end;
          let exp = f.fexport && not (f.finline || f.fasname <> None) in
          let _ =
            match f.fnspace with
              | [] -> true
              | n::_ when n = ns -> true
              | n::_ -> raise (Bad_namespace (Some n, f.floc))
          in
          let (ft, ptl,rt) = buildFunType ns ge f in
          let symbol = Symbol.factory
            ?asname:f.fasname ns f.fname exp ft (Some f.floc)
          in
          if exp && f.finline then
            ge#emit_warning ns (Warn.Exported_inline f.floc);
          if macro then symbol#make_const;
          sym_env#add f.fname symbol;
          f.finfo <- Some (TypeAlg.info ft true false);
          ft;
        end
    else (sym_env#get f.fname)#get_type
  end

let mktname box = TypeAlg.TypeName (box#get_name, TypeAlg.stdmeta ())

let chkcstexpr loc = function
  | Some e
    ->
    begin
      try
        ignore (TypeAlg.strict_cst (TypeAlg.cst_eval e))
      with
        | Not_found -> raise (TypeAlg.Expr_not_constant loc)
    end
  | _ -> ()

let member ?(deconly=false) ns ge builder box bd m =
  let inter =
    match bd with Interface _ -> true | _ -> false
  in
  let aux = function
    | Method f
      ->
      let symenv = ge#get_sym_env ns in
      begin
        symenv#enter_context ();
        if (not inter) then
          symenv#add_nomangling
            "this"
            (Symbol.factory ns "this" false (mktname box) (Some f.floc))
        else
          if f.fbody <> [] then raise (Method_interface_must_empty f.floc);
        let ft = fdecl ~deconly ~field:true ns ge f in
        if deconly then
          begin
            builder#add_method f.fname ft;
            if f.fbody = [] then builder#make_abstract f.fname;
          end;
        symenv#leave_context
      end
    | SField vd
      ->
      begin
        let st = vdecl ~deconly ~field:true ns ge vd in
        if deconly then
          begin
            let _ = try builder#add vd.vname st with
              | Box.Redefine_field s
                -> raise (Redefine_field (s,vd.vloc) ) in
            if List.mem "local" vd.vmods then
              box#make_local vd.vname
          end
      end
    | EField ({cont=(x,eopt)} as b)
      ->
      if deconly then
        begin
          chkcstexpr b.loc eopt;
          builder#add x TypeAlg.intDef;
          try ge#tagenv#register_tag x box#btype b.loc ns with
            | Enum.Tag_conflict (_,n,t,l)
              -> raise (Tag_conflict (x,l,n,t,l))
        end
  in
  begin
    match (m,bd) with
      | (Method _, (Class _| Interface _))
      | (SField _, (Class _ | Struct _ | Union _))
      | (EField _, Enum _)
        -> aux m
      | _
        -> raise (WrongFieldKind (Asttools.member_get_loc m))
  end

let base_cons_type =
  TypeAlg.Fun ([TypeAlg.voidstar], TypeAlg.voidstar, TypeAlg.stdmeta ())
let base_del_type =
  TypeAlg.Fun ([TypeAlg.Void],TypeAlg.Void, TypeAlg.stdmeta ())

let voidstar loc =
  TPointer {
    cont =
      Void {
        cont = ();
        loc=loc;
        info = None;
      };
    loc = loc;
    info = None;
  }

let constructor ?(deconly=false) ns ge box inb =
  match inb.constructor with
    | Some fd
      ->
      if box#box_is_abstract then
        raise (Abstract_class_no_constructor (box#get_name,box#get_loc));
      if deconly then
        begin
          fd.fname <- box#get_name;
          fd.fnspace <- inb.bnspace;
          fd.fparams <-
            ([],voidstar fd.floc,"_this")::fd.fparams;
          fd.frtype <- voidstar inb.bname.loc;
        end;
      ignore (
        fdecl ~deconly ~ctx:box#get_name ~field:true ns ge fd
      );
    | None
      ->
      begin
        if (not box#box_is_abstract) && deconly then
          ignore (
            fdecl ~deconly ~ctx:box#get_name ~field:true ns ge
              {
                fname = box#get_name;
                fnspace = inb.bnspace;
                fparams = [([],voidstar inb.bname.loc,"_this")];
                finline = false;
                frtype = voidstar inb.bname.loc;
                fbody = [];
                floc = inb.bname.loc;
                finfo = Some(TypeAlg.info base_cons_type true false);
                fexport = true;
                fasname = None;
              }
          )
      end

let destructor ?(deconly=false) ns ge box inb =
  match inb.destructor with
    | Some fd
      ->
      if box#box_is_abstract then
        raise (Abstract_class_no_constructor (box#get_name,box#get_loc));
      if deconly then
        begin
          fd.fname <- "~" ^ box#get_name;
          fd.fnspace <- inb.bnspace;
          fd.fparams <-
            ([],voidstar fd.floc,"_this")::[];
        end;
      ignore (
        fdecl ~deconly ~ctx:box#get_name ~field:true ns ge fd
      )
    | None
      ->
      begin
        if (not box#box_is_abstract) && deconly then
          ignore (
            fdecl ~deconly ~ctx:box#get_name ~field:true ns ge
              {
                fname = "~" ^ box#get_name;
                fnspace = inb.bnspace;
                fparams = [([],voidstar inb.bname.loc,"_this")];
                finline = false;
                frtype = Void {cont=();loc=inb.bname.loc;info=None};
                fbody = [];
                floc = inb.bname.loc;
                finfo = Some (TypeAlg.info base_del_type true false);
                fexport = true;
                fasname = None
              }
          )
      end

let add_cons_del ~deconly ns ge box = function
  | Class {cont=ib}
    ->
    begin
      constructor ~deconly ns ge box ib;
      destructor ~deconly ns ge box ib
    end
  | _ -> ()

let check_implem ns ge = function
  | Class {cont=ib}
    ->
    let box = (ge#get_type_env ns)#get_class ib.bname.cont in
      begin
        List.iter (
          fun (ns',i) ->
            ((ge#get_type_env ns')#get_interface i)#check_implem box
        ) ib.interf;
      end
  | _ -> ()

let struct_kind ns ge lb ib b =
  begin
    let exp = Asttools.is_box_exported ns ib in
    let builder = (ge#get_type_env ns)#builder in
    let syms = ge#get_sym_env ns in
    let box =
      begin
        builder#start exp b;
        builder#get_box;
      end
    in
    syms#enter_context ();
    List.iter
      (member ~deconly:true ns ge builder box b) ib.members;
    lb.info <- Some (TypeAlg.info (box#btype) true false);
    builder#finalize;
    syms#leave_context;
  end

let bdecl ?(deconly=false) ns ge = function
  | (Struct ({cont=(_,ib)} as lb)) as b
    ->
    begin
      begin
        match ib.bnspace with
          | n::_ when ns <> n
              -> raise (Bad_namespace (Some n, lb.loc))
          | _ -> ()
      end;
      if deconly then struct_kind ns ge lb ib b
    end
  | (Union lb | Enum lb ) as b
    ->
    begin
      begin
        match lb.cont.bnspace with
          | n::_ when ns <> n
              -> raise (Bad_namespace (Some n, lb.loc))
          | _ -> ()
      end;
      if deconly then struct_kind ns ge lb lb.cont b
    end
  | (Interface lb) as b
  | ((Class lb) as b)
    ->
    begin
      begin
        Asttools.clean_supports_namespace ns lb.cont;
        match lb.cont.bnspace with
          | n::_ when ns <> n
              -> raise (Bad_namespace (Some n, lb.loc))
          | _ -> ()
      end;
      let builder = (ge#get_type_env ns)#builder in
      let syms = ge#get_sym_env ns in
      let box =
        try
          if not deconly then (ge#get_type_env ns)#get_box lb.cont.bname.cont
          else
            begin
              builder#start true b;
              builder#get_box;
            end
        with
          | Types.No_such_box _ -> assert false
      in
      syms#restore_context box#get_name;
      box#iter_field (
        fun f t ->
          syms#add_nomangling f
            (Symbol.factory ns f false t None)
      );
      ge#debug#print 4
        "typing: checking members of class %s:" box#get_name;
      List.iter
        (member ~deconly ns ge builder box b) lb.cont.members;
      if deconly then
        begin
          lb.info <- Some (TypeAlg.info box#btype true false);
          builder#finalize;
          check_implem ns ge b;
        end;
      syms#add_nomangling
        "this"
        (Symbol.factory ns "this" false (mktname box)
           (Some lb.loc)) ;
      syms#save_context box#get_name;
      add_cons_del ~deconly ns ge box b;
    end

let mclassdecl ?(deconly=false) ns ge mc =
  begin
    let exp = match mc.mcnspace with
      | [] -> true
      | n::_ when n = ns -> true
      | n::_ -> raise (Bad_namespace (Some n, mc.mcloc))
    in
    let builder = (ge#get_type_env ns)#builder in
    let _ = texpr ns ge mc.store in
    let syms = ge#get_sym_env ns in
    let add_op mbox f =
      begin
        syms#enter_context ();
        let ft = fdecl ~deconly ns ge f in
        if deconly then
          builder#add f.fname ft;
        syms#leave_context;
      end
    in
    let box =
      try
        if not deconly then
          (ge#get_type_env ns)#get_macroclass mc.cmacro.cont
        else
          builder#start_macro exp mc
      with
        | Types.No_such_box _ -> assert false
    in
    box#export ns;
    syms#enter_context ();
    box#iter_field (
      fun f t ->
        syms#add_nomangling f (Symbol.factory ns f false t None)
    );
    syms#add_nomangling "this"
      (Symbol.factory ns "this" false box#get_store None);
    List.iter (add_op box) mc.mc_ops;
    syms#leave_context;
    builder#finalize;
  end

let typedef ?(deconly=false) ns ge td =
  begin
    td.tdnspace <- [ns];
    let t = texpr ns ge td.tdtype in
      if (t = TypeAlg.Error) then
        raise (TypedefError (td.tdname,td.tdloc));
      td.tdinfo <- Some (TypeAlg.info t true false);
      t
  end

let check_propop ?(deconly=false) ns ge box op =
  let syms = ge#get_sym_env ns in
  let builder = (ge#get_type_env ns)#builder in
    begin
      syms#enter_context ();
      let ft = fdecl ~deconly ns ge op in
        if deconly then
          builder#add op.fname ft;
        box#set_op op;
        syms#leave_context;
    end

let property ?(deconly=false) ns ge prop =
  let syms = ge#get_sym_env ns in
  let builder = (ge#get_type_env ns)#builder in
    begin
      let store = texpr ns ge prop.properstore in
      let box =
        if not deconly then
          (ge#get_type_env ns)#get_property prop.pname
        else
          builder#start_property
            prop.pname prop.properloc store
            (texpr ns ge prop.properthis)
      in
        box#export ns;
        if deconly then
          builder#add "this" (texpr ns ge prop.properthis);
        syms#enter_context ();
        box#iter_field (
          fun f t ->
            syms#add_nomangling f (Symbol.factory ns f false t None)
        );
        check_propop ~deconly ns ge box prop.getter;
        check_propop ~deconly ns ge box prop.setter;
        syms#leave_context;
        builder#finalize;
    end

(* dummy test code *)
let mapper ?(deconly=false) ns ge map =
  let mapbox = new Mapper.property_group ns ge "mapper" fdecl in
    mapbox#set_tvar "T"

(* dummy test code *)
let unused ns ge = new Subtyping.subtyper ge

