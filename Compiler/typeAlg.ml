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

(* Type Algebra *)

exception Expr_not_constant of Ast.location

type cst_expr =
  | I of int64
  | BinOp of string * cst_expr * cst_expr
  | PreOp of string * cst_expr
  | Sizeof of type_alg
  | CstVar of string

and type_alg =
  | Int of bool * int64 * meta_info
  | Char of meta_info
  | Float of int64 * meta_info
  | TypeName of string * meta_info
  | MacroName of string * meta_info
  | NSTypeName of string * string * meta_info
  | Pointer of type_alg * meta_info
  | Fun of (type_alg list) * type_alg * meta_info
  | Array of type_alg * cst_expr list * meta_info
  | MacroCall of type_alg
  | Ref of type_alg
  | Void
  | PreType of string * meta_info
  | CInt of meta_info
  | Modifiers of type_alg * meta_info
  | Error

and meta_info =
    {
      (* We may add more field for further use *)
      mutable modifiers : string list;
    }

type info = {
  mutable talg : type_alg;
  mutable const : bool;
  mutable rewrited : bool;
  mutable need_conv : (type_alg * type_alg) option;
}

let stdmeta () = { modifiers = [] }

let rec array_as_pointer = function
  | Array (t,(_::[] | []),m)
    -> Pointer (t,m)
  | Modifiers (t,m)
    -> Modifiers(array_as_pointer t, m)
  | t -> t

let push_metainfo { modifiers=ml } = function
    | (Int (_,_,m) | CInt m | Char m | Float (_,m)
      | TypeName (_,m) | MacroName (_,m) | NSTypeName (_,_,m)
      | Pointer (_,m)  | Fun (_,_,m) | Array (_,_,m) | PreType (_,m))
      -> m.modifiers <- ml@m.modifiers
    | _ -> ()

let get_metainfo = function
    | (Int (_,_,m) | CInt m | Char m | Float (_,m)
      | TypeName (_,m) | MacroName (_,m) | NSTypeName (_,_,m)
      | Pointer (_,m)  | Fun (_,_,m) | Array (_,_,m) | PreType (_,m))
      -> m
    | _ -> stdmeta ()

module StrSet = Set.Make(String)

exception Not_named_type

let rec get_name ns = function
  | TypeName(n,_) -> (ns,n)
  | NSTypeName (ns,n,_) -> (ns,n)
  | Modifiers (t,_) -> get_name ns t
  | _ -> raise Not_named_type

let to_set elts =
  List.fold_left (fun s e -> StrSet.add e s) StrSet.empty elts

let meta_fusion m1 m2 =
  let s1 = to_set m1.modifiers in
  let s2 = to_set m2.modifiers in
    { modifiers = StrSet.elements (StrSet.union s1 s2) }

let rec unfold_mods = function
  | Modifiers  (t,_) -> unfold_mods t
  | t -> t

let info t c r = {
  talg = t;
  const = c;
  rewrited = r;
  need_conv = None;
}

let get_rtype = function
  | Fun (_,rt,_) -> rt
  | _ -> assert false

let is_fun = function
  | Fun _ -> true
  | _ -> false

let voidstar = Pointer (Void, stdmeta ())
let intDef  = Int(false, Int64.of_int (Nativeint.size), stdmeta ())
let uintDef = Int(true,  Int64.of_int (Nativeint.size), stdmeta ())

let objbase_name = "cb_obj_base_type"
let objbase = TypeName (objbase_name, stdmeta ())


let rec rebuild_expr builder loc = function
  | I i -> builder#value (Ast.VInt i) loc
  | BinOp (o,e0,e1) ->
      builder#binop o
        (rebuild_expr builder loc e0)
        (rebuild_expr builder loc e1)
        {talg=uintDef; const=true; rewrited = builder#isrw; need_conv=None;}
        loc
  | PreOp (o,e0) ->
      builder#preop o
        (rebuild_expr builder loc e0)
        {talg=uintDef; const=true; rewrited = builder#isrw; need_conv=None;}
        loc
  | Sizeof t ->
      builder#sizeof t loc
  | CstVar x ->
      builder#id ?ns:None x
        {talg=uintDef; const=true; rewrited = builder#isrw; need_conv=None;}
        loc

let cstbop =
  let h = Hashtbl.create 7 in
  List.iter (fun (o,f) -> Hashtbl.add h o f)
    [
      ("+", Int64.add);
      ("-", Int64.sub);
      ("*", Int64.mul);
      ("/", Int64.div);
      ("<<", (fun i1 i2 -> Int64.shift_left i1 (Int64.to_int i2)));
      (">>", (fun i1 i2 -> Int64.shift_right i1 (Int64.to_int i2)));
    ];
  h

let eval_bop op e1 e2 =
  match (e1,e2) with
    | (I i1, I i2) -> I ((Hashtbl.find cstbop op) i1 i2)
    | _ -> BinOp(op,e1,e2)

let eval_uop op e =
  match (op,e) with
    | ("+",I i) -> I i
    | ("-",I i) -> I (Int64.neg i)
    | (("+"|"-"),_) -> PreOp (op,e)
    | _ -> raise Not_found

let strict_cst = function
  | I i -> i
  | _ -> raise Not_found

let rec cst_eval = function
  | Ast.Id { Ast.cont=([],x); Ast.loc=l } ->
      begin
        try CstVar ((Macro_set.get ())#get x) with
          | _ -> raise (Expr_not_constant l)
      end
  | Ast.Value {Ast.cont=Ast.VInt i} -> I i
  | Ast.Sizeof {Ast.cont=t} -> Sizeof (fromTExpr t)
  | Ast.BinOp {Ast.cont=(op,e1,e2);Ast.loc=l} ->
      begin
        try eval_bop op (cst_eval e1) (cst_eval e2) with
          | _ -> raise (Expr_not_constant l)
      end
  | Ast.PreOp {Ast.cont=(op,e);Ast.loc=l} ->
      begin
        try eval_uop op (cst_eval e) with
          | _ -> raise (Expr_not_constant l)
      end
  | e -> raise (Expr_not_constant (Asttools.expr_get_loc e))

and fromTExpr = function
  | Ast.Void _ -> Void
  | Ast.TName {Ast.cont=([], n)} -> TypeName (n,stdmeta ())
  | Ast.TName {Ast.cont=(ns::[], n)} -> NSTypeName (ns, n,stdmeta ())
  | Ast.Num {Ast.cont=(b,e)} ->
      begin
        try Int (b,strict_cst (cst_eval e),stdmeta ()) with
            _ -> raise (Expr_not_constant (Asttools.expr_get_loc e))
      end
  | Ast.TChar _ -> Char (stdmeta ())
  | Ast.TFloat {Ast.cont=e} ->
      begin
        try Float (strict_cst (cst_eval e),stdmeta ()) with
            _ -> raise (Expr_not_constant (Asttools.expr_get_loc e))
      end
  | Ast.TPointer {Ast.cont=t} -> Pointer (fromTExpr t, stdmeta ())
  | Ast.Array {Ast.cont=(t,el)} ->
    Array (fromTExpr t, List.map cst_eval el, stdmeta ())
  | Ast.Fun {Ast.cont=(tl, t)} ->
    Fun (List.map fromTExpr tl, fromTExpr t, stdmeta ())
  | _ -> assert false

let rec pp_cst_expr fmt = function
  | I i -> Format.fprintf fmt "%Ld" i
  | BinOp (o,e1,e2) ->
    begin
      Format.fprintf fmt "@;@[<b 2>(";
      pp_cst_expr fmt e1;
      Format.fprintf fmt "@;%s@;" o;
      pp_cst_expr fmt e2;
      Format.fprintf fmt ")@]"
    end
  | PreOp (o,e) ->
    begin
      Format.fprintf fmt "@;%s@[<h>(" o;
      pp_cst_expr fmt e;
      Format.fprintf fmt ")@]"
    end
  | Sizeof t ->
    begin
      Format.fprintf fmt "@;@[<h>sizeof@;";
      pp fmt t;
      Format.fprintf fmt "@]"
    end
  | CstVar x ->
      Format.fprintf fmt "@[<h>%s@]" x

and ppmeta fmt mi =
  Format.fprintf fmt "@[<h>";
  List.iter (fun m -> Format.fprintf fmt "%s@;" m) mi.modifiers;
  Format.fprintf fmt "@]"

and pp fmt = function
  | CInt m ->
    Format.fprintf fmt "@[<h>";
    ppmeta fmt m;
    Format.fprintf fmt "int@]"
  | Int(b,s,m) ->
    Format.fprintf fmt "@[<h>";
    ppmeta fmt m;
    Format.fprintf fmt "int<%s%Ld>@]"
      (if b then "+" else "") s
  | Char m ->
    Format.fprintf fmt "@[<h>";
    ppmeta fmt m;
    Format.fprintf fmt "char@]"
  | Float (s,m) ->
    Format.fprintf fmt "@[<h>";
    ppmeta fmt m;
    Format.fprintf fmt "float<%Ld>@]" s
  | PreType (n,m) ->
    Format.fprintf fmt "@[<h>";
    ppmeta fmt m;
    Format.fprintf fmt "%S@]" n
  | TypeName (n,m)
  | MacroName (n,m) ->
    Format.fprintf fmt "@[<h>";
    ppmeta fmt m;
    Format.fprintf fmt "%s@]" n
  | NSTypeName (ns,n,m) ->
    Format.fprintf fmt "@[<h>";
    ppmeta fmt m;
    Format.fprintf fmt "%s::%s@]" ns n
  | Pointer
      ((Pointer _ | Void | TypeName _ | Int(_,_,_) | Float _) as t,m) ->
      begin
	Format.fprintf fmt "@[<h>";
        ppmeta fmt m;
	pp fmt t;
	Format.fprintf fmt "*@]"
      end
  | Pointer (t,m) ->
      begin
	Format.fprintf fmt "@[<h>(";
        ppmeta fmt m;
	pp fmt t;
	Format.fprintf fmt ")*@]"
      end
  | Fun (tl,t,m) ->
      begin
	Format.fprintf fmt "@[<h>";
        ppmeta fmt m;
        Format.fprintf fmt "(@[<h>";
	Pptools.seppp fmt "," (pp fmt) tl;
	Format.fprintf fmt "@]):@;";
	pp fmt t;
	Format.fprintf fmt "@]"
      end
  | Array (t,ds,m) ->
      begin
	Format.fprintf fmt "@[<h>";
        ppmeta fmt m;
	pp fmt t;
	Format.fprintf fmt "[@[<h>";
	Pptools.seppp fmt "," (pp_cst_expr fmt) ds;
	Format.fprintf fmt "@]]@]"
      end
  | MacroCall t ->
      pp fmt t
  | Ref t -> pp fmt (Pointer (t,stdmeta ()))
  | Void -> Format.fprintf fmt "@[<h>void@]"
  | Modifiers (t,m) ->
	Format.fprintf fmt "@[<h>";
        ppmeta fmt m;
	pp fmt t;
        Format.fprintf fmt "@]"
  | Error -> Format.fprintf fmt "@[<h>ERROR@]"

let rec is_pointer = function
  | Pointer _ | Array _ -> true
  | Modifiers (t,_) -> is_pointer t
  | _ -> false

let rec is_bitoperable = function
  | CInt _ | Char _ | Int _ -> true
  | Modifiers (t,_) -> is_bitoperable t
  | _ -> false

let rec is_num = function
  | Char _
  | Int _
  | CInt _
  | Float _ -> true
  | Modifiers (t, _) -> is_num t
  | _ -> false

let rec is_int sign size = function
  | Int (b,s,_) -> ((not sign) || b) && s<=size
  | CInt _ -> (not sign) && size>=32L
  | Char _ -> size=8L
  | Modifiers (t,_) -> is_int sign size t
  | _ -> false

let rec is_signed = function
  | CInt m -> Int (false,32L,m)
  | Int (true,s,m) -> Int (false,s,m)
  | Modifiers (t,m) ->
    Modifiers (is_signed t, m)
  | t -> t

let rec deRef = function
  | Pointer (t,_)
  | Array (t,_::[],_) -> t
  | Modifiers (t,m) -> deRef t
  | _ -> assert false

let rec is_condition = function
  | Int _
  | CInt _
  | Char _
  | Float _
  | Pointer _
  | Array (_,_::_,_) -> true
  | Modifiers (t,_) -> is_condition t
  | _ -> false

let rec is_incrementable = function
  | CInt _ | Char _ | Int _ | Pointer _ -> true
  | Modifiers (t,_) -> is_incrementable t
  | _ -> false

let rec compatible t1 t2 =
  t1=t2
  || ((is_num t1) && is_num t2)
  || (
    match (t1,t2) with
      | (TypeName (n1,m1), TypeName (n2,m2))
        -> n1=n2 (* TODO: Check modifier for warning*)
      | (NSTypeName (ns1,n1,m1), NSTypeName (ns2,n2,m2))
        -> ns1=ns2 && n1=n2 (* TODO: Check modifier for warning*)
      | (Pointer (Void,m1), Pointer (_,m2))
      | (Pointer (_,m1), Pointer (Void,m2))
        -> true (* TODO: Check modifier for warning *)
      | (Pointer (p1,m1), Pointer (p2,m2))
        -> compatible p1 p2 (* TODO: Check modifier for warning *)
      | (((Pointer _) as tp), Array (ta,(_::[] | []),ma))
      | (Array (ta,(_::[] | []),ma), ((Pointer _) as tp))
        -> compatible (Pointer (ta,ma)) tp (* TODO: Check modifier for warning *)
      | (Array (ta1,d1,m1), Array(ta2,d2,m2))
        -> (compatible ta1 ta2) && d1=d2 (* TODO: Check modifier for warning *)
      | (Modifiers (t1,_), Modifiers(t2,_))
        -> compatible t1 t2
      | (Modifiers (t1,_), _)
        -> compatible t1 t2
      | (_,Modifiers (t2,_))
        -> compatible t1 t2
      | _ -> false
  )

let buildIntType b s =
  let get_size = function
    | n when n <= 8L  -> "8"
    | n when n <= 16L -> "16"
    | n when n <= 32L -> "32"
    | n when n <= 64L -> "64"
    | _ -> assert false
  in
    (if b then "uint" else "int")^(get_size s)^"_t"

let buildFloatType =
  let trans = Hashtbl.create 13 in
  let _ =
    List.iter (
      fun (s,t) -> Hashtbl.add trans s t
    ) [
      (32L,"float");
      (64L,"double");
      (80L,"long double");
    ]
  in fun s -> Hashtbl.find trans s

let dummang = new Mangler.dummang

let rec cst_to_c ?(exp=false) ?(mang=dummang) ?(ns="") = function
  | I i -> Int64.to_string i
  | BinOp (o,e1,e2) ->
      "("^(cst_to_c e1)^o^(cst_to_c e2)^")"
  | PreOp (o,e) ->
      o^"("^(cst_to_c e)^")"
  | Sizeof t ->
      "sizeof (" ^ (toCType ~exp ~mang ~ns "" t) ^ ")"
  | CstVar x -> x

and modtoCType modifiers =
  List.fold_left (fun s m -> s ^ m ^ " ") "" modifiers.modifiers

and toCType ?(exp=false) ?(mang=dummang) ?(ns="") var = function
  | Error -> assert false
  | MacroCall t -> toCType ~exp ~mang ~ns var t
  | Ref t -> toCType ~exp ~mang ~ns var (Pointer (t,stdmeta ()))
  | Void -> "void "^var
  | Int (u,s,m) ->
    (buildIntType u s)^" " ^ (modtoCType m) ^ var
  | Char m -> "char " ^ (modtoCType m) ^ var
  | CInt m -> "int " ^ (modtoCType m) ^ var
  | Float (s,m) ->
    (buildFloatType s)^" " ^ (modtoCType m) ^ var
  | TypeName (n,m)
  | MacroName (n,m)
    ->
    (mang#mangle ns n)^" " ^ (modtoCType m) ^ var
  | NSTypeName (ns,n,m) ->
    (mang#mangle ns n)^" " ^ (modtoCType m) ^ var
  | Pointer (t,m) ->
    toCType ~mang ~ns ("*" ^ (modtoCType m) ^ var) t
  | Fun (tl,rt,m) ->
      let rec ptype = function
        | [] -> ""
        | h::[] -> (toCType ~mang ~ns "" h)
        | h::t -> (toCType ~mang ~ns "" h)^","^ptype t
      in
        (toCType ~mang ~ns "" rt)
        ^"(*"^ (modtoCType m) ^ var ^")("^(ptype tl)^")"
  | Array (t,[],m) ->
    toCType ~mang ~ns ("*"^ (modtoCType m) ^ var) t
  | Array (t,dl,m) ->
      let rec pdims = function
          [] -> ""
        | h::t -> "["^(cst_to_c ~mang ~ns h)^"]"^(pdims t)
      in
        (toCType ~mang ~ns ((modtoCType m) ^ var) t)^(pdims dl)
  | Modifiers (t,m) ->
    toCType ~mang ~ns ((modtoCType m) ^ var) t
  | _ -> assert false

let not_multiple = function
  | 8L | 16L | 32L | 64L -> false
  | _ -> true


(* Other helper *)

open Ast

exception DEBUG_missing_type_info of location

let rec minimalSize = function
  | 0L -> 1L
  | n -> Int64.add 1L (minimalSize (Int64.div n 2L))

let canUnsign n = (Int64.compare Int64.zero n) <= 0

let value ge = function
  | VInt i -> Int (canUnsign i, minimalSize i, stdmeta ())
  | VFloat _ -> Float (64L,stdmeta ())
  | VChar _ -> Char (stdmeta ())
  | VCstStr _ -> Pointer (Char (stdmeta ()), stdmeta ())
  | VTag tag -> ge#tagenv#tag_type tag

let rec is_left_val ns ge = function
  | Id {cont=(_,x); info=Some info} ->
      true || not (info.const)
  | PreOp {cont=("*",_)}
  | Index _ -> true
  | Id {info=None;loc=loc} ->
      raise (DEBUG_missing_type_info loc)

  | Field ({cont=(expr, field)} as b)
  | PField ({cont=(expr, field)} as b) -> (
      try
        let box = match b.info with
          | Some info -> (
              match unfold_mods (info.talg) with
                | TypeName (name,_) ->
                    (ge#get_type_env ns)#get_class name
                | NSTypeName (namespace, name, _) ->
                    (ge#get_type_env namespace)#get_class name
                | _ ->
                  assert false
            )
          | None -> assert false
        in
          box#is_local field (* why ? *)
      with
          (* TODO: circular dependency: Types.No_such_class _ ->
           * true *)
          _ -> true
    )
  | _ -> false


(* Helper Operations: typeof ops extract type info from a typed AST node. *)

let typeof e =
  try (Asttools.expr_get_info e).talg with
    | _ -> assert false

let typeof_texpr t =
  try (Asttools.texpr_get_info t).talg with
    | _ -> raise (DEBUG_missing_type_info (Asttools.texpr_get_loc t))

let typeof_vdecl vd =
  match vd.vinfo with
    | Some {talg = t} -> t
    | None -> assert false

let typeof_typedef td =
  match td.tdinfo with
    | Some {talg = t} -> t
    | None -> assert false

let typeof_stm s =
  try (Asttools.stm_get_info s).talg with
    | _ -> assert false
