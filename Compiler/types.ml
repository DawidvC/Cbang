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

(* Types Environment *)

(*
 * Basic purpose pf types is to map type name and concrete type definitions:
 * any types that can remapped as a concrete type will and only boxed types
 * will keep their name as entry point.
 *)

exception Already_defined_typename of
    string * Ast.location option * Ast.location
exception No_such_typename of string
exception Typename_not_exported of string * string
exception No_such_box of string
exception Not_boxed
exception Not_class
exception No_such_class of string
exception No_such_interface of string
exception No_such_macroclass of string
exception No_such_property of string

exception DEBUG_not_solved_pretype of string * string * Ast.location

open TypeAlg

module StrSet = Set.Make (String)

class ['mang,'box,'cbox,'ge] env (ns:string) (ge:'ge) =
  let ext_get opened f =
    let rec aux = function
      | [] -> raise Not_found
      | h::t ->
          try
            f h
          with _ -> aux t
    in
      aux opened
  in
object (self:'self)

  constraint 'ge = <debug: Debug_printer.printer; ..>

  val typemap = Hashtbl.create 53
  val boxmap  = Hashtbl.create 53
  val cboxmap = Hashtbl.create 53
  val iboxmap = Hashtbl.create 53
  val macrostore = Hashtbl.create 53
  val propstore = Hashtbl.create  53
  val mutable tn_exported =  StrSet.empty
  val mutable opened = []
  val mutable builder = None
  val mutable curbox = None

  method clone =
    {<
      typemap    = Hashtbl.copy typemap;
      boxmap     = Hashtbl.copy boxmap;
      cboxmap    = Hashtbl.copy cboxmap;
      iboxmap    = Hashtbl.copy iboxmap;
      macrostore = Hashtbl.copy macrostore;
      curbox     = None;
    >}

  method open_ns (ns:string)
    (e:'self) =
    opened <- (ns,e) :: opened

  method is_exported t = StrSet.mem t tn_exported

  method private _mod_canon m =
    m.modifiers <-
      StrSet.elements
      (List.fold_left (fun s x -> StrSet.add x s) StrSet.empty m.modifiers)

  method mod_canon = function
    | (Int (_,_,m) | CInt m | Char m | Float (_,m)
      | TypeName (_,m) | MacroName (_,m) | NSTypeName (_,_,m)
      | Pointer (_,m)  | Fun (_,_,m) | Array (_,_,m) | PreType (_,m))
      -> self#_mod_canon m
    | _ -> ()

  method canon t =
    match self#_canon t with
      | TypeName (n,m)
        -> NSTypeName (ns,n,m)
      | t -> t

  method private _canon = function
    | (Int _ | CInt _ | Char _ | Float _ | Void | Error
          | MacroCall _ | Ref _ | MacroName _ )
        as t
      -> self#mod_canon t; t
    | PreType (t,m)
      ->
      begin
        self#_mod_canon m;
        match self#get_type t with
          | PreType (n,m') ->
            Modifiers (TypeName (n,m'),m)
          | t' -> Modifiers (t',m)
      end
    | TypeName (t,m)
      -> Modifiers (self#get_type t,m)
    | NSTypeName (n,t,m) when n = ns
      -> Modifiers (self#get_type t,m)
    | NSTypeName (n,t,m) as nst
      -> Modifiers ((ge#get_type_env n)#canon nst,m)
    | Pointer (t,m)
      ->
      begin
        self#_mod_canon m;
        Pointer (self#canon t,m);
      end
    | Fun (al,rt,m)
      ->
      begin
        self#_mod_canon m;
        Fun (List.map self#canon al, self#canon rt,m)
      end
    | Array (t,el,m)
      ->
      begin
        self#_mod_canon m;
        Array (self#canon t, el,m)
      end
    | Modifiers (t, { modifiers = [] } )
      -> self#canon t
    | Modifiers (t,m)
      ->
      begin
        self#_mod_canon m;
        Modifiers (self#canon t, m)
      end

  method boxcompatible t tn =
    try
      (self#get_box tn)#compatible t
    with
      | (Box.Need_conv store) as e -> raise e
      | _ -> false

  method private chk_modifier loc m1 m2 =
    match (Qualifiers.compat m1.modifiers m2.modifiers) with
      | _::_ as l ->
          List.iter (
            fun q ->
              ge#emit_warning ns (Warn.Discard_qualifier (q,loc))
          ) l
      | _ -> ()

  method private inner_compat loc t0 t1 =
    begin
      match t0 with
        | TypeName (t0,_)
        | MacroName (t0,_) ->
            self#boxcompatible t1 t0
        | NSTypeName (n,t0,_) when n=ns ->
            self#boxcompatible t1 t0
        | NSTypeName (n,_,_) as t0 ->
            (ge#get_type_env n)#compatible loc t0 t1
        | Pointer (Void,_) -> self#is_pointer t1
        | Modifiers (t0,_) ->
            self#inner_compat loc t0 t1
        | _ -> TypeAlg.compatible t0 t1
    end

  method compatible loc t1 t2 =
    let (t1,t2) = (self#canon t1,self#canon t2) in
    let m1 = TypeAlg.get_metainfo t1 in
    let m2 = TypeAlg.get_metainfo t2 in
    let _ = self#chk_modifier loc m1 m2 in
      (ge#debug : Debug_printer.printer)#locprint 1 loc ""
        "@[<b 2>Using #compatible for %t %t@]"
        (fun fmt -> TypeAlg.pp fmt t1) (fun fmt -> TypeAlg.pp fmt t2);
      (self#inner_compat loc (TypeAlg.unfold_mods t1)
         (TypeAlg.unfold_mods t2))

  method chkassign loc tl tr =
    let (tl,tr) =
      (TypeAlg.unfold_mods (self#canon tl),
       TypeAlg.unfold_mods (self#canon tr))
    in
      match tl with
        | NSTypeName (ns',_,_) as tl when ns' <> ns ->
            (ge#get_type_env ns')#chkassign loc tl tr
        | NSTypeName(_,n,_) | TypeName(n,_) ->
            begin
              match tr with
                | NSTypeName(ns',n',_) when ns'=ns -> n'=n
                | NSTypeName(_,n',_) | TypeName(n',_) -> n'=n
                | _ -> false
            end
            || (try
                  (self#_getBox tl)#chkassign loc tr
                with Not_found -> self#compatible loc tl tr)
        | tl ->
            try
              (self#_getBox tl)#chkassign loc tr
            with Not_found -> self#compatible loc tl tr

  method private _getBox = function
    | TypeName (t,_)
    | MacroName (t,_) -> self#get_box t
    | NSTypeName (n,t,_) when n=ns -> self#get_box t
    | NSTypeName (n,t,_) -> (ge#get_type_env n)#get_box t
    | Modifiers (t,_) -> self#_getBox t
    | _ -> raise Not_found

  method is_condition t =
    let t = self#canon t in
      try
        (self#_getBox t)#is_condition
      with
          Not_found -> TypeAlg.is_condition t

  method is_bitoperable t =
    try
      (self#_getBox t)#is_bitoperable
    with
        Not_found -> TypeAlg.is_bitoperable t

  method is_incrementable t =
    try
      (self#_getBox t)#is_incrementable
    with
        Not_found -> TypeAlg.is_incrementable t

  method is_pointer t =
    try
      (self#_getBox t)#is_pointer
    with
        Not_found -> TypeAlg.is_pointer t

  method is_int sign size t =
    TypeAlg.is_int sign size (self#canon t)

  method private check_t n l =
    try
      let (t, old_loc) = Hashtbl.find typemap n in
        begin
          match t with
            | TypeAlg.PreType (n',_) when n = n' -> Some old_loc
            | _ ->
                raise (Already_defined_typename (n, Some old_loc, l))
        end
    with Not_found -> None

  method pre_register_type exp (name:string) loc =
    begin
      match (self#check_t name loc) with
        | None ->
            Hashtbl.add typemap name
              ((TypeAlg.PreType (name,TypeAlg.stdmeta ())), loc);
            if exp then
              tn_exported <- StrSet.add name tn_exported;
        | l ->
            raise (Already_defined_typename (name, l, loc))
    end

  method post_register_type name t =
    begin
      let (_,loc) = Hashtbl.find typemap name in
        Hashtbl.replace typemap name (t, loc);
        tn_exported <- StrSet.add name tn_exported;
    end

  method register_type exp (name:string) (t:TypeAlg.type_alg) loc =
    begin
      begin
        match self#check_t name loc with
          | None ->
              Hashtbl.replace typemap name (t, loc);
          | Some l when l = loc ->
              self#post_register_type name t
          | l ->
              raise (Already_defined_typename (name, l, loc))
      end;
      if exp then
        tn_exported <- StrSet.add name tn_exported;
    end

  method get_type n =
    try
      match Hashtbl.find typemap n with
        | (TypeAlg.PreType (t,m), _ ) ->
            TypeAlg.NSTypeName (ns,t,m)
        | (t,_) -> t
    with
        Not_found ->
          begin
            try
              ext_get opened (fun (_,e) -> e#get_type n)
            with
                Not_found -> raise (No_such_typename n)
          end

  method register_box exp n (b:'box) =
    begin
      let t = NSTypeName (ns,n, stdmeta ()) in
        Hashtbl.add boxmap n b;
        self#post_register_type n t;
        if exp then
          tn_exported <- StrSet.add n tn_exported;
    end
  method get_box n =
    try
      Hashtbl.find boxmap n
    with
      | Not_found ->
         begin
           try
             ext_get opened (fun (_,e) -> e#get_box n)
           with
               Not_found -> raise (No_such_box n)
         end

  method register_class n (b:'cbox) =
    begin
      ignore (self#check_t n b#get_loc);
      Hashtbl.add cboxmap n b
    end

  method get_class_from_type = function
    | TypeAlg.TypeName (n,_)
      -> self#get_class n
    | TypeAlg.NSTypeName (ns, n, _)
      -> (ge#get_type_env ns)#get_class n
    | TypeAlg.Modifiers (t,_)
      -> self#get_class_from_type t
    | _ -> raise Not_class

  method get_class n =
    try Hashtbl.find cboxmap n with
      | Not_found -> raise (No_such_class n)

  method check_obj_type = function
    | TypeAlg.NSTypeName (n, _, _) as t when n <> ns
      -> (ge#get_type_env n)#check_obj_type t
    | TypeAlg.TypeName (tn,_)
    | TypeAlg.NSTypeName (_, tn,_)
      ->
      if not (Hashtbl.mem cboxmap tn) then
        raise (No_such_class tn)
      else tn
    | TypeAlg.Modifiers (t,_)
      -> self#check_obj_type t
    | _ -> raise Not_found

  method register_interface n (b:'ibox) =
    ignore (self#check_t n b#get_loc);
    Hashtbl.add iboxmap n b
  method get_interface n =
    try
      Hashtbl.find iboxmap n
    with
      | Not_found -> raise (No_such_interface n)
  method get_ext_interface ns' n =
    (ge#get_type_env ns')#get_interface n
  method register_interface_tag (b:'ibox) =
    let t = TypeAlg.Int(true, 32L, stdmeta ()) in
      begin
        ge#tagenv#register_tag
          (b#semid ge#mangler) t b#get_loc ns;
        ge#debug#print 3 "Registered: %s" (b#semid ge#mangler);
      end


  method register_macroclass (m:string) mb =
    ignore (self#check_t m mb#get_loc);
    Hashtbl.add macrostore m mb;
  method get_macroclass m =
    try Hashtbl.find macrostore m with
      | Not_found -> raise (No_such_macroclass m)

  method register_property (m:string) prop =
    ignore (self#check_t m prop#get_loc);
    Hashtbl.add propstore m prop

  method get_property m =
    try Hashtbl.find propstore m with
      | Not_found -> raise (No_such_property m)

  method get_box_from_type = function
    | TypeAlg.TypeName (n,_)
    | TypeAlg.NSTypeName (_,n,_)
        when n = TypeAlg.objbase_name
        -> self#builder#obj_base_box
    | TypeAlg.MacroName (t,_)
    | TypeAlg.TypeName (t,_)
      -> self#get_box t
    | TypeAlg.NSTypeName (n,t,_) when n = ns
      -> self#get_box t
    | TypeAlg.NSTypeName (n,_,_) as t
      -> (ge#get_type_env n)#get_box_from_type t
    | TypeAlg.Modifiers (t,_)
      -> self#get_box_from_type t
    | _ -> raise Not_boxed

  method register_builder b =
    builder <- Some b

  method builder =
    match builder with
      | Some b -> b
      | _ -> assert false

  method get_sizeof mangler t =
    try
      let box = self#get_box_from_type t in
        box#get_sizeof mangler
    with
      | Not_boxed -> TypeAlg.toCType "" ~mang:mangler t

  method set_current_box b =
    curbox <- Some b
  method get_current_box =
    match curbox with
      | Some b -> self#get_box b
      | _ -> raise Not_boxed
  method leave_curent_box = curbox <- None

  (* Env gained using open directive *)

  (* Ok, be sure every one is canon *)
  method post_typing =
    begin
      Hashtbl.iter
        (fun n (t,l) -> Hashtbl.replace typemap n ((self#canon t),l))
        typemap;
      Hashtbl.iter
        ( fun n -> function
            | (TypeAlg.PreType _ as t, l) ->
                Hashtbl.replace typemap n ((self#canon t),l)
            | _ -> ()
        ) typemap;
      Hashtbl.iter
        (fun n (t,l) -> Hashtbl.replace typemap n ((self#canon t),l))
        typemap;
      Hashtbl.iter
        ( fun n -> function
            | (TypeAlg.PreType (nt,_), l) ->
                raise (DEBUG_not_solved_pretype (n,nt,l))
            | _ -> ()
        ) typemap;
    end

  method subtyper = ge#subtyper

  initializer
    builder <- Some (new Box.builder ns self);
    ge#register_type_env ns self

end
