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

(* Management of Box type *)

open Ast

module StrPair =
struct
  type t = string * string
  let compare s1 s2 = Pervasives.compare s1 s2
end

module StrSet = Set.Make(String)
module StrPairSet = Set.Make(StrPair)

exception No_such_field of string
exception Redefine_field of string
exception Override_not_matching of
    string * TypeAlg.type_alg * TypeAlg.type_alg * location
exception Local_override of string * location

exception Interface_conflict
exception Cannot_override_in_interface of
  string * TypeAlg.type_alg
exception Interface_missing of string * string * location
exception Implem_break_interface of
    string * string * TypeAlg.type_alg * TypeAlg.type_alg * location
exception Interface_support_conflict of
  string * string * string * TypeAlg.type_alg * TypeAlg.type_alg * location

let locfname (l1,l2) = l1.Lexing.pos_fname

let rec exception_find get f = function
  | [] -> raise (No_such_field f)
  | h::t ->
    try get h f with _ -> exception_find get f t

class ['mang,'builder] box (name:string) loc =
object (self:'self)

  constraint 'info = TypeAlg.info
  constraint 'cblock = 'info expr ref

  method can_delete = false

  val mutable ns = ""
  method export n = ns <- n
  method get_namespace = ns

  method btype =
    if (ns <> "") then
      TypeAlg.NSTypeName (ns,name, TypeAlg.stdmeta ())
    else
      TypeAlg.TypeName (name, TypeAlg.stdmeta ())

  method get_loc : location = loc
  method get_name = name
  method get_c_name (mangler:'mang) =
    mangler#mangle ns name

  val fields = Hashtbl.create 101
  val mutable local_fields = StrSet.empty


  method add (field:string) (t:TypeAlg.type_alg) =
    if self#have_field field then
      raise (Redefine_field field);
    Hashtbl.add fields field t
  method make_local field_name =
    local_fields <- StrSet.add field_name local_fields
  method is_local field_name =
    StrSet.mem field_name local_fields

  method add_method : string -> TypeAlg.type_alg -> unit =
    assert false
  method make_abstract (_:string) = ()
  method is_abstract (_:string) = false
  method box_is_abstract = false
  method interfaces : (string * string) list = []

  method field_is_method (f:string) = false

  method get_field_type f =
    try Hashtbl.find fields f with
        _ -> raise (No_such_field f)

  method have_field f =
    Hashtbl.mem fields f

  method iter_field f =
    Hashtbl.iter f fields

  method compatible = function
    | TypeAlg.NSTypeName (_,t,_)
    | TypeAlg.TypeName (t,_) -> t=name
    | _ -> false

  method is_condition = false
  method is_bitoperable = false
  method is_incrementable = false
  method is_pointer = false

  method get_sizeof (mangler:'mang) =
    mangler#mangle ns self#get_name

  method get_init_name (mangler:'mang) = ""
  method get_del_name (mangler:'mang) = ""

  method rewrite_call (mangler:'mang) (builder:'builder)
    (cblock: 'cblock) = ()

  method converter
    (mangler:'mang) (builder:'builder) (e:'info expr)
    (target:TypeAlg.type_alg) (targetbox: ('mang,'builder) box option) = e

  method assign_converter
    (mangler:'mang) (builder:'builder) (e:'info expr)
    (left:'info expr) (target:TypeAlg.type_alg) (right:'info expr) = e

  method get_swconverter_symbol
    (mangler:'mang)
    (builder:'builder)
    (e:'info expr) = e

  method need_revconv = false
  method need_real_this = false
  method make_real (builder:'builder)
    (e : 'info expr) = e

  method chkassign (loc:location) tr =
    self#compatible tr

end

class ['mang,'builder] obj_box =
object (self)
  inherit ['mang,'builder] box TypeAlg.objbase_name (Lexing.dummy_pos, Lexing.dummy_pos)
  method need_revconv = true
  method box_is_abstract = true
end

class ['mang,'builder] enum_box name loc =
object (self:'self)
  inherit ['mang,'builder] box name loc as super

  method is_condition = true
  method is_bitoperable = true

end

class ['mang,'builder] class_box name loc =
object (self:'self)
  inherit ['mang,'builder] box name loc as super
  val mutable parent = None

  method can_delete = true

  val mutable override = StrSet.empty
  val mutable localmethods = StrSet.empty
  val mutable localfields  = StrSet.empty

  method converter
    (mangler:'mang) (builder:'builder) (e:'info expr)
    (target:TypeAlg.type_alg) targetbox =
    begin
      let rw = builder#isrw in
        builder#rwoff;
        let result = builder#cast target e (Asttools.expr_get_loc e) in
          if rw then builder#rwon;
          result
    end

  method assign_converter
    (mangler:'mang) (builder:'builder) (e:'info expr)
    (left:'info expr) (target:TypeAlg.type_alg) (right:'info expr)
    = e

  method get_swconverter_symbol
    (mangler:'mang) (builder:'builder) (e:'info expr) =
    builder#pfield e
      mangler#semfun
      (TypeAlg.info TypeAlg.voidstar true true)
      (Asttools.expr_get_loc e)

  method private local_entity f = function
    | TypeAlg.Fun _
      -> localmethods <- StrSet.add f localmethods
    | _
      -> localfields <- StrSet.add f localfields

  method is_overrided m =
    StrSet.mem m override

  method get_method_pointer_name (mangler:'mang) m =
    if (StrSet.mem m localmethods) || self#is_overrided m then
      mangler#build_method_name ns self#get_name m
    else
      begin
        match parent with
          | Some p -> p#get_method_pointer_name mangler m
          | _ -> assert false
      end

  method field_is_method m =
    (StrSet.mem m localmethods)
  || (
    match parent with
      | Some p -> p#field_is_method m
      | _ -> false
  )

  method get_field_type f =
    try Hashtbl.find fields f with
      | _ ->
        begin
          match parent with
            | Some p
              -> p#get_field_type f
            | _
              -> raise (No_such_field f)
        end

  method have_field f = (Hashtbl.mem fields f)
  ||( match parent with
      | Some p -> p#have_field f
      | _ -> false
  )

  method apply_field func f =
    func f (self#get_field_type f)

  method iter_has_parent f =
    StrSet.iter (fun k -> self#apply_field f k) localfields;
    match parent with
      | Some p -> p#iter_has_parent f
      | _ -> ()

  method iter_field f =
    StrSet.iter (fun k -> self#apply_field f k) localfields;
    StrSet.iter (fun k -> self#apply_field f k) localmethods;
    StrSet.iter (fun k -> self#apply_field f k) override;
    match parent with
      | Some p -> p#iter_has_parent f
      | _ -> ()

  method get_init_name (mangler:'mang) =
    (mangler#mangle ns self#get_name) ^ "_init"

  method get_del_name (mangler:'mang) =
    (mangler#mangle ns self#get_name)^"_del"

  method get_struct_name (mangler:'mang) =
    mangler#build_class_struct ns self#get_name

  method get_vtbl_cname (mangler:'mang) =
    (mangler#vtbl_name ns (self#get_name))
  method get_vtbl_sname (mangler:'mang) =
    (mangler#vtbl_name ns (self#get_name)) ^ "_s"

  method get_sizeof mangler =
    self#get_struct_name mangler

  method private cmp_parent tn =
    match parent with
      | Some p -> p#compatible tn
      | _ -> false

  method private extend m t =
    if (self#have_field m) then raise (Redefine_field m)
    else
      begin
        localfields <- StrSet.add m localfields;
        super#add m t
      end

  val mutable abs_methods = StrSet.empty

  method make_abstract m =
    abs_methods <- StrSet.add m abs_methods
  method is_abstract m =
    StrSet.mem m abs_methods
  method box_is_abstract =
    not (StrSet.is_empty abs_methods)

  method add_method m t =
    if (self#have_field m) then
      begin
        if (t <> self#get_field_type m) then
          raise
            (Override_not_matching (m,t,self#get_field_type m,loc));
        if Hashtbl.mem fields m then
          raise (Local_override (m,loc));
        override <- StrSet.add m override
      end
    else
      begin
        localmethods <- StrSet.add m localmethods;
        super#add m t
      end

  method add m t = self#extend m t

  method extends (c:'self) =
    parent <- Some c;

  method compatible = function
    | TypeAlg.Pointer (TypeAlg.Void,_) -> true
    | t ->
      (super#compatible t)
      || self#cmp_parent t

  method is_condition = true
  method is_incrementable = true
  method is_pointer = true

  method rewrite_call mangler builder rfc =
    let cblock =
      match !rfc with
        | Call b -> b
        | _ -> assert false
    in
    let (e,el) = cblock.cont in
    let rt =
      match cblock.info with
        | Some i -> i.TypeAlg.talg
        | None -> assert false
    in
      match e with
        | Field ({cont=(this,f);info=Some info} as b)
            when Asttools.no_compound this ->
          let e' = builder#pfield (
            builder#pfield this mangler#vtable_field info b.loc
          ) f info b.loc
          in
            cblock.cont <- (e',this::el)
        | Field ({cont=(this,f)} as b) ->
          let ft = self#get_field_type f in
          let e' =
            builder#compoundwraper
              this f
              mangler#vtable_field
              el ft rt b.loc
          in
            rfc := e'
        | _ -> assert false

  method get_field_descr =
    let herited =
      match parent with
        | Some p -> p#get_field_descr
        | _ -> []
    in
      herited@(Hashtbl.fold (
        fun f t l ->
          if not (StrSet.mem f localmethods) then
            (f,t) :: l
          else l
      ) fields [])

  method get_method_descr =
    let herited =
      match parent with
        | Some p -> p#get_method_descr
        | _ -> []
    in
      herited@(
        StrSet.fold (
          fun f l -> (f, self#get_field_type f) :: l
        ) localmethods []
      )

  val mutable interf = StrPairSet.empty
  method implem ns' i =
    interf <- StrPairSet.add (ns',i) interf
  method interfaces =
    StrPairSet.fold (fun x l -> x::l) interf []

  method get_interface_vtbl inter (mangler:'mang) =
    mangler#interface_vtbl self#get_name inter

end

class ['mang,'builder] interface_box name loc =
object (self:'self)
  inherit ['mang,'builder] box name loc as super
  val mutable parent = []
  val mutable methods = StrSet.empty
  val method_origin = Hashtbl.create 53
  method get_methods = methods

  method box_is_abstract = true

  method can_delete = true
  method is_condition = true
  method is_incrementable = true
  method is_pointer = true

  method check_method (c : ('mang,'builder) box) (m:string) =
    begin
      let ti = (self#get_field_type m) in
      let tc = (c#get_field_type m) in
        if ti <> tc then
          raise
            (Implem_break_interface (self#get_origin m,m,ti,tc,loc));
    end

  method fusion (c:'self) =
    try
      StrSet.iter
        (fun m -> c#check_method (self:> ('mang,'builder) box) m)
        (StrSet.inter methods c#get_methods);
      StrSet.iter
        (fun m -> Hashtbl.add method_origin m c#get_name)
        (StrSet.diff c#get_methods methods);
      methods <- StrSet.union methods (c#get_methods);
      parent <- c :: (c#get_parents @ parent);
    with
      | Implem_break_interface (i,m,ti,tc,loc)
        -> raise (Interface_support_conflict
                    (self#get_origin m,i,m,ti,tc,loc)
        )

  method extends (c:'self) =
    self#fusion c

  method get_parents = parent

  method have_field f =
    (Hashtbl.mem fields f)
  || (List.exists (fun p -> p#have_field f) parent)

  method get_field_type f =
    try Hashtbl.find fields f with
      | _ -> exception_find (fun p f -> p#get_field_type f) f parent

  method get_origin f =
    Hashtbl.find method_origin f

  method check_implem (cbox: ('mang,'builder) class_box) =
    StrSet.iter
      (fun m ->
        try
          self#check_method (cbox:> ('mang,'builder) box) m;
        with
          | No_such_field _
            -> raise (Interface_missing
                        (self#get_origin m,m,cbox#get_loc)
            )
      ) methods

  method cmp_parent t =
    List.exists (fun p -> p#compatible t) parent
  method compatible = function
    | TypeAlg.Pointer (TypeAlg.Void,_) -> true
    | t ->
      (super#compatible t)
      || self#cmp_parent t

  method add_method m t =
    if (self#have_field m) && (t <> self#get_field_type m) then
      raise (Interface_support_conflict
               (self#get_name, self#get_origin m,
                m, t, self#get_field_type m, loc)
      )
    else
      begin
        methods <- StrSet.add m methods;
        Hashtbl.add fields m t;
        if not (Hashtbl.mem method_origin m) then
          Hashtbl.add method_origin m self#get_name;
      end

  method semid (mangler:'mang) =
    mangler#interface_id ns self#get_name

  method rewrite_call mangler builder rfc =
    let cblock =
      match !rfc with
        | Call b -> b
        | _ -> assert false
    in
    let (e,el) = cblock.cont in
    let rt =
      match cblock.info with
        | Some i -> i.TypeAlg.talg
        | None -> assert false
    in
      match e with
        | Field ({cont=(this,f);info=Some info} as b)
            when Asttools.no_compound this ->
          let e' = builder#pfield (
            builder#pfield this mangler#ivtable_field info b.loc
          ) f info b.loc
          in
            cblock.cont <-
              (e',(builder#pfield this "real_this" info b.loc)::el)
        | Field ({cont=(this,f)} as b) ->
          let ft = self#get_field_type f in
          let e' =
            builder#compound_interwraper
              this f mangler#ivtable_field el ft rt b.loc
          in
            rfc := e'
        | _ -> assert false

  method get_vtbl_tdname (mangler:'mang) =
    (self#get_vtbl_sname mangler) ^ "_t"

  method get_method_descr =
    StrSet.fold (
      fun f l -> (f, self#get_field_type f) :: l
    ) methods []

  method get_struct_name (mangler:'mang) =
    mangler#build_class_struct ns self#get_name

  method get_vtbl_sname (mangler:'mang) =
    (mangler#vtbl_name ns (self#get_name)) ^ "_s"

  method get_vtbl_cbtype (mangler:'mang) =
    (mangler#vtbl_name "" self#get_name) ^ "_s_t"

  method need_revconv = true
  method need_real_this = true

  method make_real (builder:'builder) e =
    self#real_this builder e

  method private choose_interface
    (mangler:'mang) (builder:'builder) (e:'info expr)
    (target:TypeAlg.type_alg) =
    begin
      builder#preop "&" (
        builder#pfield (
          builder#pfield e (mangler#interfaces_addr)
            (TypeAlg.info target true true) (Asttools.expr_get_loc e)
        ) (ns^ "$" ^ self#get_name)
          (TypeAlg.info target true true) (Asttools.expr_get_loc e)
      ) (TypeAlg.info target true true) (Asttools.expr_get_loc e)
    end

  method sw_accessor
    (mangler:'mang) (builder:'builder)
    (target:TypeAlg.type_alg) (loc : Ast.location ) =
    let this =
      builder#id ?ns:None "this" (TypeAlg.info target true true) loc
    in
      begin
        self#choose_interface mangler builder this target
      end

  method private real_this (builder:'builder) e =
    builder#pfield e "real_this"
      (TypeAlg.info TypeAlg.objbase true true)
      (Asttools.expr_get_loc e)

  method converter
    (mangler:'mang) (builder:'builder) (e:'info expr)
    (target:TypeAlg.type_alg) targetbox =
    let targetbox =
      match targetbox with
        | Some tb -> tb
        | None -> assert false
    in
    let loc = Asttools.expr_get_loc e in
    builder#cast self#btype (
      builder#dynamic_cast targetbox#need_real_this e
        (targetbox#get_swconverter_symbol mangler builder)
        target self#btype (self#semid mangler) loc
    ) loc

  method assign_converter
    (mangler:'mang) (builder:'builder) (e:'info expr)
    (left:'info expr) (target:TypeAlg.type_alg) (right:'info expr) = e

  method get_swconverter_symbol
    (mangler:'mang) (builder:'builder) (e:'info expr) =
    builder#pfield (
      builder#pfield e "real_this"
        (TypeAlg.info TypeAlg.objbase true true)
        (Asttools.expr_get_loc e)
    ) mangler#semfun
      (TypeAlg.info TypeAlg.voidstar true true)
      (Asttools.expr_get_loc e)

end

exception Macro_no_such_field_use_store of TypeAlg.type_alg

class ['mang,'builder] storebox name store loc env =
object (self:'self)
  inherit ['mang,'builder] box name loc as m
  method get_store:TypeAlg.type_alg = store
  method compatible = function
    | TypeAlg.TypeName (t,_) -> t=name
    | TypeAlg.NSTypeName (tns, t, m) ->
        (* TODO: compatibility for modifiers - handling warning *)
        tns=ns && t=name
    | t -> TypeAlg.compatible store t
  method is_condition = env#is_condition store
  method is_bitoperable = env#is_bitoperable store
  method is_incrementable = env#is_incrementable store
  method is_pointer = env#is_pointer store
  method get_sizeof mangler = env#get_sizeof mangler store
  method field_to_macro (mangler:'mang) (f:string) = f
end

class ['mang,'builder] macrobox name store loc env =
object (self:'self)
  inherit ['mang,'builder] storebox name store loc env as m

  method field_to_macro mangler f =
    "_CB_Macro$"^(mangler#mangle ns self#get_name)^"$"^f

  method get_field_type f =
    try m#get_field_type f with
      | No_such_field _ ->
          raise (Macro_no_such_field_use_store store)

  method rewrite_call mangler builder rfc =
    let cblock =
      match !rfc with
        | Call b -> b
        | _ -> assert false
    in
    let (e,el) = cblock.cont in
    let rt =
      match cblock.info with
        | Some i -> i.TypeAlg.talg
        | None -> assert false
    in
      match e with
        | Field ({cont=(this,f);info=Some info} as b)
            when Asttools.no_compound this ->
            let e' =
              builder#id ?ns:None (self#field_to_macro mangler f)
                (TypeAlg.info rt true true) b.loc
            in
              cblock.cont <- (e',this::el);
              cblock.info <- Some {
                info with
                  TypeAlg.talg = TypeAlg.MacroCall info.TypeAlg.talg;
                  TypeAlg.rewrited = false;
              }
        | _ -> assert false
end

exception Need_conv of TypeAlg.type_alg

class ['mang,'builder] property_box name loc store env =
object (self:'self)
  inherit ['mang,'builder] storebox name store loc env as m

  constraint 'info = TypeAlg.info

  method field_to_macro mangler f =
    "_CB_Property$"^(mangler#mangle ns self#get_name)^"$"^f

  val mutable getter : 'info fundecl option = None
  val mutable setter : 'info fundecl option = None

  val mutable real_type = None

  method set_realtype t = real_type <- Some t
  method realtype =
    match real_type with
      | Some t -> TypeAlg.unfold_mods t
      | None -> TypeAlg.Error

  method get_getter =
    match getter with
      | Some f -> f
      | _ -> assert false
  method get_setter =
    match setter with
      | Some f -> f
      | _ -> assert false

  method set_op = function
    | { Ast.fname="get"} as f ->
      getter <- Some f
    | { Ast.fname="set"} as f ->
      setter <- Some f
    | _ -> assert false

  method converter mangler builder e target targetbox =
    let loc = Asttools.expr_get_loc e in
    let cp =
      builder#id ?ns:None (Formacro.build_call_point loc)
        (TypeAlg.info target true true) loc
    in
    let op =
      builder#id ?ns:None (self#field_to_macro mangler "get")
        (TypeAlg.info (self#get_field_type "get") true true) loc
    in
      builder#call op [cp;e] (TypeAlg.info target true true) loc

  method assign_converter mangler builder e left target value =
    let loc = Asttools.expr_get_loc e in
    let cp =
      builder#id ?ns:None (Formacro.build_call_point loc)
        (TypeAlg.info target true true) loc
    in
    let op =
      builder#id ?ns:None (self#field_to_macro mangler "set")
        (TypeAlg.info (self#get_field_type "set") true true) loc
    in
      builder#call op [cp;left; value] (TypeAlg.info target true true) loc

  method same_type = function
    | TypeAlg.TypeName (n,_)
        when n = name -> true
    | TypeAlg.NSTypeName (ns',n,_)
        when ns'= ns && n = name -> true
    | _ -> false

  method real_compat t =
    TypeAlg.compatible self#realtype t

  method compatible t =
    self#same_type t
  || self#real_compat t
  || (m#compatible t && raise (Need_conv store))

  method chkassign loc = function
    | TypeAlg.TypeName (n,_)
    | TypeAlg.NSTypeName (_,n,_)
        when n = name -> true
    | t when self#real_compat t -> true
    | t when m#compatible t ->
      raise (Need_conv store)
    | _ -> false

  method is_condition =
    (env#is_condition self#realtype)
    || (env#is_condition store && raise (Need_conv store))
  method is_bitoperable =
    (env#is_bitoperable self#realtype)
    || (env#is_bitoperable store && raise (Need_conv store))
  method is_incrementable = false
    (* (env#is_incrementable self#realtype) *)
    (* || (env#is_incrementable store && raise (Need_conv store)) *)
  method is_pointer =
    (env#is_pointer self#realtype)
    || (env#is_pointer store && raise (Need_conv store))

  method get_sizeof mangler = env#get_sizeof mangler self#realtype

end

class ['e,'info,'mang,'builder] builder (ns:string) (env:'e) =
object (self:'self)
  val mutable cur = None
  val mutable name = ""

  method private update_name =
    match cur with
      | Some b -> name <- b#get_name
      | _ -> assert false

  method start_property name loc store realtype =
    let m = new property_box name loc store env in
    m#set_realtype realtype;
    env#register_property name m;
    cur <- Some (m :> ('mang,'builder) box);
    self#update_name;
    env#subtyper#bypass ns self#get_cur#get_name;
    env#register_box true self#get_cur#get_name self#get_cur;
    m

  method start_macro exp (mb:'info mclassdecl) =
    let store = TypeAlg.fromTExpr mb.store in
    let m = new macrobox mb.cmacro.cont store mb.mcloc env in
    env#register_macroclass mb.cmacro.cont m;
    cur <- Some (m :> ('mang,'builder) box);
    self#update_name;
    env#subtyper#bypass ns self#get_cur#get_name;
    env#register_box exp self#get_cur#get_name self#get_cur;
    m

  method start exp (b:'info boxdecl) =
    match cur with
      | None -> self#_start exp b
      | _ -> assert false

  method private get_cur =
    match cur with
      | Some c -> c
      | _ -> assert false

  method private expbox exp =
    if exp then
      self#get_cur#export ns

  val laterq = Queue.create ()
  method later (f: unit -> unit) =
    Queue.push f laterq
  method do_later =
    while not (Queue.is_empty laterq) do
      (Queue.take laterq) ()
    done

  method private _start exp bd =
    begin
      match bd with
        | Struct {cont=(_,b);loc=l}
        | Union  {cont=b;loc=l} ->
          cur <- Some (new box b.bname.cont l)
        | Enum   {cont=b;loc=l} ->
          cur <- Some (new enum_box b.bname.cont l)
        | Interface {cont=b;loc=l} ->
          let ib = new interface_box b.bname.cont l in
          begin
            env#subtyper#regiter_name ns ib#get_name;
            List.iter (
              fun (ns',p) ->
                ib#extends (env#get_ext_interface ns' p);
                env#subtyper#name_subtype ns ib#get_name ns' p;
            ) b.interf
          end;
          env#register_interface ib#get_name ib;
          self#later (fun () -> env#register_interface_tag ib);
          cur <- Some (ib :> ('mang,'builder) box);
        | Class  {cont=b;loc=l} ->
          let cb = new class_box b.bname.cont l in
          begin
            match b.parent with
              | Some p ->
                cb#extends (env#get_class p);
                env#subtyper#name_subtype ns cb#get_name ns p
              | _ -> ()
          end;
          env#subtyper#regiter_name ns cb#get_name;
          List.iter (
            fun (ns',i) ->
              cb#implem ns' i;
              List.iter (
                fun ibox ->
                  cb#implem ibox#get_namespace ibox#get_name
              ) (env#get_ext_interface ns' i)#get_parents;
              env#subtyper#name_subtype ns cb#get_name ns' i;
          ) b.interf;
          env#register_class cb#get_name cb;
          cur <- Some (cb :> ('mang,'builder) box);
    end;
    self#expbox exp;
    self#update_name;
    env#register_box exp self#get_cur#get_name self#get_cur

  method add f t = self#get_cur#add f t
  method add_method m t = self#get_cur#add_method m t
  method make_abstract m = self#get_cur#make_abstract m
  method get_box = self#get_cur
  method finalize =
    self#do_later;
    cur <- None; name <- ""

  val obb = ((new obj_box) :> ('mang,'builder) box)
  method obj_base_box = obb

  initializer
    env#register_builder self

end
