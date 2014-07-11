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

(* Subtyping Algorithm *)

(* An attempt for a better typing checking using subtyping *)

open TypeAlg

module NType =
struct
  type t = string * string
  let compare = Pervasives.compare
end

module TypeSet = Set.Make(NType)

let add_succ set t1 t2 =
  let succ =
    try Hashtbl.find set t1 with
      | Not_found -> TypeSet.empty
  in
    Hashtbl.replace set t1 (TypeSet.add t2 succ)

let connected set t1 t2 =
  let visited = ref (TypeSet.add t1 TypeSet.empty) in
  let q = Queue.create () in
  let found = ref false in
  Queue.push t1 q;
  while not (!found) && not (Queue.is_empty q) do
    let t = Queue.take q in
    let succ =
      try Hashtbl.find set t with
        | Not_found -> TypeSet.empty
    in
    TypeSet.iter (
      fun ts ->
        found := !found || ts = t2;
        if not (TypeSet.mem ts !visited) then
          begin
            visited := TypeSet.add ts !visited;
            Queue.push ts q;
            add_succ set t1 ts;
          end
    ) succ;
  done;
  !found

type special_cases =
  | PointerArith | Logical | Std
  | Cmp

let op_cases =
  let tab =
    let t = Hashtbl.create 17 in
      List.iter (fun (o,k) -> Hashtbl.add t o k)
        [
          ("+", PointerArith);
          ("-", PointerArith);
          ("+=", PointerArith);
          ("-=", PointerArith);
          ("||", Logical);
          ("||=", Logical);
          ("&&", Logical);
          ("&&=", Logical);
          ("!", Logical);
          ("==", Cmp);
          ("<", Cmp);
          (">", Cmp);
          ("<=", Cmp);
          (">=", Cmp);
          ("!=", Cmp);
        ];
      t
  in
    fun op ->
      try Hashtbl.find tab op with
        | Not_found -> Std

exception Need_conv of type_alg
exception Need_downcast_obj of int

class ['ge] subtyper (ge:'ge) =
object (self:'self)

  constraint 'ge = < debug : Debug_printer.printer; .. >

  val subt_set = Hashtbl.create 53
  val mutable bypass_set = TypeSet.empty

  method private cnx loc ((ns1,n1) as t1) t2 =
    (t1 = t2) ||
      if not (Hashtbl.mem subt_set t1) then
        begin
          let debug : Debug_printer.printer = ge#debug in
            self#dump_out;
            debug#locprint 1 loc ""
              "Subtyper: type not found: %s::%s" ns1 n1;
            false
        end
      else connected subt_set t1 t2

  method private special mode loc ns t1 t2 =
    match (mode,t1,t2) with
      | (_, Modifiers (t1,_), t2)
      | (_, t1, Modifiers (t2,_) )
        -> self#special mode loc ns t1 t2
      | (PointerArith, Pointer (Void,_), _)
      | (PointerArith, _, Pointer (Void,_))
        -> false
      | ((PointerArith | Logical), Pointer _, Int _)
      | ((PointerArith | Logical), Int _, Pointer _)
      | ((PointerArith | Logical), Pointer _, Pointer _)
      | (Logical, Int _, Int _)
        -> true
      | (PointerArith, ((NSTypeName _ | TypeName _) as t),
         (Pointer _ | Int _))
      | (PointerArith, (Pointer _ | Int _),
         ((NSTypeName _ | TypeName _) as t))
        -> (ge#get_type_env ns)#is_pointer t
      | (Logical, ((NSTypeName _ | TypeName _) as t),
         (Pointer _ | Int _))
      | (Logical, (Pointer _ | Int _),
         ((NSTypeName _ | TypeName _) as t))
        -> (ge#get_type_env ns)#is_condition t
      | (Cmp, (NSTypeName _ | TypeName _),(NSTypeName _ | TypeName _))
        ->
        begin
          try
            let i =
              (if ((ge#get_type_env ns)#get_box_from_type t1)#need_real_this
               then 1 else 0)
              +
                (if ((ge#get_type_env ns)#get_box_from_type t2)#need_real_this
                 then 2 else 0)
            in
              if i = 0 then true else raise (Need_downcast_obj i)
          with
            | Types.Not_boxed -> self#_is_subtype loc ns t1 t2
        end
      | _ -> self#_is_subtype loc ns t1 t2

  method is_subtype ?(mode=Std) loc ns t1 t2 =
    let (t1,t2) = (array_as_pointer t1, array_as_pointer t2) in
      (t1 = t2)
      || ((self#is_bypass ns t1)
          && (ge#get_type_env ns)#compatible loc t1 t2)
      || match mode with
          | Std -> self#_is_subtype loc ns t1 t2
          | _ -> self#special mode loc ns t1 t2

  method private _is_subtype loc ns t1 t2 =
    (t1 = t2)
  || match (t1,t2) with
      | (Pointer (Void, _), t)
      | (t, Pointer (Void, _))
        -> (ge#get_type_env ns)#is_pointer t
      | _
        ->
        begin
          ge#debug#locprint 1 loc ns
            "@[<b 2>_is_subtype for: %t %t@]"
            (fun fmt -> pp fmt t1) (fun fmt -> pp fmt t2);
          try
            (self#cnx loc (get_name ns t1) (get_name ns t2)
            && ( raise (Need_conv t2)))
            || (ge#debug#print 2 "@,@[<h>NOT SUBTYPE@]";false)
          with
            | Not_named_type
              -> self#regular_type_is_subtype loc ns t1 t2
        end

  method regular_type_is_subtype loc (ns:string) t1 t2 =
    let (t1,t2) = (array_as_pointer t1, array_as_pointer t2) in
      match (t1,t2) with
        | (Char _, Int _)
        | (Int _, Char _)
          -> true
        | (Int (b1,s1,_), Int (b2,s2,_))
          ->
          ((b1 || not b2) ||(ge#emit_warning ns (Warn.UnsignSignCmp loc);true))
          && (s1 <= s2 || (ge#emit_warning ns (Warn.IntegerTooWide loc);true))
        | (Float (s1,_), Float (s2,_))
          -> s1 <= s2 || (ge#emit_warning ns (Warn.FloatTooWide loc);true)
        | (Int _, Float _)
          -> true
        | (CInt m, _)
          -> self#_is_subtype loc ns (Int (false, 32L, m)) t2
        | (_, CInt m)
          -> self#_is_subtype loc ns t1 (Int (false, 32L, m))
        | (Modifiers (t1,_), t2)
        | (t1, Modifiers (t2,_))
          -> self#_is_subtype loc ns t1 t2
        | (Pointer (t1,_), Pointer (t2,_))
          -> self#_is_subtype loc ns t1 t2
        | (Fun (pl1,rt1,_), Fun (pl2,rt2,_))
          ->
          self#_is_subtype loc ns rt1 rt2
          && (try List.for_all2 (
            fun t1 t2 -> self#_is_subtype loc ns t2 t1
          ) pl1 pl2 with Invalid_argument _ -> false)
        | (Array (t1,d1,_), Array (t2,d2,_))
          ->
          self#_is_subtype loc ns t1 t2
          && (d1 = d2)
        | _ -> false

  method name_subtype ns1 n1 ns2 n2 =
    ge#debug#print 2 "@,@[<b 2>Subtyping adding:@;%s::%s -> %s::%s@]"
      ns1 n1 ns2 n2;
    add_succ subt_set (ns1,n1) (ns2,n2)

  method regiter_name ns n =
    if not (Hashtbl.mem subt_set (ns,n)) then
      Hashtbl.add subt_set (ns,n) TypeSet.empty

  method bypass ns n =
    bypass_set <- TypeSet.add (ns,n) bypass_set
  method is_bypass ns t =
    try
      TypeSet.mem (get_name ns t) bypass_set
    with
      | Not_named_type -> false

  method dump_out =
    let debug : Debug_printer.printer = ge#debug in
    let fmt = debug#get_formatter 1 in
      begin
        debug#print 1 "#### Subtyping graph ####";
        Hashtbl.iter (
          fun (ns,n) v ->
            Format.fprintf fmt "@[<v>@[<v 4>@[<b 2>  %s::%s" ns n;
            Format.fprintf fmt "@] ->";
            if v = TypeSet.empty then Format.fprintf fmt " emtpy";
            TypeSet.iter (
              fun (ns,n) -> Format.fprintf fmt "@,%s::%s" ns n
            ) v;
            Format.fprintf fmt "@]@,@]";
        ) subt_set;
        Format.fprintf fmt "@[<v>@[<v 2>bypass:";
        TypeSet.iter (
          fun (ns,n) -> Format.fprintf fmt "@,%s::%s" ns n
        ) bypass_set;
        Format.fprintf fmt "@]@,@]";
        debug#print 1 "@[<h>#### Subtyping graph: end ####@]";
      end

end
