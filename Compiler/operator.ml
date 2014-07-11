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

(* Operators management *)

exception OperandFailed of int

type genValidator =
  | ArithBOp | BitBOp | LogBOp | CmpBOp
  | ArithUOp | BitUOp | LogUOp | IncUOp | DerefOp | RefOp

let operators =
  [
    (BitBOp,   ["|";"&";"%";"<<";">>";"^"]);
    (ArithBOp, ["+";"-";"*";"/"]);
    (LogBOp,   ["||";"&&";]);
    (CmpBOp,   ["==";"<";">";"<=";">=";"!="]);
    (ArithUOp, ["+";"-";]);
    (BitUOp,   ["~"]);
    (LogUOp,   ["!"]);
    (IncUOp,   ["++";"--"]);
    (DerefOp,  ["*"]);
    (RefOp,    ["&"]);
  ]


exception Need_conv of int * TypeAlg.type_alg * TypeAlg.type_alg

let rec opBArith t1 t2 =
  let m =
    TypeAlg.meta_fusion
      (TypeAlg.get_metainfo t1)
      (TypeAlg.get_metainfo t2)
  in
    match (t1,t2) with
      | (TypeAlg.Modifiers (t1,_), TypeAlg.Modifiers (t2,_)) ->
          opBArith t1 t2
      | (TypeAlg.Modifiers (t1,_), _) ->
          opBArith t1 t2
      | (_,TypeAlg.Modifiers (t2,_)) ->
          opBArith t1 t2
      | (TypeAlg.Char _, TypeAlg.Char _) ->
          TypeAlg.Char m

      | (TypeAlg.Char _, TypeAlg.Int(b,s,_))
      | (TypeAlg.Int(b,s,_), TypeAlg.Char _) ->
          TypeAlg.Int(b,max 8L s, m)

      | (TypeAlg.Int(b1,s1,_),TypeAlg.Int(b2,s2,_)) ->
          TypeAlg.Int(b1&&b2,max s1 s2, m)

      | (TypeAlg.Float(s1,_),TypeAlg.Float(s2,_)) ->
          TypeAlg.Float(max s1 s2, m)

      | (TypeAlg.Float(s1,_),(TypeAlg.Int(_,_,_)|TypeAlg.Char _))
      | ((TypeAlg.Int(_,_,_)|TypeAlg.Char _),TypeAlg.Float(s1,_))
        -> TypeAlg.Float(s1,m)

      | ((TypeAlg.Int(_,_,_)
         | TypeAlg.Char _), (TypeAlg.Pointer _ as t))
      | ((TypeAlg.Pointer _ as t),(TypeAlg.Int(_,_,_)|TypeAlg.Char _))
        -> t

      | ((TypeAlg.TypeName _ | TypeAlg.NSTypeName _), t)
      | (t, (TypeAlg.TypeName _ | TypeAlg.NSTypeName _)) -> t

      | _ -> assert false

let rec opBCmp ns ge l t1 t2 =
  let m =
    TypeAlg.meta_fusion
      (TypeAlg.get_metainfo t1)
      (TypeAlg.get_metainfo t2)
  in
    match (t1,t2) with
      | (TypeAlg.Modifiers (t1,_), TypeAlg.Modifiers (t2,_)) ->
        opBCmp ns ge l t1 t2
      | (TypeAlg.Modifiers (t1,_), _) ->
        opBCmp ns ge l t1 t2
      | (_,TypeAlg.Modifiers (t2,_)) ->
        opBCmp ns ge l t1 t2
      | (TypeAlg.Char _, TypeAlg.Char _) ->
        TypeAlg.Int(false,1L,m)

      | (TypeAlg.Char _, TypeAlg.Int(b,_,_))
      | (TypeAlg.Int(b,_,_), TypeAlg.Char _) ->
        if not b then ge#emit_warning ns (Warn.UnsignSignCmp l);
        TypeAlg.Int(false,1L,m)

      | (TypeAlg.Int(b1,_,_), TypeAlg.Int(b2,_,_)) ->
        if b1 <> b2 then ge#emit_warning ns (Warn.UnsignSignCmp l);
        TypeAlg.Int(false,1L,m)

      | (TypeAlg.Int _,  TypeAlg.Pointer _)
      | (TypeAlg.Pointer _, TypeAlg.Int _) ->
        ge#emit_warning ns (Warn.PointerIntCmp l);
        TypeAlg.Int(false,1L,m)

      | _ -> TypeAlg.Int(false,1L,m)

let bitBOp t1 t2 =
  let m =
    TypeAlg.meta_fusion
      (TypeAlg.get_metainfo t1)
      (TypeAlg.get_metainfo t2)
  in
    match (t1,t2) with
      | (TypeAlg.Int(b1,s1,_),TypeAlg.Int(b2,s2,_)) ->
        TypeAlg.Int(b1||b2,max s1 s2,m)

      | ((TypeAlg.Int _ as t), _)
      | (_,(TypeAlg.Int _ as t)) -> TypeAlg.Modifiers (t,m)

      | _ -> TypeAlg.uintDef

let logOp = TypeAlg.Int(true, 1L, TypeAlg.stdmeta ())

let rec deref l ns ge = function
  | TypeAlg.Modifiers (t,_) ->
    deref l ns ge t
  | TypeAlg.Pointer (TypeAlg.Void, _) ->
    ge#emit_warning ns (Warn.VoidDeref l);
    TypeAlg.Void

  | TypeAlg.Pointer (t,_)
  | TypeAlg.Array (t, _::[],_) -> t

  | _ -> raise (OperandFailed 0)

class ['ge] operator (ge:'ge) =
object (self:'self)

  val binop = Hashtbl.create 13
  val uniop = Hashtbl.create 13

  method registerOp (op:string) = function
    | (ArithBOp as f)
    | (BitBOp as f)
    | (CmpBOp as f)
    | (LogBOp as f) -> Hashtbl.add binop op f
    | (ArithUOp | BitUOp
      | LogUOp  | IncUOp
      | DerefOp | RefOp ) as f ->
        Hashtbl.add uniop op f

  method private chk ns t = function
    | ArithBOp | ArithUOp
        -> (ge#get_type_env ns)#is_condition t
    | BitBOp | BitUOp
        -> (ge#get_type_env ns)#is_bitoperable t
    | LogBOp | LogUOp
        -> (ge#get_type_env ns)#is_condition t
    | CmpBOp -> (ge#get_type_env ns)#is_condition t
    | IncUOp -> (ge#get_type_env ns)#is_incrementable t
    | DerefOp -> (ge#get_type_env ns)#is_pointer t
    | RefOp -> true

  method private bchk l ns t0 t1 = function
    | ArithBOp -> opBArith t0 t1
    | BitBOp -> bitBOp t0 t1
    | LogBOp -> logOp
    | CmpBOp -> opBCmp ns ge l t0 t1
    | _ -> assert false

  method private uchk l ns sign_needed t = function
    | IncUOp | ArithUOp ->
        if sign_needed then TypeAlg.is_signed t
        else t
    | BitUOp  -> t
    | LogUOp  -> logOp
    | DerefOp -> deref l ns ge t
    | RefOp   -> TypeAlg.Pointer (t, TypeAlg.stdmeta ())
    | _ -> assert false

  method uni l ns op t =
    let k = Hashtbl.find uniop op in
      if (self#chk ns t k) then
        self#uchk l ns (op="-") t k
      else raise (OperandFailed 0)

  method bin l ns op t0 t1 =
    let k = Hashtbl.find binop op in
      match (self#chk ns t0 k, self#chk ns t1 k) with
        | (false,false) -> raise (OperandFailed 2)
        | (false,_)     -> raise (OperandFailed 0)
        | (_,false)     -> raise (OperandFailed 1)
        | _ -> self#bchk l ns t0 t1 k

  val assignop =
    let h = Hashtbl.create 11 in
      List.iter (fun (a,o) -> Hashtbl.add h a o)
        [
	  ("+=","+");
	  ("-=","-");
	  ("*=","*");
	  ("/=","/");
	  ("%=","%");
	  ("|=","|");
	  ("^=","^");
	  ("&=","&");
	  ("<<=","<<");
	  (">>=",">>");
        ]; h

  method assign l ns aop t0 t1 =
    self#bin l ns (Hashtbl.find assignop aop) t0 t1

  initializer
    ge#register_operators self;
    List.iter (
      fun (k,ops) ->
        List.iter (fun o -> self#registerOp o k) ops
    ) operators;
end
