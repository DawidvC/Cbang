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

type location = (Lexing.position * Lexing.position)

type ('info,'a) locblock = {
  mutable loc : location;
  mutable cont : 'a;
  (* Ast content *)
  mutable info : 'info option
(* Will be "known" as TypeAlg.info*)
}

type value =
  | VInt of int64
  | VFloat of float
  | VChar of char
  | VCstStr of string
  | VTag of string

type 'info expr =
  | Value of ('info,value) locblock
  | Id of ('info, string list * string) locblock
  | BinOp of ('info, string * 'info expr * 'info expr) locblock
  | PreOp of ('info, string * 'info expr) locblock
  | PostOp of ('info, string * 'info expr) locblock
  | AssignOp of ('info, string * 'info expr * 'info expr) locblock
  | TerOp of ('info, 'info expr * 'info expr * 'info expr) locblock
  | Call of ('info, 'info expr * 'info expr list) locblock
  | Index of ('info, 'info expr * 'info expr list) locblock
  | Field of  ('info, 'info expr * string) locblock
  | PField of ('info, 'info expr * string) locblock
  | Cast of ('info, 'info texpr * 'info expr) locblock
  | Sizeof of ('info, 'info texpr) locblock
  | Decl of ('info, 'info vdecl) locblock
  | Compound of ('info, 'info statement list) locblock
  | SelfInit of ('info, 'info texpr * 'info expr list) locblock

and 'info swtest =
  | SwValue of ('info, value) locblock
  | Default of ('info, unit) locblock

and 'info statement =
  | EmptyStatement of ('info, unit) locblock
  | Delete of ('info, 'info expr) locblock
  | Expr of ('info, 'info expr) locblock
  | Block of ('info, 'info statement list) locblock
  | If of ('info, 'info expr * 'info statement * 'info statement) locblock
  | While of ('info, 'info expr * 'info statement) locblock
  | Do of ('info, 'info expr * 'info statement) locblock
  | For of ('info,
            'info expr option * 'info expr option * 'info expr option
            * 'info statement) locblock
  | Switch of
      ('info, 'info expr
        * ('info swtest * 'info statement list) list) locblock
  | Break of ('info, unit) locblock
  | Continue of ('info, unit) locblock
  | Return of ('info, 'info expr option) locblock
  | Label of ('info, string) locblock
  | Goto of ('info, string) locblock
  | Asm of ('info, 'info asmblock) locblock

and 'info asmoperand = {
  mutable asmopname : string option;
  mutable asmconstraint : string;
  mutable asmexpr : 'info expr;
  mutable oploc : location;
}

and 'info asmblock = {
  mutable asmcode : string;
  mutable asminputs : 'info asmoperand list;
  mutable asmouputs : 'info asmoperand list;
  mutable asmclobbers : string list;
  mutable asmvolatile : bool;
  mutable asminfo : 'info option;
  mutable asmloc : location;
}

and 'info fundecl = {
  mutable fname : string;
  mutable fnspace : string list;
  mutable fparams : (string list * 'info texpr * string) list;
  mutable finline : bool;
  mutable frtype : 'info texpr;
  mutable fbody : 'info statement list;
  mutable floc : location;
  mutable finfo : 'info option;
  mutable fexport : bool;
  mutable fasname : string option;
}

and 'info texpr =
  | Void of ('info, unit) locblock
  | TName of ('info, string list * string) locblock
  | Num of ('info, bool * 'info expr) locblock
  | TFloat of ('info, 'info expr) locblock
  | TChar of ('info, unit) locblock
  | TPointer of ('info, 'info texpr) locblock
  | Array of ('info, 'info texpr * 'info expr list) locblock
  | Fun of ('info, 'info texpr list * 'info texpr) locblock
  | TModifier of ('info, string * 'info texpr) locblock

and 'info vdecl = {
  mutable vmods : string list;
  mutable vtype : 'info texpr;
  mutable vname : string;
  mutable vinit : 'info expr option;
  mutable vloc  : location;
  mutable vinfo : 'info option;
}

and 'info member =
  | Method of 'info fundecl
  | SField of 'info vdecl
  | EField of ('info, string * 'info expr option) locblock

and 'info innerboxdecl = {
  bname : ('info, string) locblock;
  mutable bnspace : string list;
  mutable members : 'info member list;
  mutable constructor : 'info fundecl option;
  mutable destructor  : 'info fundecl option;
  mutable parent : string option;
  mutable interf : (string * string) list;
}

and 'info boxdecl =
  | Struct of ('info, bool * 'info innerboxdecl) locblock
  | Union of ('info, 'info innerboxdecl) locblock
  | Enum of ('info, 'info innerboxdecl) locblock
  | Class of ('info, 'info innerboxdecl) locblock
  | Interface of ('info, 'info innerboxdecl) locblock

and 'info mclassdecl = {
  cmacro : ('info, string) locblock;
  mutable mcnspace : string list;
  mutable store : 'info texpr;
  mutable mc_ops : 'info fundecl list;
  mutable mcloc : location;
}

and 'info propertydecl = {
  pname : string;
  mutable properstore : 'info texpr;
  mutable properthis : 'info texpr;
  mutable getter : 'info fundecl;
  mutable setter : 'info fundecl;
  mutable properloc : location;
}

and 'info typedef = {
  mutable tdname : string;
  mutable tdnspace : string list;
  mutable tdtype : 'info texpr;
  mutable tdloc  : location;
  mutable tdinfo : 'info option;
}

and 'info import =
  | Import of ('info, string) locblock
  | Include of ('info, string list) locblock
  | Open of ('info, string) locblock

and 'info modparam = {
  mutable modexport : bool;
  mutable modimports : 'info import list;
}

and 'info gdecl =
  | FDecl of 'info fundecl
  | TDef of 'info typedef
  | BDecl of 'info boxdecl
  | MCDecl of 'info mclassdecl
  | VDecl of 'info vdecl
  | Macro of 'info gdecl
  | Map of 'info vdecl * string
  | Proper of 'info propertydecl


and 'info source = {
  mutable srcmodparam : 'info modparam;
  mutable srcgdecls : 'info gdecl list;
  mutable srcloc : location;
}
