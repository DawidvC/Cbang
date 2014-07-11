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

(* Symbol management *)

exception No_such_symbol of string

class ['mang] symbol
  (ns:string) (srcname:string)
  (loc:Ast.location option) =
object (self:'self)

  val mutable glob = false
  val mutable cname = srcname
  val location = loc

  method get_location = location

  method register_mangler (m:'mang) =
    cname <- m#mangle ns srcname

  method export = glob <- true
  method is_exported = glob

  method get_srcname = srcname
  method get_c_name = cname

  val mutable stype = TypeAlg.Error
  method give_type t = stype <- t
  method get_type = stype

  val mutable cst=false
  method make_const = cst <- true
  method const = cst

end

class ['mang] ext_symbol
  (ns:string) (srcname:string)
  (loc:Ast.location option) =
object (self:'self)
  inherit ['mang] symbol ns srcname loc
  method export_as s =
    begin
      glob <- true;
      cname <- s;
    end
  method register_mangler _ = ()
end

class ['mang] gen_env (ns:string) (mng:'env) =
object (self:'self)
  val mutable mangler = mng
  val mutable tab = Hashtbl.create 53

  method get_mangler = mangler

  method add (name:string) (sym:'mang symbol) =
    sym#register_mangler mangler;
    Hashtbl.add tab name sym

  method add_nomangling name sym =
    Hashtbl.add tab name sym

  method get name =
    try Hashtbl.find tab name with
      | Not_found -> raise (No_such_symbol name)
  method rget name = Hashtbl.find tab name

  method local = false

  method mem name =
    Hashtbl.mem tab name
end

class ['mang] local_env ?(suffix="") prior ns mng =
object (self:'self)
  inherit ['mang] gen_env ns mng as super

  method add name sym =
    mangler#register_local_name name;
    super#add name sym

  method rget name =
    try super#get name with
      | _ -> prior#rget name
  method get name = self#rget name

  method local = true

  initializer
    mangler <- Mangler.factory ~suffix ~parent:(Some prior#get_mangler) ()
end

module StrSet = Set.Make(String)

class ['mang] env ns mng =
object (self:'self)
  inherit ['mang] gen_env ns mng as super
  val mutable exported = StrSet.empty
  val mutable level = 0
  val context = Stack.create ()
  val context_back = Hashtbl.create 53

  method enter_context () =
    self#enter_context_suffix "" ()

  method enter_context_suffix suffix () =
    let prior =
      if Stack.is_empty context then (self :> 'mang gen_env)
      else (Stack.top context)
    in
      level <- level + 1;
      Stack.push (new local_env ~suffix:suffix prior ns mng) context

  method leave_context =
    assert(level > 0);
    level <- level - 1;
    ignore(Stack.pop context);
    Gc.minor ()

  method save_context (key:string) =
    assert(level > 0);
    level <- level - 1;
    Hashtbl.replace context_back key (Stack.pop context);

  method restore_context key =
    if (Hashtbl.mem context_back key) then
      begin
        level <- level + 1;
        Stack.push (Hashtbl.find context_back key) context
      end
    else self#enter_context ()

  method add name sym =
    if Stack.is_empty context then
      begin
        super#add name sym;
        if sym#is_exported then
          exported <- StrSet.add name exported
      end
    else
      (Stack.top context)#add name sym

  method add_nomangling name sym =
    if Stack.is_empty context then
      begin
        super#add_nomangling name sym;
        if sym#is_exported then
          exported <- StrSet.add name exported
      end
    else
      (Stack.top context)#add_nomangling name sym

  method get name =
    try
      if Stack.is_empty context then super#get name
      else (Stack.top context)#get name
    with
      | _ -> self#ext_get name

  method exported_symbol_name =
    StrSet.elements exported

  method forwardable name =
    try
      not ((super#get name)#is_exported)
    with _ -> false

  method local =
    if Stack.is_empty context then false
    else (Stack.top context)#local

  (* Env gained using open directive *)

  val mutable opened = []

  method open_ns (ns:string)
    (e:'self) =
    opened <- (ns,e) :: opened

  method ext_get name =
    let rec aux = function
      | [] -> raise (No_such_symbol name)
      | (ns,e)::_ when e#mem name ->
        e#get name
      | _::t -> aux t
    in aux opened

end

let factory ?(asname) ns name exp t loc =
  let ns = if exp then ns else "" in
  let s =
    match asname with
        None -> new symbol ns name loc
      | Some a ->
        let s = new ext_symbol ns name loc in
          s#export_as a;
          (s:>< mangle : string -> string -> string; .. > symbol)
  in
  s#give_type t;
  if exp then s#export;
  s
