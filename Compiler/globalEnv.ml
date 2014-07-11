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

(* Global Envirronement: a gate to all envirronements with an access
   indexed on namespaces *)

class ['te,'se,'mang,'ops] env deb =
object (self:'self)
  val tenvs = Hashtbl.create 13
  val senvs = Hashtbl.create 13
  val fenv = Hashtbl.create 13
  val mutable mang : 'mang option = None
  method register_mangler m =
    mang <- Some m

  method mangler =
    match mang with
      | Some m -> m
      | _ -> assert false

  val mutable build:'self AstBuilder.builder = Obj.magic None
  method register_builder b =
    build <- b
  method builder = build

  method register_file_name (ns:string) (f:string) =
    Hashtbl.add fenv ns f
  method get_file_name ns =
    Hashtbl.find fenv ns
  method namespacePresent ns =
    Hashtbl.mem fenv ns

  method register_type_env (ns:string) (t:'te) =
    Hashtbl.add tenvs ns t
  method get_type_env ns =
    try Hashtbl.find tenvs ns with
      | _ -> raise (Asttools.Namespace_not_found ns)

  method register_sym_env (ns:string) (s:'se) =
    Hashtbl.add senvs ns s
  method get_sym_env ns =
    try Hashtbl.find senvs ns with
      | _ -> raise (Asttools.Namespace_not_found ns)

  method open_in ns nsin =
    begin
      (self#get_sym_env nsin)#open_ns
        ns (self#get_sym_env ns);
      (self#get_type_env nsin)#open_ns
        ns (self#get_type_env ns);
    end

  method emit_warning ns (w:Warn.warnKind) =
    Warn.warning (self#get_file_name ns) w

  val mutable ops:'ops = None
  method register_operators o =
    ops <- Some o;

  method operators =
    match ops with
      | Some o -> o
      | _ -> assert false

  (* Enum tag *)
  val tags = new Enum.tagenv
  method tagenv = tags


  (* Forward declaration management *)
  val forwarder = new Forward.forwarder
  method encounter ns s =
    if (self#get_sym_env ns)#forwardable s then
      forwarder#encounter s
  method declare ns s =
    if (self#get_sym_env ns)#forwardable s then
      forwarder#declare s
  method need_forward ns s =
    if (self#get_sym_env ns)#forwardable s then
      forwarder#need_forward s
    else false

  (* TODO: will be useless when current modification will be over *)
  (* "local" keyword handling *)

  (* Need more flexibility here *)

  (* Stores the type of the "local" variable in a declaration. This is
   * used during typing. *)
  val mutable local_type : (string * string) option = None
  method get_local_type = local_type
  method set_local_type t = local_type <- Some t

  (* Stores the name of the local-allocated variable in a declaration. This is
   * used to put the proper name as the first parameter of a constructor when
   * generating the code. *)
  val mutable local_name : string option = None
  method get_local_name = local_name
  method set_local_name n = local_name <- Some n
  (* END TODO (to be removed) *)

  val debug_printer : Debug_printer.printer = deb
  method debug = debug_printer

  (* pervasive/std module handling *)
  val mutable perva = None
  method register_pervasive (p : Pervasive_handling.std) =
    perva <- Some p
  method get_pervasive =
    match perva with
      | Some p -> p
      | None -> raise Not_found
  method open_pervasive ns =
    match perva with
      | Some std
        -> self#open_in std#get_module_name ns
      | _ -> ()

  (* Subtyping new version *)
  val mutable _subtyper = Obj.magic None

  method subtyper = _subtyper

  initializer
    _subtyper <- new Subtyping.subtyper self

end
