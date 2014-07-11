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

(* Basic mangling *)

class mangler =
object (self:'self)
  method mangle ns n =
    if ns <> "" then
      "cbang$"^ns^"$"^n
    else n

  method getid = 0

  method build_class_struct ns n =
    "struct s_cb_class_"^(if ns<>"" then self#mangle ns n else n)

  method register_local_name (_:string) = ()

  method vtable_field = "_methods"
  method ivtable_field = "ivtbl"

  method pack = "__attribute__((packed))"

  method build_struct_name ns n =
    "struct s_" ^ (self#mangle ns n)

  method build_enum_name ns n =
    "enum e_" ^ (self#mangle ns n)

  method build_union_name ns n =
    "union u_" ^ (self#mangle ns n)

  method vtbl_name ns n =
    (self#mangle ns n) ^ "_vtbl"

  method build_method_name (ns:string) c m =
    (self#mangle ns c) ^ "$" ^ m ^ "$impl"

  method enum_tag ns tag t =
    "E_" ^ (self#mangle ns t) ^ "$" ^ tag

  method semfun = "cb_swinterface"

  method interface_id ns id =
    "CB_" ^ ns ^ "_INTERFACE_" ^ id

  method interface_vtbl c i =
    "cb$"^ c ^ "$" ^ i ^ "$inter_vtbl"

  val mutable loccount = -1
  method build_local_obj_name name =
    loccount <- loccount + 1;
    Printf.sprintf "cb$%s_static%02d" name loccount

  method interfaces_block ns name = "_interfaces$" ^ ns ^ "$" ^ name
  method interfaces_addr = "_interfaces_addr"

end

class dummang =
object
  inherit mangler
  method mangle ns n = n
end

class localMangler ?(s="") p =
object (self)
  inherit mangler

  val parent:'self option = p

  val lname = Hashtbl.create 53

  val mutable id = 0
  method getid = id
  val mutable count = 0
  method private gid =
    let s = string_of_int count in
      count <- count + 1;
      s
  method private loc_mangle n =
    n^"_l"^(string_of_int id)^""^self#gid^s

  method register_local_name n =
    Hashtbl.add lname n (self#loc_mangle n)

  method private pmangle ns n =
    match parent with
      | Some p -> p#mangle ns n
      | _ -> n

  method mangle ns n =
    try Hashtbl.find lname n with
      | Not_found -> self#pmangle ns n

  initializer
    begin
      begin
        match parent with
          | Some p -> id <- p#getid + 1
          | _ -> ()
      end;
    end

end

let factory ?(suffix="") ?parent () =
  match parent with
    | None ->
        if suffix="nomang" then new dummang
        else new mangler
    | Some p -> new localMangler ~s:suffix p
