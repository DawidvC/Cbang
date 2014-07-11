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

(* C Qualifiers management *)

module StrSet = Set.Make(String)

let to_set elts =
  List.fold_left (fun s e -> StrSet.add e s) StrSet.empty elts

let known_qualifiers = to_set ["static";"const";"volatile"]
let not_output_qualifiers = to_set ["local"]

let compat l1 l2 =
  let s1 = to_set l1 in
  let s2 = to_set l2 in
    StrSet.elements (StrSet.diff s1 s2)

let fusion l1 l2 =
  let s1 = to_set l1 in
  let s2 = to_set l2 in
    StrSet.elements (StrSet.union s1 s2)

let compat l1 l2 =
  let s1 = to_set l1 in
  let s2 = to_set l2 in
    StrSet.elements (StrSet.diff s1 s2)

let fusion l1 l2 =
  let s1 = to_set l1 in
  let s2 = to_set l2 in
    StrSet.elements (StrSet.union s1 s2)

let gen fmt ns ge loc ql =
  let qs =
    List.fold_left (
      fun set qualifier ->
        let is_output = StrSet.mem qualifier known_qualifiers in
        if not is_output &&
           not (StrSet.mem qualifier not_output_qualifiers)
        then
          ge#emit_warning ns (Warn.Unknown_qualifier (qualifier, loc));
        if is_output then
          StrSet.add qualifier set
        else
          set
    ) StrSet.empty ql
  in
    StrSet.iter (Format.fprintf fmt "%s@;") qs
