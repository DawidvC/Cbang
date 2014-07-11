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

(* Managing string printing *)

let escape =
  let h = Hashtbl.create 13 in
  let _ =
    List.iter (fun (n,c) -> Hashtbl.add h n c)
      [
        (7,  'a');
        (8,  'b');
        (9,  't');
        (10, 'n');
        (11, 'v');
        (12, 'f');
        (13, 'r');
        (92, '\\');
        (34, '"');
        (39, '\'');
        (27, 'e');
      ]
  in
    fun c ->
      try
        Printf.sprintf "\\%c" (Hashtbl.find h c)
      with Not_found ->
        if c < 32 || c > 126 then
          Printf.sprintf "\\%o" c
        else
          Printf.sprintf "%c" (Char.chr c)

let char c = "'" ^ (escape (Char.code c)) ^ "'"

let str s =
  let b = Buffer.create (String.length s) in
    Buffer.add_string b "\"";
    String.iter (
      fun c -> Buffer.add_string b (escape (Char.code c))
    ) s;
    Buffer.add_string b "\"";
    Buffer.contents b
