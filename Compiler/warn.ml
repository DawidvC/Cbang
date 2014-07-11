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

(* Handling Warning *)

type warnKind =
  | VoidDeref of Ast.location
  | UnsignSignCmp of Ast.location
  | IntegerTooWide of Ast.location
  | FloatTooWide of Ast.location
  | PointerIntCmp of Ast.location
  | StmUnreachable of Ast.location
  | Unknown_qualifier of string * Ast.location
  | Exported_inline of Ast.location
  | Local_override of string * Ast.location
  | Discard_qualifier of string * Ast.location
  | GenWarn of Ast.location * string

let warning fname = function
  | VoidDeref l ->
    Asttools.locerrp fname l;
    Format.eprintf
      "@[<h>WARNING: dereferencing void pointer.@]@,"
  | UnsignSignCmp l
    ->
    begin
      Asttools.locerrp fname l;
      Format.eprintf
        "@[<h>WARNING: mixing signed and unsigned integer.@]@,"
    end
  | IntegerTooWide l
    ->
    begin
      Asttools.locerrp fname l;
      Format.eprintf
        "@[<h>WARNING: integer too wide.@]@,"
    end
  | FloatTooWide l
    ->
    begin
      Asttools.locerrp fname l;
      Format.eprintf
        "@[<h>WARNING: float too wide.@]@,"
    end
  | PointerIntCmp l ->
    Asttools.locerrp fname l;
    Format.eprintf
      "@[<h>WARNING: comparing pointer and integer.@]@,"
  | StmUnreachable l ->
    Asttools.locerrp fname l;
    Format.eprintf
      "@[<h>WARNING: statement may be unreachable.@]@,"
  | Unknown_qualifier (q,l) ->
    Asttools.locerrp fname l;
    Format.eprintf
      "@[<h>WARNING: unknown qualifier %s.@]@," q
  | Exported_inline l ->
    Asttools.locerrp fname l;
    Format.eprintf
      "@[<h>WARNING: inline ignored, exported function can not be inlined !@]@,"
  | Local_override (m,l) ->
    Asttools.locerrp fname l;
    Format.eprintf
      "@[<h>WARNING: method %s is overriden in the same class@]@,"
      m
  | Discard_qualifier (q, l) ->
    Asttools.locerrp fname l;
    Format.eprintf
      "@[<h>WARNING: discarding qualifier %s@]@," q
  | GenWarn (l,m) ->
    Asttools.locerrp fname l;
    Format.eprintf
      "@[<h>WARNING: %s@]@," m
