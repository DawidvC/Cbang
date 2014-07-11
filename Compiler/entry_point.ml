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

 (*
  * Entry point management.
  * then « main » function will be manage externaly for conveniences
  * we define a string constant for main name and C name generated.
  *)

open Ast

exception Entry_wrong_param of string * location
exception Entry_wrong_return of string * location
exception Bad_namespace of string option * location

let entry = "main"
let c_entry = "main"

let is_entry ({fname = n}) =
  n = entry

let is_argc = function
  | TypeAlg.Int (false,_,_) -> true
  | _ -> false

let is_argv = function
  | TypeAlg.Pointer (TypeAlg.Pointer (TypeAlg.Char _, _), _)
  | TypeAlg.Array (TypeAlg.Pointer (TypeAlg.Char _, _), _::[],_)
    -> true
  | _ -> false

let check_proto ns ge texpr fd =
  match fd.fparams with
    | [] -> []
    | [(argc,t_argc,_); (argv,t_argv,_)] when
        (is_argc (texpr t_argc))
        && (is_argv (texpr t_argv))
        ->
      [
        (argc,TypeAlg.CInt (TypeAlg.stdmeta ()));
        (argv,TypeAlg.Pointer
          (TypeAlg.Pointer
             (TypeAlg.Char (TypeAlg.stdmeta ()), TypeAlg.stdmeta ())
             , TypeAlg.stdmeta ()
          )
        )
      ]
    | _ ->
      raise (Entry_wrong_param (entry,fd.floc))

let check_return ns ge texpr fd =
  if (ge#get_type_env ns)#is_int false 32L (texpr fd.frtype) then
    TypeAlg.CInt (TypeAlg.stdmeta ())
  else
    raise (Entry_wrong_return (entry,fd.floc))

let buildFunType ptl rt =
  let (_,tl) = List.split ptl in
    TypeAlg.Fun (tl,rt,TypeAlg.stdmeta())

let entry_check ns ge texpr bfuntype fd =
  if is_entry fd then
    let debug = (ge#debug : Debug_printer.printer) in
    let sym_env = ge#get_sym_env ns in
      begin
        debug#locprint 4 fd.floc "" "%s is an entry point." fd.fname;
        let _ =
          match fd.fnspace with
            | [] -> ()
            | n::_ when n = ns -> ()
            | n::_ -> raise (Bad_namespace (Some n, fd.floc))
        in
        let ptl = (check_proto ns ge texpr fd) in
        let rt = (check_return ns ge texpr fd) in
        let ft = buildFunType ptl rt in
          sym_env#add fd.fname
            (Symbol.factory ?asname:(Some c_entry)
               ns fd.fname true ft
               (Some fd.floc)
            );
          fd.finfo <- Some (TypeAlg.info ft true false);
          ft;
      end
  else
    TypeAlg.Error
