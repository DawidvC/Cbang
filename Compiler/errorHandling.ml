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


open Typing
open Asttools
open Sempass

let dont _ = ()

let perror ?(inj=dont) fname l msg =
  Format.eprintf "@[<v>";
  locerrp fname l;
  Format.eprintf "Error: %s" msg;
  inj ();
  Format.eprintf "@]@."

let mod_error fname (l1,l2) =
  let aux l =
    (l.Lexing.pos_lnum, l.Lexing.pos_cnum - l.Lexing.pos_bol)
  in
  let (line1,c1) = aux l1 in
  let (line2,c2) = aux l2 in
  Format.eprintf "@,@[<b>module %s, line " fname;
  if (line1 <> line2) then
    Format.eprintf "%d-%d," line1 line2
  else
    Format.eprintf "%d," line1;
  Format.eprintf " characters %d-%d:@]@;" c1 c2

let string_of_loc l =
  begin
    print_loc Format.str_formatter "" l;
    Format.flush_str_formatter ()
  end

let rec errorpp (debug:Debug_printer.printer) fname = function
  | NotACondition l ->
    perror fname l "expression is not a condition."
  | InvalidBinOp (op,l) ->
    perror fname l
      (Printf.sprintf "invalid use of binary operator %s." op)
  | InvalidOperBinOp (op,l) ->
    perror fname l
      (Printf.sprintf "invalid operand of binary operator %s." op)
  | InvalidUniOp (op,l) ->
    perror fname l
      (Printf.sprintf "invalid use of unary operator %s." op)
  | NotLeftValue l ->
    perror fname l "expression is not a left value."
  | AssignMismatch l ->
    perror fname l "type mismatch in assignement."
  | AssignMismatchExtended (l,t0,t1) ->
    let pp () =
      begin
        Format.eprintf "@,@[<h>Cannot assign value of type@;";
        TypeAlg.pp Format.err_formatter t1;
        Format.eprintf "@;in container of type@;";
        TypeAlg.pp Format.err_formatter t0;
        Format.eprintf "@]";
      end
    in
    perror ~inj:pp fname l "type mismatch in assignement:"
  | NotAssignable (s,l) ->
    perror fname l ("type "^s^" cannot be used for assignement.")
  | BranchTypeMismatch l -> perror fname l "branch type mismatch."
  | BreakOutside l ->
    perror fname l "break outside of a loop or a switch."
  | ContinueOutside l ->
    perror fname l "continue outside of a loop."
  | IndexNotInt (i,l) ->
    perror fname l
      (Printf.sprintf "array indice %d is not an int." i)
  | InvalidArg (i,l) ->
    perror fname l
      (Printf.sprintf "type mismatch for function argument %d." i)
  | NoReturnExp l ->
    perror fname l "return value from a void function."
  | NoSuchField (f,l) ->
    perror fname l (Printf.sprintf "no field %s in boxed value." f)
  | NotAFunction l ->
    perror fname l
      "expression is not a function it cannot be applied."
  | NotAnArray l ->
    perror fname l
      "indexed access to something that is not an array."
  | NotBoxed l ->
    perror fname l
      "field access to something that is not a boxed value."
  | NotEnoughArg l ->
    perror fname l "this function expects more arguments."
  | NotEnoughIndex l ->
    perror fname l "not enough indices for array."
  | NotPointerBoxed l ->
    perror fname l
      "field access to something that is not a boxed pointer."
  | OpAssignMismatch (op,l) ->
    perror fname l
      (Printf.sprintf "invalid use of assignement operator %s." op)
  | PointerNotArray l ->
    perror fname l
      "expression is a pointer not multi-dimensional array."
  | RetTypeMismatch l ->
    perror fname l "return type mismatch."
  | TooManyArg l ->
    perror fname l "too many argument."
  | TooManyIndex l ->
    perror fname l "too many indices for array."
  | UnboundId (ons,x,l) ->
    perror fname l
      (Printf.sprintf "undeclarred identifier %s%s."
	 (match ons with None -> "" | Some ns -> ns^"::")
	 x)
  | SyntaxError l -> perror fname l "syntax error."
  | Bad_namespace (ns, l) ->
    perror fname l
      (match ns with
        | None -> "Using a namespace is not allowed here."
        | Some ns ->
          "Declaring in \"" ^ ns ^ "\" namespace is not allowed here."
      )
  | Bad_type (t,l) ->
    perror fname l (TypeAlg.pp Format.str_formatter t;
		    Format.flush_str_formatter ())
  | Not_found_module (m,l) ->
    perror fname l ("Couldn't find module "^m^" in search path.")
  | ModuleError (module_name, e, l) ->
    begin
      Format.eprintf "@[<v>";
      locerrp fname l;
      Format.eprintf
	"@[<v 2>Error in imported module: %s@," module_name;
      errorpp debug (module_name^".cb") e;
      Format.eprintf "@]@]@.";
    end
  | Cant_delete l ->
    perror fname l
      "Deleting something that is not an object."
  | Tag_conflict (tag,loc_error,ns_orig,enum_orig,loc_orig) ->
    let pp () =
      Format.eprintf "@,@[<b>Previous definition was in@;";
      mod_error ns_orig loc_orig;
      Format.eprintf "in type ";
      TypeAlg.pp Format.err_formatter enum_orig;
      Format.eprintf ".@]";
    in
    perror ~inj:pp fname loc_error
      (Printf.sprintf
         "enum tag %s redefined. Each tag should be defined once."
         tag)
  | Swtest_incompatible (t1,t2,l) ->
    let pp () =
      Format.eprintf "@[<h>can't switch value of type@;";
      TypeAlg.pp Format.err_formatter t1;
      Format.eprintf "@;with case value of type@;";
      TypeAlg.pp Format.err_formatter t2;
      Format.eprintf ".@]";
    in
    perror ~inj:pp fname l ""

  | Types.Already_defined_typename (tn, None, new_loc) ->
    perror fname new_loc
      ("Already defined typename: " ^ tn ^ ".")
  | Types.Already_defined_typename (tn, Some old_loc, new_loc) ->
    let old_loc = string_of_loc old_loc in
    perror fname new_loc
      ("Already defined typename: "^tn^" (here: "^old_loc^").")
  | Typename_not_exported (ns, tn, l) ->
    perror fname l
      ("Typename "^tn^" is not exported in the "^ns^" namespace.")
  | Symbol_not_exported (ns, sn, l) ->
    perror fname l
      ("Identifier "^sn^" is not exported in the "^ns^" namespace.")
  | Already_defined_symbol (sn, None, new_loc) ->
    perror fname new_loc
      ("Already defined symbol: " ^ sn ^ ".")
  | Already_defined_symbol (sn, Some old_loc, new_loc) ->
    let old_loc = string_of_loc old_loc in
    perror fname new_loc
      ("Already defined symbol: " ^sn^ " (here: " ^old_loc^ ").")
  | Lexer.Unfinished_comment_at_eof l ->
    perror fname l "Comment not terminated"
  | Redefine_field (f,l) ->
    perror fname l
      ("a field named "^f
       ^" already exits in current struct, union or class")
  | Unknown_type_name (t,l) ->
    perror fname l
      ("cannot find any definition for typename "^t)
  | No_such_namespace (n,l) ->
    perror fname l ("unbound namespace "^n)
  | Local_name_out_of_scope loc ->
    perror fname loc "\"local\" identifier cannot be used here"
  | Local_not_array_nor_obj loc ->
    perror fname loc
      "\"local\" only applies to objects or arrays of objects"
  | Local_not_dynarray loc ->
    perror fname loc "\"local\" works only with dynamic arrays"
  | Abstract_class_no_instance (c,l) ->
    perror fname l
      ("class " ^
          c ^ " is abstract, it can't be instantiated.")
  | Abstract_class_no_constructor (c,l) ->
    perror fname l
      ("class " ^ c
       ^ " is abstract, it can't have constructor nor destructor.")
  | TypedefError (name,l) ->
    perror fname l
      ("error in definition of type "^name^".")
  | Method_interface_must_empty l ->
    perror fname l
      "methods in interface must be empty."
  | WrongFieldKind l ->
    perror fname l
      "this kind of field is not possible here."
  | Macrobox_call_not_left_val l ->
    perror fname l "object in macro class call must be a left value."
  | Box.Local_override (m,l) ->
    perror fname l
      ("method "^m^" is overriden in the same class definition")

  (* Interface *)
  | Box.Interface_missing (i,m,loc) ->
    perror fname loc
      ("method " ^m^ " required by interface " ^i^ " is missing.")
  | Box.Implem_break_interface (i,m,ti,t,loc) ->
    perror fname loc
      ("method " ^m^ " required by interface " ^i^ " has type "
       ^ (TypeAlg.pp Format.str_formatter ti;
          Format.flush_str_formatter ())
       ^ " instead of type "
       ^ (TypeAlg.pp Format.str_formatter t;
          Format.flush_str_formatter ())
      )
  | Box.Interface_support_conflict (i1,i2,m,ti,t,loc) ->
    perror fname loc
      ("Interface " ^i1^ " disagree with interface " ^i2
       ^ " on method " ^m^ ", "
       ^ (TypeAlg.pp Format.str_formatter ti;
          Format.flush_str_formatter ())
       ^ " is not equivalent to "
       ^ (TypeAlg.pp Format.str_formatter t;
          Format.flush_str_formatter ())
      )

  (* Entry point *)
  | Entry_point.Entry_wrong_param (main, loc) ->
    perror fname loc
      ("Entry point's (" ^ main ^ ") parameters don't match.")
  | Entry_point.Entry_wrong_return (main, loc) ->
    perror fname loc
      ("Entry point's (" ^ main ^ ") return type does not match"
       ^ " (should be 32bits signed integer.)")

  (* Debug *)
  | DEBUG_type_issue (t,l) ->
    debug#locprint 0 l fname "%s"
      (TypeAlg.pp Format.str_formatter t;
       Format.flush_str_formatter ())
  | TypeAlg.DEBUG_missing_type_info l ->
    debug#locprint 0 l fname "Type info not available."
  | DEBUG_no_info l ->
    debug#locprint 0 l fname "Missing info"
  | DEBUG_generic_located_error l ->
    debug#locprint 0 l fname "Generic error."
  | e -> raise e
