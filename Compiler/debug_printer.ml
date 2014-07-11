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

(* Facilities for debug printing *)

let null_formatter =
  Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

let none _ = ()

class type printer =
object
  method print : int -> ('a, Format.formatter, unit) format -> 'a
  method locprint :
    int -> Ast.location -> string
      -> ('a, Format.formatter, unit) format -> 'a
  method get_formatter : int -> Format.formatter
  method open_debug : int -> unit
  method close_debug : int -> unit
  method in_print : int -> ('a, Format.formatter, unit) format -> 'a
end

class debug level =
object (self:(#printer as 'self))

  val formatter = Format.err_formatter

  method private getchan = formatter

  method get_printer:
    'a. int -> ('a, Format.formatter, unit) format -> 'a =
    fun n ->
      if n <= level then
        Format.eprintf
      else
        Format.ifprintf self#getchan

  method get_formatter n =
    if n <= level then
      self#getchan
    else null_formatter

  method open_debug n =
    (self#get_printer n) "@[<v 2>DEBUG:@;"
  method close_debug n =
    (self#get_printer n) "@]@,"

  method in_print:
    'a. int -> ('a, Format.formatter, unit) format -> 'a =
    fun n args ->
      (self#get_printer n) args

  method print n fmt =
    (self#get_printer n) ("@[<b 2>DEBUG:@;"^^fmt^^"@]@,");

  method locprint n ((l1,_) as loc) _fname fmt =
    let fname = l1.Lexing.pos_fname in
    let print_loc fname (l1, l2) =
      let line1 = l1.Lexing.pos_lnum in
      let (c1,c2) = (
        l1.Lexing.pos_cnum - l1.Lexing.pos_bol,
        l2.Lexing.pos_cnum - l1.Lexing.pos_bol
      )
      in
        begin
          (self#get_printer n) "@[<b>";
          if fname <> "" then
            (self#get_printer n) "File %S,@;" fname;
          (self#get_printer n) "line %d,@;" line1;
          (self#get_printer n) "characters %d-%d:@]@;" c1 c2
        end
    in
      (self#get_printer n) "@[<v 2>DEBUG:@;";
      print_loc fname loc;
      (self#get_printer n) (fmt^^"@]@,")

end

class debug_file level file =
object (self:(#printer as 'self))
  inherit debug level
  val chan = Format.formatter_of_out_channel (open_out file)

  method private getchan = chan

  method get_printer n =
    if n <= level then
      Format.fprintf chan
    else
      Format.ifprintf chan
end

let factory ?(file) n =
  let tmp =
    match file with
      | None -> new debug n
      | Some f -> new debug_file n f
  in
    (tmp :> printer)
