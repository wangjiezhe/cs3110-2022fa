(* Header.
   The code here will be copied literally into the generated [lexer.ml].
   Use [open ...] with the same reason as in [parser.mly]. *)
{
open Parser
}

(* Identifiers.
   These are named regular expressions. *)
let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

(* Rules.
   Like BNF.
   [rule] and [parse] are keywords.

   [white { read lexbuf }] means if whitespace is matched, instead of returning
   a token the lexer should just call the read rule again and return whatever
   token results. In other words, whitespace will be skipped.

   The two for [id]s and [int]s use the expression [Lexing.lexeme lexbuf].
   This calls a function [lexeme] defined in the [Lexing] module, and returns
   the string that matched the regular expression. For example, in the [id]
   rule, it would return the sequence of upper and lower case letters that
   form the variable name.

   The [eof] regular expression is a special one that matches the end of the
   file (or string) being lexed. *)
rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "<=" { LEQ }
  | "*" { TIMES }
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }