(** The type of the abstract syntax tree (AST). *)
type expr =
  | Var of string
  | App of expr * expr
  | Fun of string * expr

(*
  Syntax of lambda calculus in BNF:

  e ::= x | e1 e2 | fun x -> e

  v ::= fun x -> e
*)
