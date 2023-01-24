(** The type of binary operators. *)
type bop =
  | Add
  | Mult
  | Leq   (* less than or equal *)

(** The type of the abstract syntax tree (AST). *)
type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr    (* binary operators *)
  | Let of string * expr * expr   (* let expression *)
  | If of expr * expr * expr      (* if-then-else expression *)
  | Pair of expr * expr

(*
  Syntax in BNF:

  e ::= x | i | b | e1 bop e2
      | if e1 then e2 else e3
      | let x = e1 in e2

  bop ::= + | * | <=

  x ::= <identifiers>

  i ::= <integers>

  b ::= true | false
*)
