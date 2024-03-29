open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [Env] is module to help with environments, which
    are maps that have strings as keys. *)
module Env = Map.Make(String)

(** [env] is the type of an environment, which maps
    a string to a value. *)
type env = value Env.t

(*
  A closure has two parts:
  - a code part, which contains function [fun x -> e], and
  - an environment part, which contains the environment [env] at the time that
    function was defined.

  A closure is notated as [(| fun x -> e, env |)].
*)

(** [value] is the type of a lambda calculus value.
    In the environment model, that is a closure. *)
and value =
  | Closure of string * expr * env

let unbound_var_err = "Unbound variable"

type scope_rule = Lexical | Dynamic
let scope = Lexical

(*
  Big-step evaluation rules:

  <env, x> ==> env(x)

  <env, i> ==> i

  <env, let x = e1 in e2> ==> v
    if <env, e1> ==> v1
    and <env[x |-> v1], e2> ==> v

  Application rules of dynamic scope (Emacs LISP, LaTeX):

  <env, fun x -> e> ==> fun x -> e

  <env, e1 e2> ==> v
    if <env, e1> ==> fun x -> e
    and <env, e2> ==> v2
    and <env[x |-> v2], e> ==> v

  Application rules of lexical scope:

  <env, fun x -> e> ==> (| fun x -> e, env |)

  <env, e1 e2> ==> v
    if <env, e1> ==> (| fun x -> e, defenv |)
    and <env, e2> ==> v2
    and <defenv[x |-> v2], e> ==> v
*)

(** [eval env e] is the [<env, e> ==> v] relation. *)
let rec eval (env : env) (e : expr) : value = match e with
  | Var x -> eval_var env x
  | App (e1, e2) -> eval_app env e1 e2
  | Fun (x, e) -> Closure (x, e, env)

(** [eval_var env x] is the [v] such that [<env, x> ==> v]. *)
and eval_var env x =
  try Env.find x env with Not_found -> failwith unbound_var_err

(** [eval_app env e1 e2] is the [v] such that [<env, e1 e2> ==> v]. *)
and eval_app env e1 e2 =
  match eval env e1 with
  | Closure (x, e, defenv) -> begin
      let v2 = eval env e2 in
      let base_env_for_body =
        match scope with
        | Lexical -> defenv
        | Dynamic -> env in
      let env_for_body = Env.add x v2 base_env_for_body in
      eval env_for_body e
    end

(** [interp s] interprets [s] by parsing
    and evaluating it with the big-step model,
    starting with the empty environment. *)
let interp (s : string) : value =
  s |> parse |> eval Env.empty
