open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Fun _ -> true
  | Var _ | App _ -> false

module VarSet = Set.Make(String)
let singleton = VarSet.singleton
let union = VarSet.union
let diff = VarSet.diff
let mem = VarSet.mem

(*
  Free variables of expressions:

  FV(x) = {x}
  FV(e1 e2) = FV(e1) + FV(e2)
    where + means set union
  FV(fun x -> e) = FV(e) - {x}
    where - means set difference
*)

(** [fv e] is a set-like list of the free variables of [e]. *)
let rec fv : expr -> VarSet.t = function
  | Var x -> singleton x
  | App (e1, e2) -> union (fv e1) (fv e2)
  | Fun (x, e) -> diff (fv e) (singleton x)

(** [gensym ()] is a fresh variable name. *)
let gensym =
  let counter = ref 0 in
  fun () ->
    incr counter; "$x" ^ string_of_int !counter

(** [replace e y x] is [e] with the name [x] replaced
    by the name [y] anywhere it occurs. *)
let rec replace e y x = match e with
  | Var z -> if z = x then Var y else e
  | App (e1, e2) -> App (replace e1 y x, replace e2 y x)
  | Fun (z, e') -> Fun ((if z = x then y else z), replace e' y x)

(*
  Substitution rules:

  x{e/x} = e
  y{e/x} = y
  (e1 e2){e/x} = e1{e/x} e2{e/x}

  (fun x -> e'){e/x} = fun x -> e'
  (fun y -> e'){e/x} = fun y -> e'{e/x}
    if y is not in FV(e)
    where FV(e) means the “free variables” of e, and is defined above.
    if y is in FV(e), do replacement defined above, and then substitution.
*)

(** [subst e v x] is [e] with [v] substituted for [x], that
    is, [e{v/x}]. *)
let rec subst e v x = match e with
  | Var y -> if x = y then v else e
  | App (e1, e2) -> App (subst e1 v x, subst e2 v x)
  | Fun (y, e') ->
    if x = y then e
    else if not (mem y (fv v)) then Fun (y, subst e' v x)
    else
      let fresh = gensym () in
      let new_body = replace e' y fresh in
      Fun (fresh, subst new_body v x)

let unbound_var_err = "Unbound variable"
let apply_non_fn_err = "Cannot apply non-function"

(* call-by-value or call-by-name *)
type eval_strategy = CBV | CBN
let strategy = CBV

(** [eval e] is the [e ==> v] relation. *)
let rec eval (e : expr) : expr = match e with
  | Var _ -> failwith unbound_var_err
  | App (e1, e2) -> eval_app e1 e2
  | Fun _ -> e

(*
  call-by-value evaluation rule (OCaml):

  e1 e2 ==> v
    if e1 ==> fun x -> e
    and e2 ==> v2
    and e{v2/x} ==> v

  call-by-name evaluation rule (Haskell):

  e1 e2 ==> v
    if e1 ==> fun x -> e
    and e{e2/x} ==> v
*)

(** [eval_app e1 e2] is the [e] such that [e1 e2 ==> e]. *)
and eval_app e1 e2 = match eval e1 with
  | Fun (x, e) ->
    let e2' =
      match strategy with
      | CBV -> eval e2
      | CBN -> e2
    in subst e e2' x |> eval
  | _ -> failwith apply_non_fn_err

(** [interp s] interprets [s] by parsing
    and evaluating it with the big-step model. *)
let interp (s : string) : expr =
  s |> parse |> eval
