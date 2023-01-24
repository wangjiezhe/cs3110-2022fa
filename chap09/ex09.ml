(* Exercise: parse [★] *)

(* Exercise: simpl ids [★★] *)
(* [id] in Ocaml allows underscore '_', which in SimPL is not allowed. *)

(* Exercise: times parsing [★★] *)

(* Exercise: infer [★★] *)
(* Run [dune build] and then [opam install .] in [simpl] directory first. *)
let infer s =
  let open Interp.Main in
  s |> parse |> typeof Context.empty

(* tests *)
let () = assert (infer "3110" = TInt)
let () = assert (infer "1 <= 2" = TBool)
let () = assert (infer "let x = 2 in 20 + x" = TInt)

(* Exercise: subexpression types [★★] *)

(* Exercise: typing [★★] *)
(*
  {} |- let x = 0 in if x <= 1 then 22 else 42 : int
    {} |- 0 : int
    x:int |- if x <= 1 then 22 else 42 : int
      x:int |- x <= 1 : bool
        x:int |- x : int
        x:int |- 1 : int
      x:int |- 22 : int
      x:int |- 42 : int
*)

(* Exercise: substitution [★★] *)
(*
  (x + 1){2/x} = 2 + 1
  (x + y){2/x}{3/y} = 2 + 3
  (x + y){1/z} = x + y
  (let x = 1 in x + 1){2/x} = let x = 1 in x + 1
  (x + (let x=1 in x+1)){2/x} = 2 + (let x = 1 in x+1)
  ((let x=1 in x+1) + x){2/x} = (let x=1 in x+1) + 2
  (let x=y in x+1){2/y} = let x=2 in x+1
  (let x=x in x+1){2/x} = let x=2 in x+1
*)

(* Exercise: step expressions [★] *)
(*
  (3 + 5) * 2
  --> (step + operation)
  8 * 2
  --> (step * operation)
  16

  if 2 + 3 <= 4 then 1 + 1 else 2 + 2
  --> (step + operation)
  if 5 <= 4 then 1 + 1 else 2 + 2
  --> (step <= operation)
  if false then 1 + 1 else 2 + 2
  --> (step if expression)
  2 + 2
  --> (step + operation)
  4
*)

(* Exercise: step let expressions [★★] *)
(*
  let x = 2 + 2 in x + x
  --> (step + expression)
  let x = 4 in x + x
  --> (step let expression)
  4 + 4
  --> (step + operation)
  8

  let x = 5 in ((let x = 6 in x) + x)
  --> (step let expression)
  (let x = 6 in x) + 5
  --> (step let expression)
  6 + 5
  --> (step + operation)
  11

  let x = 1 in (let x = x + x in x + x)
  --> (step let expression)
  let x = 1 + 1 in x + x
  --> (step + operation)
  let x = 2 in x + x
  --> (step let expression)
  2 + 2
  --> (step + operation)
  4
*)

(* Exercise: variants [★] *)
(*
  Left (1+2)
  --> (step + operation)
  Left 3

  match Left 42 with Left x -> x+1 | Right y -> y-1
  --> (step match expression)
  42 + 1
  --> (step + operation)
  43
*)

(* Exercise: application [★★] *)
(*
  (fun x -> 3 + x) 2
  --> (step function application)
  3 + 2
  --> (step + operation)
  5

  let f = (fun x -> x + x) in (f 3) + (f 3)
  --> (step let expression)
  ((fun x -> x + x) 3) + ((fun x -> x + x) 3)
  --> (step function application)
  (3 + 3) + ((fun x -> x + x) 3)
  --> (step + operation)
  6 + ((fun x -> x + x) 3)
  --> (step function application)
  6 + (3 + 3)
  --> (step + operation)
  6 + 6
  --> (step + operation)
  12

  let f = fun x -> x + x in
  let x = 1 in
  let g = fun y -> x + f y in
  g 3
  --> (step let expression)
  let x = 1 in
  let g = fun y -> x + (fun x -> x + x) y in
  g 3
  --> (step let expression)
  let g = fun y -> 1 + (fun x -> x + x) y in
  g 3
  --> (step let expression)
  (fun y -> 1 + (fun x -> x + x) y) 3
  --> (step function application)
  1 + (fun x -> x + x) 3
  --> (step function application)
  1 + (3 + 3)
  --> (step + operation)
  1 + 6
  --> (step + operation)
  7

  let f = (fun x -> fun y -> x + y) in
  let g = f 3 in
  (g 1) + (f 2 3)
  --> (step let expression)
  let g = (fun x -> fun y -> x + y) 3 in
  (g 1) + ((fun x -> fun y -> x + y) 2 3)
  --> (step function application)
  let g = fun y -> 3 + y in
  (g 1) + ((fun x -> fun y -> x + y) 2 3)
  --> (step let expression)
  ((fun y -> 3 + y) 1) + ((fun x -> fun y -> x + y) 2 3)
  --> (step function application)
  (3 + 1) + ((fun x -> fun y -> x + y) 2 3)
  --> (step + operation)
  4 + ((fun x -> fun y -> x + y) 2 3)
  --> (step function application)
  4 + ((fun y -> 2 + y) 3)
  --> (step function application)
  4 + (2 + 3)
  --> (step + operation)
  4 + 5
  --> (step + operation)
  9
*)

(* Exercise: omega [★★★] *)
(*
  (fun x -> x x) (fun x -> x x)
  --> (step function application)
  (fun x -> x x) (fun x -> x x)
*)

(* Exercise: pair parsing [★★★] *)
(* Exercise: pair type checking [★★★] *)
(* Exercise: pair evaluation [★★★] *)
(* see simpl/src/{ast.ml,lexer.mll,parser.mly,main.ml} *)

(* Exercise: desugar list [★] *)
(*
   [1; 2; 3] =
   Right (1, Right (2, Right (3, Left 0)))
*)

let not_empty lst =
  match lst with
  | `Left _ -> 0
  | `Right _ -> 1

(* Exercise: generalize patterns [★★★★] *)
(*
  BNF grammer for patterns:

  p ::= i | (p1, p2) | Left p | Right p | x | _

  BNF grammer for expressions:

  e ::= ...
      | match e with | p1 -> e1 | p2 -> e2 | ... | pn -> en

  v =~ p // s
    if v = p s
  pronounced as "[v] matches [p] producing substitution [s]".

  e.g. 2 =~ x // {2/x}
    because 2 = x {2/x}
*)
(*
  evaluation rules for match expressions:

  match e with | p1 -> e1 | p2 -> e2 | ... | pn -> en
  --> match e' with | p1 -> e1 | p2 -> e2 | ... | pn -> en
    if e --> e'

  match v with | p1 -> e1 | p2 -> e2 | ... | pn -> en
  --> match v with | p2 -> e2 | ... | pn -> en
    if there does not exist an s such that v =~ p1 // s

  match v with | p1 -> e1 | p2 -> e2 | ... | pn -> en
  --> e1 s
    if there exists an s such that v =~ p1 // s
*)
(*
  e.g.

  match (1 + 2, 3) with | (1,0) -> 4 | (1,x) -> x | (x,y) -> x + y
  --> match (3, 3) with | (1,0) -> 4 | (1,x) -> x | (x,y) -> x + y
  --> match (3, 3) with | (1,x) -> x | (x,y) -> x + y
  --> match (3, 3) with | (x,y) -> x + y
  --> x + y {3/x}{3/y}
  --> 3 + 3
  --> 6
*)

(* Exercise: let rec [★★★★] *)
(*
  e ::= ...
      | rec f -> e

  rec f -> e  -->  e{(rec f -> e)/f}

  e.g.
  rec f -> f
  --> (step rec)
  f {(rec f -> f)/f}
  --> (substitue)
  rec f -> f
  --> ... (inifite loop)
*)
(*
  desugar

  let rec f = e1 in e2

  to

  let f = rec f -> e1 in e2
*)
(*
  let rec fact = fun x -> if x <= 1 then 1 else x * (fact (x - 1)) in
  fact 3

  is desugared to

  let fact = rec fact -> fun x -> if x <= 1 then 1 else x * (fact (x - 1)) in
  fact 3

  To simplify, we use [F] for
  [rec fact -> fun x -> if x <= 1 then 1 else x * (fact (x - 1))],
  then
  F --> (fun x -> if x <= 1 then 1 else x * (fact (x - 1))){F/fact}
        = fun x -> if x <= 1 then 1 else x * (F (x - 1))

  ...
  --> F 3
  --> fun x -> if x <= 1 then 1 else x * (F (x - 1)) 3
  --> if 3 <= 1 then 1 else 3 * (F (3 - 1))
  --> if false then 1 else 3 * (F (3 - 1))
  --> 3 * (F (3 - 1))
  --> 3 * (fun x -> if x <= 1 then 1 else x * (F (x - 1)) (3 - 1))
  --> 3 * (fun x -> if x <= 1 then 1 else x * (F (x - 1)) 2)
  --> 3 * (if 2 <= 1 then 1 else 2 * (F (2 - 1)))
  --> 3 * (if false then 1 else 2 * (F (2 - 1)))
  --> 3 * (2 * (F (2 - 1)))
  --> 3 * (2 * ((fun x -> if x <= 1 then 1 else x * (F (x - 1))) (2 - 1)))
  --> 3 * (2 * ((fun x -> if x <= 1 then 1 else x * (F (x - 1))) 1))
  --> 3 * (2 * (if 1 <= 1 then 1 else 1 * F (1 - 1)))
  --> 3 * (2 * (if true then 1 else 1 * F (1 - 1)))
  --> 3 * (2 * 1)
  --> 3 * 2
  --> 6
*)

(* Exercise: simple expressions [★] *)
(*
  110 + 3 * 1000           (op rule)
    because 110 ==> 110    (constant rule)
    and 3*1000 ==> 3000    (op rule)
      because 3 ==> 3      (constant rule)
      and 1000 ==> 1000    (constant rule)
      and 3 * 1000 is 3000
    and 110+3300 is 3410

  if 2 + 3 < 4 then 1 + 1 else 2 + 2 ==> 4    (if rule)
    because 2 + 3 < 4 ==> false               (op rule)
      because 2 + 3 ==> 5                     (op rule)
        because 2 ==> 2                       (constant rule)
        and 3 ==> 3                           (constant rule)
        and 2+3 is 5
      and 4 ==> 4                             (constant rule)
      and 5<4 is false
    and 2 + 2 ==> 4                           (op rule)
      because 2 ==> 2                         (constant rule)
      and 2 ==> 2                             (constant rule)
      and 2+2 is 4
*)

(* Exercise: let and match expressions [★★] *)
(*
  let x=0 in 1 ==> 1      (let rule)
    because 0 ==> 0       (constant rule)
    and 1{0/x} ==> 1      (constant rule)

  let x=2 in x+1 ==> 3    (let rule)
    because 2 ==> 2       (constant rule)
    and x+1{2/x} ==> 3    (op rule)
      because x ==> 2     (variable substitution)
      and 1 ==> 1         (constant rule)
      and 2+1 is 3

  match Left 2 with Left x -> x+1 | Right x -> x-1 ==> 3    (match rule)
    because Left 2 = Left 2                                 (constant rule)
    and x+1{2/x} ==> 3                                      (op rule)
      because x ==> 2                                       (variable substitution)
      and 1 ==> 1                                           (constant rule)
      and 2+1 is 3
*)

(* Exercise: closures [★★] *)
(*
  (fun x -> x+1) 2 ==> 3                     (application rule)
    because fun x -> x+1 ==> fun x -> x+1    (anonymous function)
    and 2 ==> 2                              (constant rule)
    and x+1 {2/x} ==> 3                      (op rule)
      because x ==> 2                        (variable substitution)
      and 1 ==> 1                            (constant rule)
      and 2+1 is 3

  let f = fun x -> x+1 in f 2 ==> 3          (let rule)
    because fun x -> x+1 ==> fun x -> x+1    (anonymous function)
    and f 2 {fun x -> x+1/f} ==> 3           (application rule)
      because f ==> fun x -> x+1             (variable substitution)
      and 2 ==> 2                            (constant rule)
      and x + 1 {2/x} ==> 3                  (op rule)
        because x ==> 2                      (variable substitution)
        and 1 ==> 1                          (constant rule)
        and 2+1 is 3
*)

(* Exercise: lexical scope and shadowing [★★] *)
(*
  let x=0 in x + (let x=1 in x) ==> 1        (let rule)
    because 0 ==> 0                          (constant rule)
    and x + (let x=1 in x) {0/x} ==> 1       (op rule)
      because x ==> 0                        (variable substitution)
      and let x=1 in x ==> 1                 (let rule)
        because 1 ==> 1                      (constant rule)
        and x {1/x} ==> 1                    (variable substitution)
      and 0+1 is 1

  let x=1 in let f=fun y -> x in let x=2 in f 0 ==> 2     (let rule)
    because 1 ==> 1                                       (constant rule)
    and let f=fun y -> x in let x=2 in f 0 {1/x} ==> 2    (let rule)
      because fun y -> x ==> fun y -> x                   (anonymous function)
      and let x=2 in f 0 {fun y -> x/f}{1/x} ==> 2        (let rule)
        because 2 ==> 2                                   (constant rule)
        and f 0 {2/x}{fun y -> x/f} ==> 2                 (application rule)
          because f ==> fun y -> x                        (variable substitution)
          and 0 ==> 0                                     (constant rule)
          and x {0/y} ==> 2                               (variable substitution)
*)

(* Exercise: more evaluation [★★] *)
(*
  let x = 2 + 2 in x + x ==> 8
    because 2 + 2 ==> 4
      because 2 ==> 2
      and 2 ==> 2
      and 2+2 is 4
    and x + x {4/x}
      because x ==> 4
      and x ==> 4
      and 4+4 is 8

  let x = 1 in let x = x + x in x + x ==> 4
    because 1 ==> 1
    and let x = x + x in x + x {1/x} ==> 4
      because x + x ==> 2
        because x ==> 1
        and x ==> 1
        and 1+1 is 2
      and x + x {2/x} ==> 4
        because x ==> 2
        and x ==> 2
        and 2 + 2 is 4

  let f = fun x -> fun y -> x + y in let g = f 3 in g 2 ==> 5
    because fun x -> fun y -> x + y ==> fun x -> fun y -> x + y
    and let g = f 3 in g 2 {fun x -> fun y -> x + y / f} ==> 5
      because f 3 ==> fun y -> 3 + y
        because f ==> fun x -> fun y -> x + y
        and 3 ==> 3
        and fun y -> x + y {3/x} ==> fun y -> 3 + y
      and g 2 {fun y -> 3 + y / g} ==> 5
        because g ==> fun y -> 3 + y
        and 2 ==> 2
        and 3 + y {2/y} ==> 5
          because 3 ==> 3
          and y ==> 2
          and 3+2 is 5

  let f = fst ((let x = 3 in fun y -> x), 2) in f 0 ==> 3
    because fst ((let x = 3 in fun y -> x), 2) ==> fun y -> 3
      because ((let x = 3 in fun y -> x), 2) ==> (fun y -> 3, 2)
        because let x = 3 in fun y -> x ==> fun y -> 3
          because 3 ==> 3
          and fun y -> x {3/x} ==> fun y -> 3
        and 2 ==> 2
    and f 0 {fun y -> 3 / f} ==> 3
      because f ==> fun y -> 3
      and 0 ==> 0
      and 3 {0/y} ==> 3
*)

(* Exercise: dynamic scope [★★★] *)
(*
  let x = 5 in
  let f y = x + y in
  let x = 4 in
  f 3
  ==> x + 3
  ==> 4 + 3
  ==> 7

  answer with lexical scope: 8
*)

(* Exercise: more dynamic scope [★★★] *)
(*
  let x = 5 in
  let f y = x + y in
  let g x = f x in
  let x = 4 in
  g 3
  ==> f 3
  ==> x + 3
  ==> 4 + 3
  ==> 7

  answer with lexical scope: 8

  let f y = x + y in
  let x = 3 in
  let y = 4 in
  f 2
  ==> x + 2
  ==> 3 + 2
  ==> 5

  answer with lexical scope: fail with unbound value x
*)

(* Exercise: constraints [★★] *)
(*
  {} |- fun x -> ( + ) 1 x : 'a -> 'c -| 'b = 'a -> 'c, int -> int -> int = int -> 'b
    x:'a |- ( + ) 1 x : 'c -| 'b = 'a -> 'c, int -> int -> int = int -> 'b
      x:'a |- ( + ) 1 : 'b -| int -> int -> int = int -> 'b
        x:'a |- ( + ) : int -> int -> int -| {}
        x:'a |- 1 : int -| {}
      x:'a |- x : 'a -| {}

  {} |- fun b -> if b then false else true : 'a -> 'b -| 'a=bool,'b=bool
    b:'a |- if b then false else true : 'b -| 'a=bool,'b=bool,'b=bool
      b:'a |- b : 'a -| {}
      b:'a |- false : bool -| {}
      b:'a |- true : bool -| {}

  {} |- fun x -> fun y -> if x <= y then y else x : 'a -> 'b -> 'e -| 'd = bool, 'e = 'a, 'e = 'b, 'c = 'b -> 'd, int -> int -> bool = 'a -> 'c
    x:'a |- fun y -> if x <= y then y else x : 'b -> 'e -| 'd = bool, 'e = 'a, 'e = 'b, 'c = 'b -> 'd, int -> int -> bool = 'a -> 'c
      x:'a, y:'b |- if x <= y then y else x : 'e -| 'd = bool, 'e = 'a, 'e = 'b, 'c = 'b -> 'd, int -> int -> bool = 'a -> 'c
        x:'a, y:'b |- ( <= ) x y : 'd -| 'c = 'b -> 'd, int -> int -> bool = 'a -> 'c
          x:'a, y:'b |- ( <= ) x : 'c -| int -> int -> bool = 'a -> 'c
            x:'a, y:'b |- ( <= ) : int -> int -> bool
            x:'a, y:'b |- x : 'a -| {}
          x:'a, y:'b |- y : 'b -| {}
        x:'a, y:'b |- y : 'a -| {}
        x:'a, y:'b |- x : 'b -| {}
*)

(* Exercise: unify [★★] *)
(*
  X = int
  Y = X -> X

  {int/X}
  Y = int -> int

  {int/X}{int->int/Y}
*)

(* Exercise: unify more [★★★] *)
(*
  X -> Y = Y -> Z
       Z = U -> W

  X = Y
  Y = Z
  Z = U -> W

  {X/Y}
  X = Z
  Z = U -> W

  {X/Y}{X/Z}
  X = U -> W

  {X/Y}{X/Z}{U->W/X}
*)

(* Exercise: infer apply [★★★] *)
(*
  let apply f x = f x
  let apply f = fun x -> f x
  let apply = fun f -> fun x -> f x
    {} |- fun f -> fun x -> f x : 'a -> 'b -> 'c -| 'a = 'b -> 'c
      f:'a |- fun x -> f x : 'b -> 'c -| 'a = 'b -> 'c
        f:'a, x:'b |- f x : 'c -| 'a = 'b -> 'c
          f:'a, x:'b |- f : 'a -| {}
          f:'a, x:'b |- x : 'b -| {}

  Solve:
  'a = 'b -> 'c
  Solution:
  {'b->'c/'a}

  apply : 'a -> 'b -> 'c = ('b -> 'c) -> 'b -> 'c
  i.e.
  apply : ('a -> 'b) -> 'a -> 'b
*)

(* Exercise: infer double [★★★] *)
(*
  let double f x = f (f x)
  let double = fun f -> fun x -> f (f x)
  {} |- fun f -> fun x -> f (f x) : 'a -> 'b -> 'd -| 'a = 'c -> 'd, 'a = 'b -> 'c
    f:'a |- fun x -> f (f x) : 'b -> 'd -| 'a = 'c -> 'd, 'a = 'b -> 'c
    f:'a, x:'b |- f (f x) : 'd -| 'a = 'c -> 'd, 'a = 'b -> 'c
      f:'a, x:'b |- f : 'a -| {}
      f:'a, x:'b |- f x : 'c -| 'a = 'b -> 'c
        f:'a, x:'b |- f : 'a -| {}
        f:'a, x:'b |- x : 'b -| {}

  Solve:
  'a = 'c -> 'd
  'a = 'b -> 'c

  {'c->'d/'a}
  'c -> 'd = 'b -> 'c

  'c = 'b
  'd = 'c

  {'c/'b}
  'd = 'c

  {'c/'d}

  Solution:
  {'c->'d/'a}{'c/'b}{'c/'d}

  double : 'a -> 'b -> 'd = ('c -> 'd) -> 'b -> 'd
                          = ('c -> 'd) -> 'c -> 'd
                          = ('c -> 'c) -> 'c -> 'c
  i.e.
  double : ('a -> 'a) -> 'a -> 'a
*)

(* Exercise: infer S [★★★★] *)
(*
  let s x y z = (x z) (y z)
  let s = fun x -> fun y -> fun z -> (x z) (y z)
  {} |- fun x -> fun y -> fun z -> (x z) (y z) : 'a -> 'b -> 'c -> 'f -| 'e = 'd -> 'f, 'a = 'c -> 'e, 'b = 'c -> 'd
    x:'a |- fun y -> fun z -> (x z) (y z) : 'b -> 'c -> 'f -| 'e = 'd -> 'f, 'a = 'c -> 'e, 'b = 'c -> 'd
      x:'a, y:'b |- fun z -> (x z) (y z) : 'c -> 'f -| 'e = 'd -> 'f, 'a = 'c -> 'e, 'b = 'c -> 'd
        x:'a, y:'b, z:'c |- (x z) (y z) : 'f -| 'e = 'd -> 'f, 'a = 'c -> 'e, 'b = 'c -> 'd
          x:'a, y:'b, z:'c |- x z : 'e -| 'a = 'c -> 'e
            x:'a, y:'b, z:'c |- x : 'a -| {}
            x:'a, y:'b, z:'c |- z : 'c -| {}
          x:'a, y:'b, z:'c |- y z : 'd -| 'b = 'c -> 'd
            x:'a, y:'b, z:'c |- y : 'b -| {}
            x:'a, y:'b, z:'c |- z : 'c -| {}

  Solve:
  'e = 'd -> 'f
  'a = 'c -> 'e
  'b = 'c -> 'd

  {'d->'f/'e}
  'a = 'c -> 'd -> 'f
  'b = 'c -> 'd

  Solution:
  {'d->'f/'e}{'c->'d->'f/'a}{'c->'d/'b}

  s : 'a -> 'b -> 'c -> 'f = 'a -> ('c -> 'd) -> 'c -> 'f
                           = ('c -> 'd -> 'f) -> ('c -> 'd) -> 'c -> 'f
  i.e.
  s : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
*)
