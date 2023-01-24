## Syntax for core OCaml in BNF

```
e ::= x | e1 e2 | fun x -> e
    | i | b | e1 bop e2
    | (e1, e2) | fst e | snd e
    | Left e | Right e
    | match e with Left x1 -> e1 | Right x2 -> e2
    | if e1 then e2 else e3
    | let x = e1 in e2

bop ::= + | * | < | =

x ::= <identifiers>

i ::= <integers>

b ::= true | false

v ::= fun x -> e | i | b | (v1, v2) | Left v | Right v
```


## Small-Step evaluation relation rules

We just use call-by-value evaluation strategy.

1. SimPL:

```
e1 + e2 --> e1' + e2
	if e1 --> e1'

v1 + e2 --> v1 + e2'
	if e2 --> e2'

i1 + i2 --> i3
	where i3 is the result of applying primitive operation + to i1 and i2

if e1 then e2 else e3 --> if e1' then e2 else e3
	if e1 --> e1'

if true then e2 else e3 --> e2

if false then e2 else e3 --> e3

let x = e1 in e2 --> let x = e1' in e2
	if e1 --> e1'

let x = v in e2 --> e2{v/x}
```

2. lambda calculus:

```
e1 e2 --> e1' e2
	if e1 --> e1'

v1 e2 --> v1 e2'
	if e2 --> e2'

(fun x -> e) v2 --> e{v2/x}
```

3. pairs:

```
Left e --> Left e'
	if e --> e'

Right e --> Right e'
	if e --> e'
```

4. constructors:

```
Left e --> Left e'
	if e --> e'

Right e --> Right e'
	if e --> e'
```

5. pattern matching:

```
match e with Left x1 -> e1 | Right x2 -> e2
--> match e' with Left x1 -> e1 | Right x2 -> e2
	if e --> e'

match Left v with Left x1 -> e1 | Right x2 -> e2
--> e1{v/x1}

match Right v with Left x1 -> e1 | Right x2 -> e2
--> e2{v/x2}
```

6. substitution (a)

```
i{v/x} = i

b{v/x} = b

(e1 + e2) {v/x} = e1{v/x} + e2{v/x}

(if e1 then e2 else e3){v/x}
 = if e1{v/x} then e2{v/x} else e3{v/x}

(let x = e1 in e2){v/x} = let x = e1{v/x} in e2

(let y = e1 in e2){v/x} = let y = e1{v/x} in e2{v/x}
  if y not in FV(v)

x{v/x} = v

y{v/x} = y

(e1 e2){v/x} = e1{v/x} e2{v/x}

(fun x -> e'){v/x} = (fun x -> e')

(fun y -> e'){v/x} = (fun y -> e'{v/x})
  if y not in FV(v)
```

7. free variables:

```
FV(x) = {x}
FV(e1 e2) = FV(e1) + FV(e2)
FV(fun x -> e) = FV(e) - {x}
FV(i) = {}
FV(b) = {}
FV(e1 bop e2) = FV(e1) + FV(e2)
FV((e1,e2)) = FV(e1) + FV(e2)
FV(fst e1) = FV(e1)
FV(snd e2) = FV(e2)
FV(Left e) = FV(e)
FV(Right e) = FV(e)
FV(match e with Left x1 -> e1 | Right x2 -> e2)
 = FV(e) + (FV(e1) - {x1}) + (FV(e2) - {x2})
FV(if e1 then e2 else e3) = FV(e1) + FV(e2) + FV(e3)
FV(let x = e1 in e2) = FV(e1) + (FV(e2) - {x})
```

8. substitution (b)

```
(e1,e2){v/x} = (e1{v/x}, e2{v/x})

(fst e){v/x} = fst (e{v/x})

(snd e){v/x} = snd (e{v/x})

(Left e){v/x} = Left (e{v/x})

(Right e){v/x} = Right (e{v/x})
```

9. substitution (c)

```
(match e with Left x1 -> e1 | Right x2 -> e2){v/x}
 = match e{v/x} with Left x1 -> e1{v/x} | Right x2 -> e2{v/x}
     if ({x1,x2} intersect FV(v)) = {}

(match e with Left x -> e1 | Right x2 -> e2){v/x}
 = match e{v/x} with Left x -> e1 | Right x2 -> e2{v/x}
     if ({x2} intersect FV(v)) = {}

(match e with Left x1 -> e1 | Right x -> e2){v/x}
 = match e{v/x} with Left x1 -> e1{v/x} | Right x -> e2
      if ({x1} intersect FV(v)) = {}

(match e with Left x -> e1 | Right x -> e2){v/x}
 = match e{v/x} with Left x -> e1 | Right x -> e2
```



## Big-Step evaluation relation rules

```
e1 e2 ==> v
  if e1 ==> fun x -> e
  and e2 ==> v2
  and e{v2/x} ==> v

fun x -> e ==> fun x -> e

i ==> i

b ==> b

e1 bop e2 ==> v
  if e1 ==> v1
  and e2 ==> v2
  and v is the result of primitive operation v1 bop v2

(e1, e2) ==> (v1, v2)
  if e1 ==> v1
  and e2 ==> v2

fst e ==> v1
  if e ==> (v1, v2)

snd e ==> v2
  if e ==> (v1, v2)

Left e ==> Left v
  if e ==> v

Right e ==> Right v
  if e ==> v

match e with Left x1 -> e1 | Right x2 -> e2 ==> v
  if e ==> Left v1
  and e1{v1/x1} ==> v

match e with Left x1 -> e1 | Right x2 -> e2 ==> v
  if e ==> Right v2
  and e2{v2/x2} ==> v

if e1 then e2 else e3 ==> v
  if e1 ==> true
  and e2 ==> v

if e1 then e2 else e3 ==> v
  if e1 ==> false
  and e3 ==> v

let x = e1 in e2 ==> v
  if e1 ==> v1
  and e2{v1/x} ==> v
```



## Evaluation in the environment model

1. lambda calculus:

```
<env, x> ==> v
  if env(x) = v

<env, e1 e2> ==> v
  if  <env, e1> ==> (| fun x -> e, defenv |)
  and <env, e2> ==> v2
  and <defenv[x |-> v2], e> ==> v

<env, fun x -> e> ==> (|fun x -> e, env|)
```

2. constants, ignore the environment:

```
<env, i> ==> i

<env, b> ==> b
```

3. directly use the environment without changing it:

```
<env, e1 + e2> ==> n
  if  <env,e1> ==> n1
  and <env,e2> ==> n2
  and n is the result of applying the primitive operation + to n1 and n2

<env, (e1, e2)> ==> (v1, v2)
  if  <env, e1> ==> v1
  and <env, e2> ==> v2

<env, fst e> ==> v1
  if <env, e> ==> (v1, v2)

<env, snd e> ==> v2
  if <env, e> ==> (v1, v2)

<env, Left e> ==> Left v
  if <env, e> ==> v

<env, Right e> ==> Right v
  if <env, e> ==> v

<env, if e1 then e2 else e3> ==> v2
  if <env, e1> ==> true
  and <env, e2> ==> v2

<env, if e1 then e2 else e3> ==> v3
  if <env, e1> ==> false
  and <env, e3> ==> v3
```

4. binding constructs (i.e., match and let expression):

```
<env, match e with Left x1 -> e1 | Right x2 -> e2> ==> v1
  if  <env, e> ==> Left v
  and <env[x1 |-> v], e1> ==> v1

<env, match e with Left x1 -> e1 | Right x2 -> e2> ==> v2
  if  <env, e> ==> Right v
  and <env[x2 |-> v], e2> ==> v2

<env, let x = e1 in e2> ==> v2
  if  <env, e1> ==> v1
  and <env[x |-> v1], e2> ==> v2
```

## Constraint-based type inference (without let expression)

types:

```
t ::= 'x | int | bool | t1 -> t2
```

1. constants and names:

```
env |- i : int -| {}

env |- b : bool -| {}

env |- n : env(n) -| {}
```

2. if expression:

```
env |- if e1 then e2 else e3 : 't -| C1, C2, C3, C
  if fresh 't
  and env |- e1 : t1 -| C1
  and env |- e2 : t2 -| C2
  and env |- e3 : t3 -| C3
  and C = {t1 = bool, 't = t2, 't = t3}
```

3. anonymous functions:

```
env |- fun x -> e : 't1 -> t2 -| C
  if fresh 't1
  and env, x : 't1 |- e : t2 -| C
```

4. function application:

```
env |- e1 e2 : 't -| C1, C2, C
  if fresh 't
  and env |- e1 : t1 -| C1
  and env |- e2 : t2 -| C2
  and C = {t1 = t2 -> 't}
```

## Solving Constraints

type substitution:

```
int {t / 'x} = int
bool {t / 'x} = bool
'x {t / 'x} = t
'y {t / 'x} = 'y
(t1 -> t2) {t / 'x} =  (t1 {t / 'x} ) -> (t2 {t / 'x} )
```

sequential composition (asscoiative, but bot commutative):

```
t (S1; S2) = (t S1) S2
```

HM type inference example:

```
I |- fun f -> fun x -> f (( + ) x 1) : 'a -> 'b -> 'e -| {'a = 'd -> 'e, 'c = int -> 'd, int -> int -> int = 'b -> 'c}
  I, f:'a |- fun x -> f (( + ) x 1) : 'b -> 'e -| {'a = 'd -> 'e, 'c = int -> 'd, int -> int -> int = 'b -> 'c}
    I, f:'a, x:'b |- f (( + ) x 1) : 'e -| {'a = 'd -> 'e, 'c = int -> 'd, int -> int -> int = 'b -> 'c}
      I, f:'a, x:'b |- f : 'a -| {}
      I, f:'a, x:'b |- (( + ) x) 1 : 'd -| {'c = int -> 'd, int -> int -> int = 'b -> 'c}
        I, f:'a, x:'b |- ( + ) x : 'c -| {int -> int -> int = 'b -> 'c}
          I, f:'a, x:'b |- ( + ) : int -> int -> int -| {}
          I, f:'a, x:'b |- x : 'b -| {}
        I, f:'a, x:'b |- 1 : int -| {}
```

unification:

```
'a = 'd -> 'e
'c = int -> 'd
int -> int -> int = 'b -> 'c
```

```
{'d -> 'e / 'a}

'c = int -> 'd
int -> int -> int = 'b -> 'c
```

```
{'d -> 'e / 'a, int -> 'd / 'c}

int -> int -> int = 'b -> 'c
```

```
{'d -> 'e / 'a, int -> 'd / 'c}

int -> int -> int = 'b -> int -> 'd
```

```
{'d -> 'e / 'a, int -> 'd / 'c}

int = 'b
int -> int = int -> 'd
```

```
{'d -> 'e / 'a, int -> 'd / 'c, int / 'b}

int -> int = int -> 'd
```

```
{'d -> 'e / 'a, int -> 'd / 'c, int / 'b}

int = int
int = 'd
```

```
{'d -> 'e / 'a, int -> 'd / 'c, int / 'b, int / 'd}
```

apply the substitution output by unification to the type inferred by constraint generation:

```
('a -> 'b -> 'e) {('d -> 'e) / 'a}; {(int -> 'd) / 'c}; {int / 'b}; {int / 'd}
=
(('d -> 'e) -> 'b -> 'e) {(int -> 'd) / 'c}; {int / 'b}; {int / 'd}
=
(('d -> 'e) -> 'b -> 'e) {int / 'b}; {int / 'd}
=
(('d -> 'e) -> int -> 'e) {int / 'd}
=
(int -> 'e) -> int -> 'e
```

## Let Polymorphism

```
env |- let x = e1 in e2 : t2 -| C1, C2
  if env |- e1 : t1 -| C1
  and generalize(C1, env, x : t1) |- e2 : t2 -| C2

env |- n : instantiate(env(n)) -| {}
```

generalization:

1. run the unification algorithm on `C1`, get a substitution `S1`.
2. return `u1 = t1 S1` as the inferred type of `e1`.
3. return `env1 = env S1`
4. generalize only variables that **are** in `u1` but are **not** in `env`.
5. As the result, we get `generalize(C1, env, x : t1) = env1, x : 'a1 ... 'an . u1`.

instantiation:

1. given a type scheme `'a1 'a2 ... 'an . t`, choose `n` fresh variables.
2. substituting each of those for `'a1` through `'an` in `t`.

e.g.

```
{} |- let id = fun x -> x in (let a = id 0 in id true): 'e -| 'b -> 'b = int -> 'c, 'd -> 'd = bool -> 'e
  {} |- fun x -> x : 'a -> 'a -| {}
    x : 'a |- x : 'a -| {}
  id : 'a . 'a -> 'a |- let a = id 0 in id true : 'e -| 'b -> 'b = int -> 'c, 'd -> 'd = bool -> 'e
    id : 'a . 'a -> 'a |- id 0 : 'c -| 'b -> 'b = int -> 'c
      id : 'a . 'a -> 'a |- id : 'b -> 'b
      id : 'a . 'a -> 'a |- 0 : int
    id : 'a . 'a -> 'a, a : 'c |- id true : 'e -| 'd -> 'd = bool -> 'e
      id : 'a . 'a -> 'a, a : 'c |- id : 'd -> 'd -| {}
      id : 'a . 'a -> 'a, a : 'c |- true : bool -| {}
```

unification:

```
'b -> 'b = int -> 'c
'd -> 'd = bool -> 'e
```

```
'b = int
'b = 'c
'd = bool
'e = 'd
```

```
'e = bool
```

## Value restriction

OCaml use weak type variables. Omitted here.
