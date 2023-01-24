type 'a pointer = 'a ref option

let null : 'a pointer = None
let malloc (x : 'a) : 'a pointer = Some (ref x)

exception Segfault

let deref (ptr : 'a pointer) : 'a =
  match ptr with
  | None -> raise Segfault
  | Some r -> !r

let ( ~* ) = deref

let assign (ptr : 'a pointer) (x : 'a) : unit =
  match ptr with
  | None -> raise Segfault
  | Some r -> r := x

let ( =* ) = assign

(* Dangerous! *)
let address (ptr : 'a pointer) : int =
  match ptr with
  | None -> 0
  | Some r -> Obj.magic r

let ( ~& ) = address

(* example *)
let p = malloc 42
let _ = deref p

(* let _ = deref null *)

let _ = ~*p
let () = assign p 2
let _ = deref p

(* let _ = assign null 0 *)

let () = p =* 3
let _ = ~*p
let _ = ~&p
