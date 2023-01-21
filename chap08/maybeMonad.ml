(* Change +-*/ from [int] operator to [int option] operator *)

(*****************************************************************************
     First solution
 *****************************************************************************)
let plus_opt (x : int option) (y : int option) : int option =
  match (x, y) with
  | None, _ | _, None -> None
  | Some a, Some b -> Some (Stdlib.( + ) a b)

let ( + ) = plus_opt

let minus_opt (x : int option) (y : int option) : int option =
  match (x, y) with
  | None, _ | _, None -> None
  | Some a, Some b -> Some (Stdlib.( - ) a b)

let ( - ) = minus_opt

let mult_opt (x : int option) (y : int option) : int option =
  match (x, y) with
  | None, _ | _, None -> None
  | Some a, Some b -> Some (Stdlib.( * ) a b)

let ( * ) = mult_opt

let div_opt (x : int option) (y : int option) : int option =
  match (x, y) with
  | None, _ | _, None -> None
  | Some a, Some b -> if b = 0 then None else Some (Stdlib.( / ) a b)

let ( / ) = div_opt

(*****************************************************************************
     Second solution
     (apply the absctraction principle and deduplicate the code)
 *****************************************************************************)
let propagate_none (op : int -> int -> int) (x : int option) (y : int option) =
  match (x, y) with
  | None, _ | _, None -> None
  | Some a, Some b -> (try Some (op a b) with _ -> None)

let ( + ) = propagate_none Stdlib.( + )
let ( - ) = propagate_none Stdlib.( - )
let ( * ) = propagate_none Stdlib.( * )
let ( / ) = propagate_none Stdlib.( / )

(*****************************************************************************
     Third solution
     (use monad)
 *****************************************************************************)
module Maybe : Monad.Monad with type 'a t = 'a option = struct
  type 'a t = 'a option

  let return x = Some x

  let bind x f =
    match x with
    | None -> None
    | Some a -> f a

  let ( >>= ) = bind
end

open Maybe

let upgrade_binary op x y =
  x >>= fun a ->
  y >>= fun b ->
  try return (op a b) with _ -> None (* requires monad sig with type *)

let ( + ) = upgrade_binary Stdlib.( + )
let ( - ) = upgrade_binary Stdlib.( - )
let ( * ) = upgrade_binary Stdlib.( * )
let ( / ) = upgrade_binary Stdlib.( / )
