module type Stack = sig
  type 'a t

  exception Empty

  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a
  val pop : 'a t -> 'a t
  val size : 'a t -> int
  val to_list : 'a t -> 'a list
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module ListStack : Stack = struct
  type 'a t = 'a list

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false

  let push x s = x :: s

  exception Empty

  let peek = function
    | [] -> raise Empty
    | x :: _ -> x

  let pop = function
    | [] -> raise Empty
    | _ :: s -> s

  let size = List.length
  let to_list = Fun.id

  let pp pp_val fmt s =
    let open Format in
    let pp_break fmt () = fprintf fmt "@," in
    fprintf fmt "@[<v 0>top of stack";
    if s <> [] then fprintf fmt "@,";
    pp_print_list ~pp_sep:pp_break pp_val fmt s;
    fprintf fmt "@,bottom of stack@]"
end

module VariantStack : Stack = struct
  type 'a t =
    | E
    | S of 'a * 'a t

  exception Empty

  let empty = E

  let is_empty = function
    | E -> true
    | _ -> false

  let rec size = function
    | E -> 0
    | S (_, s) -> succ (size s)

  let push x s = S (x, s)

  let peek = function
    | E -> raise Empty
    | S (x, _) -> x

  let pop = function
    | E -> raise Empty
    | S (_, s) -> s

  let rec to_list = function
    | E -> []
    | S (x, s) -> x :: to_list s

  let pp _ _ _ = ()
end

module CustomStack : Stack = struct
  type 'a entry = {
    top : 'a;
    rest : 'a t;
    size : int;
  }

  and 'a t = S of 'a entry option

  exception Empty

  let empty = S None

  let is_empty = function
    | S None -> true
    | _ -> false

  let size = function
    | S None -> 0
    | S (Some { top = _; rest = _; size }) -> size

  let push x s = S (Some { top = x; rest = s; size = size s + 1 })

  let peek = function
    | S None -> raise Empty
    | S (Some { top; rest = _; size = _ }) -> top

  let pop = function
    | S None -> raise Empty
    | S (Some { top = _; rest; size = _ }) -> rest

  let to_list _ = []
  let pp _ _ _ = ()
end
