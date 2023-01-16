module type Queue = sig
  type 'a t

  exception Empty

  val empty : 'a t
  val is_empty : 'a t -> bool
  val enqueue : 'a -> 'a t -> 'a t
  val front : 'a t -> 'a
  val dequeue : 'a t -> 'a t
  val size : 'a t -> int
  val to_list : 'a t -> 'a list
end

module ListQueue : Queue = struct
  (** The list [x1; x2; ...; xn] represents the queue with [x1] at its front,
      followed by [x2], ..., followed by [xn]. *)
  type 'a t = 'a list

  exception Empty

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false

  let enqueue x q = q @ [ x ]

  let front = function
    | [] -> raise Empty
    | x :: _ -> x

  let dequeue = function
    | [] -> raise Empty
    | _ :: q -> q

  let size = List.length
  let to_list = Fun.id
end

module BatchedQueue : Queue = struct
  (** [{o; i}] represents the queue [o @ List.rev i]. For example,
      [{o = \[1; 2\]; i = \[5; 4; 3\]}] represents the queue [1, 2, 3, 4, 5],
      where [1] is the front element. To avoid ambiguity about emptiness,
      whenever only one of the lists is empty, it must be [i]. For example,
      [{o = \[1\]; i = \[\]}] is a legal representation, but
      [{o = \[\]; i = \[1\]}] is not. This implies that if [o] is empty, [i]
      must also be empty. *)
  type 'a t = {
    o : 'a list;
    i : 'a list;
  }

  exception Empty

  let empty = { o = []; i = [] }

  let is_empty = function
    | { o = []; _ } -> true
    | _ -> false

  let enqueue x = function
    | { o = []; _ } -> { o = [ x ]; i = [] }
    | { o; i } -> { o; i = x :: i }

  let front = function
    | { o = []; _ } -> raise Empty
    | { o = h :: _; _ } -> h

  let dequeue = function
    | { o = []; _ } -> raise Empty
    | { o = [ _ ]; i } -> { o = List.rev i; i = [] }
    | { o = _ :: t; i } -> { o = t; i }

  let size { o; i } = List.(length o + length i)
  let to_list { o; i } = o @ List.rev i
end
