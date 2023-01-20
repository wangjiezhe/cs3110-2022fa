type ('k, 'v) t = ('k * 'v) RbSet.t

let empty : ('k, 'v) t = RbSet.Leaf

let insert (k : 'k) (v : 'v) (m : ('k, 'v) t) : ('k, 'v) t =
  RbSet.insert (k, v) m

let rec find (k : 'k) (m : ('k, 'v) t) : 'v option =
  match m with
  | Leaf -> None
  | Node (_, l, (key, value), r) ->
      if k < key then find k l else if k > key then find k r else Some value
