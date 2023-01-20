let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

let fibm n =
  let memo : int option array = Array.make (n + 1) None in
  let rec f_mem n =
    match memo.(n) with
    | Some result -> (* computed already *) result
    | None ->
        let result = if n < 2 then 1 else f_mem (n - 1) + f_mem (n - 2) in
        (* record in table *)
        memo.(n) <- Some result;
        result
  in
  f_mem n

let memo f =
  let h = Hashtbl.create 11 in
  fun x ->
    try Hashtbl.find h x
    with Not_found ->
      let y = f x in
      Hashtbl.add h x y;
      y

let memo_rec f =
  let h = Hashtbl.create 16 in
  let rec g x =
    try Hashtbl.find h x
    with Not_found ->
      let y = f g x in
      Hashtbl.add h x y;
      y
  in
  g

let fib_memo =
  let fib self n = if n < 2 then 1 else self (n - 1) + self (n - 2) in
  memo_rec fib

module Unmemoized = struct
  type tree =
    | Empty
    | Node of int * tree * tree

  (* Returns optimum fun for t. *)
  let rec party t = max (party_in t) (party_out t)

  (* Returns optimum fun for t assuming the root node of t
   * is included. *)
  and party_in t =
    match t with
    | Empty -> 0
    | Node (v, left, right) -> v + party_out left + party_out right

  (* Returns optimum fun for t assuming the root node of t
   * is excluded. *)
  and party_out t =
    match t with
    | Empty -> 0
    | Node (_, left, right) -> party left + party right
end

module Memoized = struct
  (* This version memoizes the optimal fun value for each tree node. It
     also remembers the best invite list. Each tree node has the name of
     the employee as a string. *)
  type tree =
    | Empty
    | Node of int * string * tree * tree * (int * string list) option ref

  let rec party t : int * string list =
    match t with
    | Empty -> (0, [])
    | Node (_, _, _, _, memo) ->
        begin
          match !memo with
          | Some result -> result
          | None ->
              let infun, innames = party_in t in
              let outfun, outnames = party_out t in
              let result =
                if infun > outfun then (infun, innames) else (outfun, outnames)
              in
              memo := Some result;
              result
        end

  and party_in t =
    match t with
    | Empty -> (0, [])
    | Node (v, name, l, r, _) ->
        let lfun, lnames = party_out l
        and rfun, rnames = party_out r in
        (v + lfun + rfun, (name :: lnames) @ rnames)

  and party_out t =
    match t with
    | Empty -> (0, [])
    | Node (_, _, l, r, _) ->
        let lfun, lnames = party l
        and rfun, rnames = party r in
        (lfun + rfun, lnames @ rnames)
end
