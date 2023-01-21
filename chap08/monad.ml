module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

(*****************************************************************************
     Sequential Order with the Monad Laws:
     Law 1: [return x >>= f] behaves the same as [f x].
     Law 2: [m >>= return] behaves the same as [m].
     Law 3: [(m >>= f) >>= g] behaves the same as [m >>= (fun x -> f x >>= g)]
 *****************************************************************************)

let inc (x : int) : int option = Some (x + 1)
let dec (x : int) : int option = Some (x - 1)

(* bind *)
let ( >>= ) x op =
  match x with
  | None -> None
  | Some a -> op a

(* composition of monads.
   [x |> (f >=> g)] behaves the same as [g (f x)]. *)
let ( >=> ) f g x = f x >>= fun y -> g y
let id = inc >=> dec

(*****************************************************************************
     Monad Laws with compose operator:
     Law 1: [return >=> f] behaves the same as [f].
     Law 2: [f >=> return] behaves the same as [f].
     Law 3: [(f >=> g) >=> h] behaves the same as [f >=> (g >=> h)]
     i.e. [return] is a left and right identity, and [>=>] is associative.
 *****************************************************************************)
