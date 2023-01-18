let rec fact_rec n = if n = 0 then 1 else n * fact_rec (n - 1)

(* recurion without rec *)
let fact0 = ref (fun x : int -> x)
let fact n = if n = 0 then 1 else n * !fact0 (n - 1)
let () = fact0 := fact
