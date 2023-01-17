let odd_divisor x =
  if x < 3 then 1
  else
    let rec search y =
      if y >= x then y (* exceeded upper bound *)
      else if x mod y = 0 then y (* found a divisor! *)
      else search (y + 2)
      (* skip evens *)
    in
    search 3

let is_correct x =
  let a = odd_divisor x in
  a mod 2 <> 0 && x mod a = 0

let test =
  QCheck.(
    Test.make ~name:"TEST ODD_DIVISOR" ~count:1000
      (make ~print:string_of_int
         (* ~collect:string_of_int *)
         Gen.(int_range 1 10000))
      is_correct)

let _ = QCheck_runner.run_tests [ test ]
