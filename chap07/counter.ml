let next_val =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter

let next_val_broken () =
  let counter = ref 0 in
  incr counter;
  !counter
