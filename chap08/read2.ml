let p =
  Lwt.bind
    Lwt_io.(read_line stdin)
    (fun s1 ->
      Lwt.bind
        Lwt_io.(read_line stdin)
        (fun s2 -> Lwt_io.printf "p: %s,%s\n" s1 s2))

open Lwt.Infix

(* [q] is the same as [p], but use infix operator.
   [p >>= c] is the same as [bind p c]. *)
let q =
  Lwt_io.(read_line stdin) >>= fun s1 ->
  Lwt_io.(read_line stdin) >>= fun s2 -> Lwt_io.printf "q: %s,%s\n" s1 s2

(* [r] is the same as [p], but use ppx syntax.
   Require [lwt_ppx] installed with opam, and [(preprocess (pps lwt_ppx))]
   in the [dune] file. *)
let r =
  let%lwt s1 = Lwt_io.(read_line stdin) in
  let%lwt s2 = Lwt_io.(read_line stdin) in
  Lwt_io.printf "r: %s,%s\n" s1 s2

let _ = Lwt_io.printf "Got here first\n"

(* let _ = Lwt_main.run p
   let _ = Lwt_main.run q
   let _ = Lwt_main.run r *)
let u = Lwt.join [ q; p; r ] (* order here does not make sense? *)
let _ = Lwt_main.run u

(* Result: *)
(* output [Got here first] *)
(* input [a] *)
(* input [b] *)
(* input [c] *)
(* input [d] -> output [p: a,d] *)
(* input [e] -> output [q: b,e] *)
(* input [f] -> output [r: c,f] *)
