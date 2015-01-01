(* A game of Bayesian Cluedo *)

open Printf
open Proba.Tree

type suspect = Alice | Bob

type weapon = Gun | Pipe


(*A = 0.3 * 0.97, 0.3 * 0.03,  B = 0.7 * 0.2, 0.7 * 0.8*)

let s = distr [(Alice, 0.3); (Bob, 0.7)]

let whodunnit : suspect mon =
  s >>= fun p ->
  (match p with
    | Alice ->
      distr [(Pipe, 0.97); (Gun, 0.03)] >>= fun w ->
      ret (Alice, w)
    | Bob ->
      distr [(Pipe, 0.2); (Gun, 0.8)] >>= fun w -> ret (Bob, w))

let print_suspect = function
  | Alice -> printf "Alice"
  | Bob   -> printf "Bob"

let _ =
  print_run print_suspect 100 whodunnit

