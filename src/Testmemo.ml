(** Testing memoization on non-recursive data structures *)

open Printf
open Nondet.TreeState

let any_bool : bool mon = ret false ||| ret true

let filter_true (bm: bool mon) : bool mon =
  bm >>= fun bv -> if bv then bm else fail

let filter_both_true (bm1: bool mon) (bm2: bool mon): (bool * bool) mon =
  filter_true bm1 >>= fun bv1 ->
  filter_true bm2 >>= fun bv2 -> ret (bv1, bv2)

let print_bool b = printf "%b" b
let print_2_bool (b1, b2) = printf "%b,%b" b1 b2

type four = | T1 | T2 | T3 | T4

let any_four_2x2 = (ret T3 ||| ret T2) ||| (ret T3 ||| (ret T4 ||| ret T1)) 

let filter_four (fm : four mon) : four mon = 
  fm >>= fun f -> 
  (ret f ||| ret T3) >>= fun f' ->
  (ret f' ||| ret T4) >>= fun f'' ->
  (ret f'' ||| ret T2) >>= fun f''' ->
  (ret f''' ||| ret T1) >>= fun f'''' ->
  if f'''' = T1 || f'''' = T3 then fm else fail

let print_four f = 
  match f with 
    | T1 -> printf "T1"
    | T2 -> printf "T2" 
    | T3 -> printf "T3"
    | T4 -> printf "T4"


let _ =
  printf "-- filter_true without memo\n";
  print_run print_bool 20 (filter_true any_bool);
  printf "-- filter_true with memo\n";
  print_run print_bool 20 (filter_true (memo any_bool));
  printf "-- filter_both_true without memo\n";
  print_run print_2_bool 20 (filter_both_true any_bool any_bool);
  printf "-- filter_true with 2 memo\n";
  print_run print_2_bool 20 (filter_both_true (memo any_bool) (memo any_bool));
  printf "-- filter_true with 1 memo\n";
  print_run print_2_bool 20 (filter_both_true (memo any_bool) any_bool);
    printf "-- deep filter_true with memo\n";
  print_run print_bool 20 (filter_true (memo (any_bool ||| any_bool)));
  printf "-- deep filter_true without memo\n";
  print_run print_bool 20 (filter_true (any_bool ||| any_bool));
    printf "-- filter_four without memo\n";
  print_run print_four 20 (filter_four any_four_2x2);
  printf "-- filter_four with memo\n";
  print_run print_four 20 (filter_four (memo any_four_2x2))
