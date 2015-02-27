(** Fun with monadic lists *)

open Printf
open Nondet.TreeState

type 'a mlist = 'a mlist_content mon
and 'a mlist_content = Nil | Cons of 'a * 'a mlist

let nil : 'a mlist = ret Nil
let cons (hd: 'a) (tl: 'a mlist) : 'a mlist = ret (Cons(hd, tl))

(** Generating an arbitrary mlist of booleans *)

let any_bmlist = fix (fun any_bmlist ->
  nil ||| cons false any_bmlist ||| cons true any_bmlist)

(** Testing generation and filtering *)

let isnil (l: 'a mlist) : unit mon =
  l >>= function Nil -> ret () | Cons(_, _) -> fail

let issingleton (l: 'a mlist) : unit mon =
  l >>= function Nil -> fail | Cons(hd, tl) -> isnil tl

let _ =
  printf "-- isnil\n";
  print_run (fun () -> printf "OK!") 1000 (isnil any_bmlist);
  printf "-- issingleton\n";
  print_run (fun () -> printf "OK!") 1000 (issingleton any_bmlist)

(** Operations over monadic lists *)

let rec mlist_of_list (l: 'a list) : 'a mlist =
  match l with
    | [] -> nil
    | x :: l' ->
      cons x (mlist_of_list l')

let rec list_of_mlist (ml: 'a mlist) : 'a list mon =
  ml >>= function Nil -> ret []
                | Cons(x, ml') ->
                  (list_of_mlist ml') >>=
                  fun v -> ret (x :: v)
                    
let rec append (ml1: 'a mlist) (ml2: 'a mlist) : 'a mlist =
  ml1 >>= function Nil -> ml2 | Cons(x, ml1') -> cons x (append ml1' ml2)
 
let rec mlist_eq (ml1: 'a mlist) (ml2: 'a mlist) : unit mon =
  ml1 >>=
  function Nil -> (ml2 >>= function Nil -> ret ()
                                  | _ -> fail)
         | Cons (v1, ml1') ->
           (ml2 >>= function
             | Cons (v2, ml2') when v1=v2 -> mlist_eq ml1' ml2'
             | _ -> fail)

                           
(** Does there exist two mlists that append to the given mlist? *)

let can_split_mlist (ml: bool mlist) : unit mon =
  let ml' = any_bmlist in
    mlist_eq ml (append ml' ml')

let _ =
  printf "-- can_split_mlist\n";
  print_run (fun () -> printf "OK!") 50
    (can_split_mlist (mlist_of_list [true;true;true;false;false]))

(** Adding memoization to the generation of boolean mlists *)


let any_bmlist () : bool mlist = fixmemo (fun any_bmlist ->
 choice [nil; cons false any_bmlist; cons true any_bmlist])

(** Illustrating the effect of memoization. *)

let nil_mlists =
  let l = any_bmlist() in isnil l >>= fun _ -> list_of_mlist l

let singleton_mlists =
  let l = any_bmlist() in issingleton l >>= fun _ -> list_of_mlist l

(** Printing the results *)

let print_bool_list l =
  let rec print = function
  | [] -> ()
  | [b] -> printf "%b" b
  | b :: l -> printf "%b; " b; print l
  in printf "["; print l; printf "]"

let _ =
  printf "-- nil_mlists\n";
  print_run print_bool_list 1000 nil_mlists;
  printf "-- singleton_mlists\n";
  print_run print_bool_list 1000 singleton_mlists

(** All possible ways to split a mlist into the concatenation of two
  mlists.  Returns the two mlists after conversion to normal lists,
  so that we can print the results. *)

let split_mlist (ml: bool mlist) : (bool list * bool list) mon =
  let ml' = any_bmlist () in
  let ml'' = any_bmlist () in
    mlist_eq ml (append ml' ml'') >>=
    fun () ->
    list_of_mlist ml' >>=
    fun l1 ->
    list_of_mlist ml'' >>= fun l2 -> ret (l1, l2)


(** Printing the results *)

let print_2_bool_list (l1, l2) =
  print_bool_list l1; printf ", "; print_bool_list l2

let _ =
  printf "-- split_mlist\n";
  print_run print_2_bool_list 1000
    (split_mlist (mlist_of_list [true;true;true;false;false]))
