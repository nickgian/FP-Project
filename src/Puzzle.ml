(** The bridge crossing puzzle *)

(* open Naive   (\* or:  open Nondet.Tree *\) *)
open Tree

type band_member = Bono | Edge | Adam | Larry

(** The actions that can be performed: *)
type action =
  | Forth of band_member * band_member  (** two persons walk forth on the bridge *)
  | Back of band_member                 (** one person walks back on the bridge *)

(** A trace of execution is the list of actions performed. *)
type trace = action list

(** This is the time it takes each person to cross the bridge. *)
let walking_time = function
  | Bono -> 1
  | Edge -> 2
  | Adam -> 5
  | Larry -> 10

(** A handy nondeterministic function: it takes a list and returns
  all the ways to decompose it into one element and the list of the
  other elements. *)

let rec take_one (l: 'a list) : ('a * 'a list) mon =
  let rec aux l hl =
    match l with
    | [] -> fail
    | h :: t -> (ret (h, hl @ t)) ||| (aux t (hl @ [h]))
  in
    aux l []

(** Likewise, but decomposes the list in two distinct elements and
  the list of the other elements. *)
(** Considers (x,y,xs) = (y,x,xs). Arbitrary chooses
 (x,y,xs) such that x < y. *)
  
let rec take_two (l: 'a list) : ('a * 'a * 'a list) mon =
  take_one l >>=
    fun (x, xs) ->
    take_one xs >>= fun (y,ys) ->
      if (walking_time x < walking_time y) then ret (x,y,ys) else fail

let trace_cost tr =
  List.fold_left (fun acc a -> match a with
			       | Back x -> acc + (walking_time x)
			       | Forth (x,y) ->
				  acc + (max (walking_time x) (walking_time y))) 0 tr
  
(** The solution to the puzzle! *)

let solution: trace mon =
  let rec aux comp =
    bind comp (fun (tr, left, crossed) ->
	       match tr, left with
	       | _, [] -> ret (tr, [], crossed)
	       | [], _ | ((Back _) :: _), _ ->
		  aux (take_two left >>=
			 fun (x,y,xs) -> ret ((Forth (x,y)) :: tr, xs, x :: y :: crossed))
	       | ((Forth _) :: _), _ ->
		  aux (take_one crossed >>=
			 fun (x,xs) -> ret ((Back x) :: tr, x :: left, xs)))
  in
  aux (ret ([],[Bono;Edge;Adam;Larry],[])) >>=
    (fun (x,y,z) -> if trace_cost x > 17 then fail else ret x)				
		  
(** Printing the solution *)

open Printf

let name_of = function
  | Bono -> "Bono"
  | Edge -> "Edge"
  | Adam -> "Adam"
  | Larry -> "Larry"

let print_action = function
  | Forth(x, y) -> printf "%s and %s go forth" (name_of x) (name_of y)
  | Back(x) -> printf "%s goes back" (name_of x)

let print_trace t =
  List.iter (fun e -> print_action e; printf "; ") t

let _ =
  print_run print_trace 50 solution

  



