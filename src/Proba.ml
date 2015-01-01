(** {1 Monads for probability distributions} *)

open Printf

(** {2 The signature of the monad for probability distributions} *)

type prob = float

type 'a distribution = ('a * prob) list

module type PROBA = sig

  type 'a mon

  val ret: 'a -> 'a mon
  val bind: 'a mon -> ('a -> 'b mon) -> 'b mon
  val (>>=): 'a mon -> ('a -> 'b mon) -> 'b mon

  val distr: 'a distribution -> 'a mon
    (** [distr d] chooses one value among zero, one or several,
        with the probabilities indicated in the distribution [d]. *)

  val flip: prob -> bool mon
    (** [flip p] returns the boolean [true] with probability [p]
        and [false] with probability [1-p]. *)

  val uniform: int -> int -> int mon
    (** [uniform lo hi] returns an integer between [lo] and [hi]
        included, with uniform probability. *)

  val choose: prob -> 'a mon -> 'a mon -> 'a mon
    (** [choose p a b] executes like [a] with probability [p]
        and like [b] with probability [1-p]. *)

  val fail: 'a mon
    (** Failure *)

  val observe: bool -> unit mon
    (* [observe b] continues (returning [()]) if [b] is [true]
       and fails if [b] is false. *)

  val run: int -> 'a mon -> 'a distribution * prob
    (* [run maxdepth m] explores the monadic computation [m]
       to maximal depth [m].  It returns a distribution of
       possible values, plus a combined probability for the
       parts of the monadic computation that were not explored
       because they exceed the maximal depth. *)

  val print_run: ('a -> unit) -> int -> 'a mon -> unit
    (* [print_run f maxdepth m] explores [m] like [run maxdepth m],
       then prints the resulting distribution using [f] to print
       individual values. *)
end

(** {2 Auxiliary functions for implementing monads} *)

(** Auxiliary for printing the results of a run *)

let print_run_aux (f: 'a -> unit) ((res, unknown): 'a distribution * prob) =
  List.iter
    (fun (x, p) -> printf "%10g: " p; f x; printf "\n")
    res;
  if unknown > 0.0 then
    printf "%10g: unknown\n" unknown

(** Auxiliary for removing duplicates from a distribution,
    accumulating their combined probabilities. *)

let remove_dups (l: 'a distribution) : 'a distribution =
  let rec remove l accu =
    match l with
    | [] -> accu
    | [xp] -> xp :: accu
    | (x1, p1 as xp1) :: ((x2, p2) :: l2 as l1) ->
        if x1 = x2
        then remove ((x1, p1 +. p2) :: l2) accu
        else remove l1 (xp1 :: accu)
  in List.rev (remove (List.sort (fun (x1,p1) (x2,p2) -> compare x1 x2) l) [])

(** Auxiliary to normalize the probabilities in a distribution
    so that they sum to 1. *)

let normalize ((res, unknown): 'a distribution * prob) =
  let total = 
    List.fold_left (fun tot (x, p) -> tot +. p) unknown res in
  (List.map (fun (x, p) -> (x, p /. total)) res, unknown /. total)

(** {2 The lazy probabilistic choice tree monad} *)

module Tree : PROBA = struct

  type 'a mon = unit -> 'a case distribution
  and 'a case = Val of 'a | Susp of 'a mon

  let ret (x: 'a) : 'a mon = fun () -> [(Val x, 1.0)]

  let rec bind (m: 'a mon) (f: 'a -> 'b mon): 'b mon =
    fun () ->
      match m () with
        | [] -> []
        | h :: t ->
          let v =
            match h with
              | (Val x, p) -> (Susp (f x), p)
              | (Susp m', p) -> (Susp (bind m' f), p)
          in
            v :: [(Susp (bind (fun () -> t) f), 1.0)]
             (* (bind (fun () -> t) f ()) *)
  (*does probability for Susp matter? 
    Probably not, we can quickcheck with eager version*)
                
  let (>>=) = bind

  let fail : 'a mon = fun () -> []

  let observe (b: bool) : unit mon =
    if b then ret () else fail

  let distr (d: 'a distribution) : 'a mon =
    fun () -> List.map (fun x -> (Val (fst x), snd x)) d

  let flip (p: prob) : bool mon =
    distr [(true, p); (false, 1.0 -. p)]

  let uniform (lo: int) (hi: int) : int mon =
    let p = 1. /. (float_of_int (hi - lo + 1)) in
    let rec mk_list i acc =
      let elm = (i, p) in
        if i = lo then List.rev (elm :: acc) else mk_list (i-1) (elm :: acc)
    in
      distr (mk_list hi [])

  let choose (p: prob) (a: 'a mon) (b: 'a mon) : 'a mon =
    fun () -> [(Susp a, p); (Susp b, 1. -. p)]

  let flatten (maxdepth: int) (m: 'a mon) : 'a case distribution =
    let rec flatten_aux i m p =
      match i with
        | 0 ->
          List.map (fun c -> match c with
              | (x, p') -> (x, p' *. p)) (m ())
        | i ->
          List.map (fun c -> match c with
              | (Val x, p') -> [(Val x, p' *. p)]
              | (Susp m', p') -> flatten_aux (i-1) m' (p*.p')) (m ())
          |> List.flatten
    in
      flatten_aux maxdepth m 1.0

  let run (maxdepth: int) (m: 'a mon) : 'a distribution * prob =
    let (values, susp) = List.partition (fun c ->
        match c with (Val _, _) -> true | _ -> false) (flatten maxdepth m) in
      (List.fold_left (fun d c ->
           match c with
             | (Val x, p) -> (x, p) :: d
             | _ -> raise (Failure "")) [] (remove_dups values),
       List.fold_left (fun tp c ->
           match c with
             | (Susp _, p) -> p +. tp
             | (Val _, _) -> raise (Failure "")) 0.0 susp)
    |> normalize

  let print_run f depth m = print_run_aux f (run depth m)

end

