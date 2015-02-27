(** From type checking to type inference and term inference,
    using a monadic representation of types and memoization *)

open Printf
open Nondet.TreeState

(** The simply-typed, implicitly-typed lambda-calculus *)

type var = string

type term =
  | Const of int
  | Var of var
  | Lam of var * term
  | App of term * term

type typ =
  | Int
  | Fun of typ * typ

(* Monadic representation of types *)

type mtyp = mtyp_descr mon
and mtyp_descr = Mint | Mfun of mtyp * mtyp

let mint : mtyp = ret Mint
let mfun (t1: mtyp) (t2: mtyp) = ret (Mfun(t1, t2))

(* Generation of all types *)

let any_mtyp() : mtyp =
  fixmemo (fun any_mtyp -> mint ||| mfun any_mtyp any_mtyp)

(* Conversions between mtyp and typ *)

let rec mtyp_of_typ = function
  | Int -> mint
  | Fun(t1, t2) -> mfun (mtyp_of_typ t1) (mtyp_of_typ t2)

let rec typ_of_mtyp (mt: mtyp) : typ mon =
  mt >>= function
        | Mint -> ret Int
        | Mfun(mt1, mt2) ->
            typ_of_mtyp mt1 >>= fun t1 ->
            typ_of_mtyp mt2 >>= fun t2 -> ret (Fun(t1, t2))

(** Equality between mtyp *)

let rec eq_mtyp (mt1: mtyp) (mt2: mtyp) : unit mon =
  mt1 >>= function
  | Mint -> mt2 >>= (function Mint -> ret ()
                              | _ -> fail)
  | Mfun (mt1a, mt1b) -> mt2 >>= (function Mint -> fail
                                         | Mfun (mt2a, mt2b) ->
                                           eq_mtyp mt1a mt2a >>=
                                           fun () -> eq_mtyp mt1a mt2a)
                               

(** Typing environments: lists of (variable : type) facts. *)

type typenv = (var * mtyp) list 

let type_of_var (env: typenv) (v: var) : mtyp option =
  try Some(List.assoc v env) with Not_found -> None

(** Type checking *)

(** [typeof env a] computes (nondeterministically) the possible type(s)
  for term [a] in environment [env]. *)

let rec typeof (env: typenv) (a: term) : mtyp =
  match a with
    | Const _ -> mint
    | Var s ->
      (match type_of_var env s with
        | Some tau -> tau
        | None -> fail)
    | Lam (x, t) ->
      let tau1 = any_mtyp () in
        mfun tau1 (typeof ((x, tau1) :: env) t)
    | App (t1, t2) ->
      (typeof env t1) >>= (fun tau1 ->
        match tau1 with
          | Mfun (tau1', tau1'') ->
            eq_mtyp tau1' (typeof env t2) >>= fun () -> tau1''
           | _ -> fail)

let types_of_closed_term (a: term) : typ mon = 
  typ_of_mtyp (typeof [] a)

(** Printing of types *)

let rec print_typ = function
  | Fun(t1, t2) ->
      print_typ_0 t1; printf "->"; print_typ t2
  | t ->
      print_typ_0 t

and print_typ_0 = function
  | Int ->
      printf "int"
  | t ->
      printf "("; print_typ t; printf ")"

(** What are the types of [(\x.x) 42] ? *)

let ex1 = types_of_closed_term (App(Lam("x", Var "x"), Const 0))

let _ = print_run print_typ 20 ex1

(** What are the types of [(\x.\y.x) 0] ? *)

let ex2 = types_of_closed_term (App(Lam("x", Lam("y", Var "x")), Const 0))

let _ = print_run print_typ 250 ex2



