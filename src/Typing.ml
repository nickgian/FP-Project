(** From type checking to type inference and term inference *)

open Printf
open Nondet.Tree

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

(** Enumeration of types *)

let any_typ : typ mon = fix (fun any_typ ->
    ret Int
|||
    (any_typ >>= fun t1 ->
     any_typ >>= fun t2 ->
     ret (Fun(t1, t2))))

(** Typing environments: lists of (variable : type) facts. *)

type typenv = (var * typ) list 

let type_of_var (env: typenv) (v: var) : typ option =
  try Some(List.assoc v env) with Not_found -> None

(** Type checking *)

(** [typeof env a] computes (nondeterministically) the possible type(s)
  for term [a] in environment [env]. *)

let rec typeof (env: typenv) (a: term) : typ mon =
  match a with
    | Const _ -> ret Int
    | Var s ->
      (match type_of_var env s with
        | Some tau -> ret tau
        | None -> fail)
    | Lam (x, t) ->
      any_typ >>= (fun tau1 ->
        (typeof ((x, tau1) :: env) t) >>= (fun tau2 -> ret (Fun (tau1, tau2))))
    | App (t1, t2) ->
      (typeof env t1) >>= (fun tau1 ->
        match tau1 with
          | Fun (tau1', tau1'') ->
            checktype env t2 tau1' >>= (fun () -> ret tau1'')
          | _ -> fail)
              
      
(** [checktype env a t] returns [()] if term [a] has type [t] in
  environment [env], and fails otherwise. *)

and checktype (env: typenv) (a: term) (t: typ) : unit mon =
  match a with
    | Const _ -> if t = Int then ret () else fail
    | Var s -> if (type_of_var env s) = Some t then ret () else fail
    | Lam (x, m) ->
      (match t with
        | Fun (tau1, tau2) ->
          typeof ((x, tau1) :: env) m >>=
          (fun taum -> if taum = tau2 then ret () else fail)
        | _ -> fail)
    | App (m, n) ->
      (typeof env a) >>= (fun tau -> if tau = t then ret () else fail)
                  

let types_of_closed_term a = typeof [] a

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

let _ = print_run print_typ 20 ex2

(** Enumeration of terms *)

let rec any_int_below (n: int) : int mon =
  if n <= 0 then fail else ret (n-1) ||| any_int_below (n-1)

let var_x (n: int) : var = "x" ^ string_of_int n

(** [any_term n] generates terms whose free variables are among
    [var_x 0] ... [var_x (n-1)]. *)
(* terms produced currently have free/captured variables up to n.
   Ask if captured variables can be unbounded and change rule for lambda *)
let any_term : int -> term mon = fixparam (fun any_term n ->
    ret (Const 42)
    |||
    (any_int_below n >>= (fun v -> ret (Var (var_x v))))
    |||
    (any_int_below n >>= (fun v -> (any_term n) >>=
                           (fun t -> ret (Lam ((var_x v), t)))))
    |||
    (any_term n >>= (fun t1 -> (any_term n) >>= (fun t2 -> ret (App (t1, t2))))))

(** Generate closed terms that have type [t]. *)

let closed_terms_of_type t : term mon =
  any_term 10 >>= (fun tm ->
      checktype [] tm t
      >>= fun b -> if b = () then ret tm else fail)
                  
(* Printing of terms *)

let rec print_term = function
  | Lam(x, a) ->
      printf "\\%s. " x; print_term a
  | a ->
      print_term_1 a

and print_term_1 = function
  | App(a, b) ->
      print_term_1 a; printf " "; print_term_0 b
  | a ->
      print_term_0 a

and print_term_0 = function
  | Const n ->
      printf "%d" n
  | Var v ->
      printf "%s" v
  | a ->
      printf "("; print_term a; printf ")"

(* Examples *)

let ex3 = closed_terms_of_type (Fun(Int, Int))

let _ = print_run print_term 25 ex3





