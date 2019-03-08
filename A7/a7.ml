exception NotImplemented;;

type typExp =
  | TypInt
  | TypVar of char
  | Arrow of typExp * typExp
  | Lst of typExp;;

type substitution = (char * typExp) list;;

let te1 = Arrow(TypInt, Arrow(TypVar 'c', TypVar 'a')) ;;
let te3 = Arrow(TypVar 'a',Arrow (TypVar 'b',TypVar 'c')) ;;

(* Question 1.1 *)
let rec occurCheck (v: char) (tau: typExp) : bool =
    match tau with
    | TypInt -> false
    | TypVar c -> if c = v then true else false
    | Arrow (t1, t2) -> (occurCheck v t1) || (occurCheck v t2)
    | Lst e -> occurCheck v e
;;

(* Question 1.2 *)
let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
    match tau2 with
    | TypInt -> TypInt
    | TypVar c -> if c = v then tau1 else TypVar c
    | Arrow (t1, t2) -> Arrow (substitute tau1 v t1, substitute tau1 v t2)
    | Lst e -> Lst (substitute tau1 v e)
;;

(* Question 1.3 *)
let applySubst (sigma: substitution) (tau: typExp) : typExp =
    let substituter sub t1 =
        match sub with
        | (v, t2) -> substitute t2 v t1
    in
    List.fold_right substituter sigma tau
;;

let getType (e: typExp) =
    match e with
    | TypInt -> 0
    | TypVar _ -> 1
    | Arrow _ -> 2
    | Lst _ -> 3
;;

(* Question 2 *)
let rec unify (tau1: typExp) (tau2:typExp) : substitution =
    match (tau1, tau2) with
    | (TypInt, TypInt) -> []
    | (TypVar a, TypVar b) -> if a = b then [] else [(a, TypVar b)]
    | (Arrow (t1, t2), Arrow (t3, t4)) -> let u1 = (unify t1 t3) in (unify (applySubst u1 t2) (applySubst u1 t4)) @ u1
    | (Lst l1, Lst l2) -> unify l1 l2
    | (TypInt, TypVar a) -> [(a, TypInt)]
    | (TypVar a, TypInt) -> [(a, TypInt)]
    | (Arrow (t1, t2), TypVar a) -> if occurCheck a (Arrow (t1, t2)) then failwith "Looping type variable." else [(a, Arrow (t1, t2))]
    | (TypVar a, Arrow (t1, t2)) -> if occurCheck a (Arrow (t1, t2)) then failwith "Looping type variable." else [(a, Arrow (t1, t2))]
    | (Lst l, TypVar a) -> if occurCheck a (Lst l) then failwith "Looping type variable." else [(a, Lst l)]
    | (TypVar a, Lst l) -> if occurCheck a (Lst l) then failwith "Looping type variable." else [(a, Lst l)]
    | _ -> failwith "Expression types do not match."
;;
- : substitution = [('a', TypVar 'c'); ('c', TypVar 'b'); ('a', TypInt)]
- : substitution = [('b', TypInt); ('c', TypVar 'b'); ('a', TypInt)]
