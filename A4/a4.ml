(* Question 1 *)

let rec mapTree f (t: 'a tree) =
  match t with
  | Node (left_t, data, right_t) -> Node (mapTree f left_t, f data, mapTree f right_t)
  | Empty -> Empty
;;

(* Question 2. *)

let rec halfint (f, (epsilon : float)) ((negValue : float), (posValue : float)) =
    let midpoint_x = (posValue +. negValue) /. 2.0 in
    let midpoint_y = f midpoint_x in
    if (abs_float (midpoint_y -. 0.0)) <= epsilon then midpoint_x
    else if midpoint_y > 0.0 then halfint (f, epsilon) (negValue, midpoint_x)
    else halfint (f, epsilon) (midpoint_x, posValue)
;;

(* Question 3. *)

let rec newton (f, (epsilon:float), (dx:float)) (guess:float) =
    let close x y = abs_float (x -. y) < epsilon in
    let improve (guess:float) = guess -. (f guess) /. (deriv (f, dx) guess) in
    if close (f guess) 0.0 then guess
    else newton (f, epsilon, dx) (improve guess)
;;

(* Question 4. *)

let indIntegral (f, (dx:float)) =
    fun x -> integral (f, 0.0, x, dx)
;;
