let mysqrt (x:float) =
    let rec aux g x =
        let is_close = close ((square g),x) in
        match is_close with
        | true -> g
        | false -> aux ((g +. x /. g) /. 2.0) x
    in
    aux 1.0 x

let g'_cube g x = (2.0 *. g +. x /. (g ** 2.0)) /. 3.0

let cube_root (x:float) =
    let rec aux g x =
        let is_close = close ((cube g),x) in
        match is_close with
        | true -> g
        | false -> aux (g'_cube g x) x
    in
    aux 1.0 x

let fast_exp (base, power) =
    let rec aux factor base power =
        match power with
        | 0 -> 1
        | 1 -> factor * base
        | _ when odd power -> aux (factor * base) (base * base) ((power - 1) / 2)
        | _ -> aux factor (base * base) (power / 2)
    in
    aux 1 base power
