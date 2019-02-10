(* Question 1.1 *)

let areNeighbours ct1 ct2 (cht : chart) =
    let bordering =
        function
        | (ctr1, ctr2) -> (ctr1 = ct1 && ctr2 = ct2) || (ctr1 = ct2 && ctr2 = ct1)
    in
    let rec search =
        function
        | [] -> false
        | hd :: tl ->
            if bordering hd then true
            else search tl
    in
    search cht
;;

(* Question 1.2 *)

let canBeExtBy (col:colour) (ct: country) (ch : chart) =
    List.for_all (fun coloured_ctr -> (areNeighbours ct coloured_ctr ch) = false ) col
;;

(* Question 1.3 *)

let rec extColouring (cht: chart) (colours : colouring) (cntry : country) =
    match colours with
    | [] -> [[cntry]]
    | colour::tl_colours ->
        if (canBeExtBy colour cntry cht) then (cntry::colour)::tl_colours
        else colour::(extColouring cht tl_colours cntry)
;;

(* Question 1.4 *)

let rec removeDuplicates lst =
  raise NotImplemented
;;

(* Question 1.5 *)

let countriesInChart (cht: chart) =
  raise NotImplemented
;;

(* Quesiton 1.6 *)

let colourTheCountries (cht : chart) =
  raise NotImplemented
;;

(* Question 2 *)

let rec insert comp (item: int) (list: rlist) =
  raise NotImplemented
;;
