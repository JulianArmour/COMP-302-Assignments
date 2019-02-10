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
    match lst with
    | [] -> []
    | hd::tl ->
        if List.mem hd tl then hd::(removeDuplicates (List.filter (fun item -> item <> hd) tl))
        else hd::(removeDuplicates tl)
;;

(* Question 1.5 *)

let countriesInChart (cht: chart) =
  let rec country_list =
    function
    | [] -> []
    | pair::tl ->
        match pair with
        | (ctr1, ctr2) -> ctr1::ctr2::(country_list tl)
  in removeDuplicates (country_list cht)
;;

(* Quesiton 1.6 *)

let colourTheCountries (cht : chart) =
    let countries = countriesInChart cht in
    List.fold_left (fun colouring country -> extColouring cht colouring country) [] countries
;;


(* Question 2 *)

let rec insert comp (item: int) (list: rlist) =
    match !list with
    | Some {data = d; next = nl} ->
        (match !nl with
        | None ->
            if comp (item, d) then
                let oldList = cell2rlist {data = d; next = nl} in
                list := Some {data = item; next = oldList}
            else
                let newTail = cell2rlist {data = item; next = ref None} in
                list := Some {data = d; next = newTail}
        | Some {data = nd; next = _} ->
            if comp (item, d) then
                let oldList = cell2rlist {data = d; next = nl} in
                list := Some {data = item; next = oldList}
            else if comp (item, nd) then
                let newCell = cell2rlist {data = item; next = nl} in
                list := Some {data = d; next = newCell}
            else insert comp item nl)
    | None -> list := Some {data = item; next = ref None}
;;


type cell = { data : int; next : rlist}
and rlist = cell option ref;;

let cell2rlist (c:cell):rlist = ref (Some c);;



let test = {data = 5; next = ref None} ;;
let test2 = {data=10; next = ref None};;
test := Some {data=5; next = cell2rlist test2};;
