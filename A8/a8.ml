type exptree =
  | Var of char
  | Expr of char * exptree * exptree

(* Question 1: Parsing *)
let parse (inputexp: string): exptree =
  let sym = ref inputexp.[0] in
  let cursor = ref 0 in

  let getsym () =
    cursor := !cursor + 1;
    sym := inputexp.[!cursor]
  in

  let rec expr (): exptree =
    let term0 = term () in
    if !sym = '+' then
        begin
            getsym ();
            Expr ('+', term0, expr ())
        end
    else term0

  and term (): exptree =
    let primary0 = primary () in
    if !sym = '*' then
        begin
            getsym ();
            Expr ('*', primary0, term ())
        end
    else primary0

  and primary (): exptree =
    if !sym = '('
    then begin
      getsym ();
      let result = expr () in
      if !sym <> ')' then
        failwith "Mismatched parens"
      else if !cursor = (String.length inputexp) - 1  then
        result
      else begin
        getsym ();
        result
      end
    end
    else
    if isin !sym charSet then
      if !cursor = (String.length inputexp) - 1 then
        Var !sym
      else
        let result = Var !sym in
        begin
          getsym ();
          result
        end
    else
      failwith "In primary"
  in
  expr ()

(* Question 2: Code Generation *)
let tempstore = ref 0

let codegen (e: exptree) =
  let rec helper ((e: exptree), (tag: char)) =
    match e with
    | Var c -> raise NotImplemented
    | Expr(op, l, r) ->
        if raise NotImplemented then
          raise NotImplemented
        else begin
          tempstore := !tempstore + 1;
          (* Your code for dealing with STORE goes here *)
          raise NotImplemented;
          (if (tag = '+') then
             Printf.printf "ADD %i\n" !tempstore
           else
             Printf.printf "MUL %i\n" !tempstore);
          tempstore := !tempstore - 1
        end
  in
  helper (e, '=')
