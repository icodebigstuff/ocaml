type rat = int * int

type op = Add | Sub | Mul | Div

type expr
  = Rat of rat
  | BinOp of expr * op * expr

  type evalError = DivByZero | FacOfNegativeNum | FacOfNonWholeNum

  exception EvalError of evalError

let rat_add (n1,d1) (n2,d2) = (n1 * d2 + n2 * d1, d1 * d2)

let rat_div (n1,d1) (n2,d2) = if n2 = 0 then raise (EvalError DivByZero) else (n1 * d2 , n2 * d1)
let rat_mul (n1,d1) (n2,d2) = (n1 * d2 * n2 * d1, d1 * d2)
let rat_sub (n1,d1) (n2,d2) = (n1 * d2 - n2 * d1, d1 * d2)

let gcd a' b' =
  let a = if a' < 0 then -a' else a' in
  let b = if b' < 0 then -b' else b' in
  let rec gcd' a b =
    if a = 1 || b = 1 then 1
    else
    if a = b
    then a
    else if a < b
    then gcd' a (b-a)
    else gcd' (a-b) b
  in gcd' a b

let rat_simplify (n,d) =
  if n = 0 then (0,1) else
  let gcd_of_n_d = gcd n d
  in  (n / gcd_of_n_d, d / gcd_of_n_d)

let rec eval (e : expr) : rat =
    match e with
    | Rat (e, e1) -> (e,e1)
    | BinOp (e1, Add, e2) -> rat_add (rat_simplify (eval e1)) (rat_simplify (eval e2))
    | BinOp (e1, Mul, e2) -> rat_mul (rat_simplify (eval e1)) (rat_simplify (eval e2))
    | BinOp (e1, Div, e2) -> rat_div (rat_simplify (eval e1))  (rat_simplify (eval e2))
    | BinOp (e1, Sub, e2) -> rat_sub (rat_simplify (eval e1))  (rat_simplify (eval e2))

    let show_evalError : evalError -> string = function
      | DivByZero -> "Division by zero"
      | FacOfNegativeNum -> "Factorial of negative number"
      | FacOfNonWholeNum -> "Factorial of non-whole number"

    let rec show (e: expr) : string =
      let show_op = function
        | Add -> " + "
        | Sub -> " - "
        | Mul -> " * "
        | Div -> " / "
      in
      match e with
        | BinOp (e1, op, e2) -> "(" ^ show e1 ^ show_op op ^ show e2 ^ ")"
        | Rat (n,d) ->
           if d = 1 then string_of_int n
           else "(" ^ string_of_int n ^ "/" ^ string_of_int d ^ ")"

exception FoundExpr of expr


let find_expr (lst: rat list) : expr option =
  let rec build_expr (ce:expr) (rest: rat list) : unit =
    print_endline ("Trying " ^ show ce) ;
    let ce_val : rat option =
      try Some (rat_simplify (eval ce)) with
      | EvalError er -> print_endline (show_evalError er); None
    in
    match ce_val with
    | None -> ()
    | Some v ->
      if v = (24 ,1) && rest = []
      then
        raise (FoundExpr ce)
      else
       (match (rest) with
        | [] -> ()
        | x::xs -> (build_expr (BinOp (Rat x,Div,ce)) xs); (build_expr (BinOp (Rat x,Add,ce)) xs); (build_expr (BinOp (Rat x,Sub,ce)) xs); (build_expr (BinOp (Rat x,Mul,ce)) xs)
        )
  in try (build_expr (Rat ((List.hd  (List.rev lst)))) (List.tl (List.rev lst))); None with
  | FoundExpr result -> Some result
