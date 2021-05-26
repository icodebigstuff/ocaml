(*Exercise S3: 1

first tree evaluates to: 12
second tree evaluates to: 7
third tree evaluates to: 27

Exercise S3: 2
  *             +                 *
/   \          /  \              /  \
5     6       3    *            +     6
                  / \         /   \
                4     5      2      4
*)


(* Exercise S3: 3 & 4 *)

type expr = Int of int
            | Add of expr * expr
            | Mul of expr * expr
            | Sub of expr * expr
            | Div of expr * expr
let e1 = Add (Mul (Int 2, Int 4), Int 5)

let rec eval (e: expr) : int =
    match e with
    | Int i -> i
    | Add (e1, e2) -> eval e1  + eval e2
    | Mul (e1, e2) -> eval e1  * eval e2
    | Sub (e1, e2) -> eval e1  - eval e2
    | Div (e1, e2) -> eval e1 / eval e2

(*Exercise S3: 5
(*add a failwith to raise an exception if an invalid input is added *)
