(* Hwk 05.  Extend the construts below as specified.
 *)

type value
  = Int of int
  | Bool of bool
  | Ref of value ref
  | Closure of string * expr * value_environment


and value_environment = (string * value) list

and expr
  = Val of value
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Lt  of expr * expr
  | Eq  of expr * expr
  | And of expr * expr
  | Not of expr
  | Let of string * typ * expr * expr
  | Id  of string
  | App of expr * expr
  | Lam of string * typ * expr
  | LetRec of string * typ * expr * expr
  | If of expr * expr * expr

and typ = Int_type
        | Bool_type
        | Func_type of typ * typ

type type_environment = (string * typ option) list

let rec let_func (f: typ) : string =
  match f with
  | Int_type -> "int"
  | Bool_type -> "bool"
  |Func_type (a,b) -> "(" ^let_func a ^ " -> " ^ let_func b ^")"

(* Part 1. Complete unparse *)
let rec unparse (e: expr) : string =
  match e with
  | Val (Int i) -> string_of_int i
  | Val (Bool b) -> string_of_bool b
  | Val(Closure _) -> "<fun>"
  | Val (Ref _) -> "reference"
  | Id i -> i
  | LetRec (_, _, _,_) -> "<fun>"
  | Add (x, y) -> "(" ^ (unparse x) ^ " + " ^ (unparse y) ^ ")"
  | Mul (x, y) -> "(" ^ (unparse x) ^ " * " ^ (unparse y) ^ ")"
  | Div (x, y) -> "(" ^ (unparse x) ^ " / " ^ (unparse y) ^ ")"
  | Sub (x, y) -> "(" ^ (unparse x) ^ " - " ^ (unparse y) ^ ")"
  | Lt (x,y) -> "(" ^ (unparse x) ^ " < " ^ (unparse y) ^ ")"
  | Eq (x,y) -> "(" ^ (unparse x) ^ " = " ^ (unparse y) ^ ")"
  | And(x,y) -> "(" ^ (unparse x) ^ " && " ^ (unparse y) ^ ")"
  | Not x -> "false"
  | Let(x,y, q, z) -> "(let "^x ^ " : "^ let_func y  ^ " = " ^ (unparse q) ^ " in " ^  (unparse z) ^ ")"
  | Lam (x, y, z) -> "(fun (" ^x ^ " : " ^ let_func y ^ ") ->" ^ (unparse z) ^ ")"
  | App (x, y) -> "(" ^(unparse x) ^" " ^ (unparse y) ^ ")"
  | If (x,y,z) -> "(if " ^ (unparse x) ^ " then " ^ (unparse y) ^ " else " ^ (unparse z) ^ ")"

(* Part 2. Complete freevars *)
let rec freevars (e: expr) : string list =
  match e with
  | Val _ -> []
  | Add (e1, e2) -> freevars e1 @ freevars e2
  | Sub (e1, e2) -> freevars e1 @ freevars e2
  | Mul (e1, e2) -> freevars e1 @ freevars e2
  | Div (e1, e2)-> freevars e1 @ freevars e2
  | Eq (e1, e2) -> freevars e1 @ freevars e2
  | And (e1, e2) -> freevars e1 @ freevars e2
  | Lt (e1, e2) -> freevars e1 @ freevars e2
  | LetRec (x, _, e1, e2) -> List.filter ( (<>) x ) (freevars e1 @ freevars e2)
  | Not e1 -> freevars e1
  | Let (x, _, e1, e2) -> freevars e1 @ (List.filter ((<>) x ) (freevars e2))
  | Id x -> [x]
  | App (e1, e2) -> freevars e1 @ freevars e2
  | Lam (x, _, e2) -> List.filter ((<>) x) (freevars e2)
  | If (e1, e2, e3) -> freevars e1 @ freevars e2 @ freevars e3


(* Part 3. Type checking *)
type result = OK of typ
            | Errs of (expr * string) list

(*Credit to Lecture Notes for expect_int function *)
let expect_int (r: result) (e: expr) : (expr * string) list=
    match r with
    | OK Int_type -> []
    | OK Func_type (a,b)  -> [(e, "expected Int type")]
    | OK Bool_type -> [(e, "expected Int type")]
    | Errs errs -> errs
(*Credit to Lecture Notes for expect_bool function *)
let expect_bool (r: result) (e: expr) : (expr * string) list=
    match r with
    | OK Bool_type -> []
    | OK Func_type (a,b)  -> [(e, "expected Int type")]
    | OK Int_type -> [(e, "expected Bool type")]
    | Errs errs -> errs

(*Credit to Lecture Notes for lookup function *)
let rec lookup (x: string) (env: type_environment) : result  =
    match env with
    | [] -> Errs [(Id x, "Identifier not Found")]
    | (y, None)::ys -> raise(Failure "No type found")
    | (y, Some ty)::ys -> if x = y then OK ty else lookup x ys


let rec type_check (e:expr) (env: type_environment) : result =
    match e with
    | Val (Int _) -> OK Int_type
    | Val (Bool _) -> OK Bool_type
    | Val (Closure _) -> raise(Failure "Not checking for closure")
    | Val (Ref _) -> raise(Failure "Not checking for ref")
    | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2)
      -> (match type_check e1 env, type_check e2 env with
          | OK Int_type, OK Int_type -> OK Int_type
          | r1, r2 -> Errs (expect_int r1 e1 @ expect_int r2 e2))
    | Lt (e1, e2) -> (match type_check e1 env, type_check e2 env with
                      | OK Int_type, OK Int_type -> OK Bool_type
                      | r1, r2 -> Errs (expect_int r1 e1 @ expect_int r2 e2))
    | Eq (e1, e2) -> (match type_check e1 env, type_check e2 env with
                      | OK Int_type, OK Int_type -> OK Bool_type
                      | r1, r2 -> Errs (expect_int r1 e1 @ expect_int r2 e2))
    | And (e1, e2) -> (match type_check e1 env, type_check e2 env with
                      | OK Bool_type, OK Bool_type -> OK Bool_type
                      | r1, r2 -> Errs (expect_bool r1 e1 @ expect_bool r2 e2))
    | Let (x, t1, e1, e2) -> (match type_check e1 env with
                              | Errs errs -> Errs errs
                              | OK ty -> if ty <> t1 then  Errs [( Let(x, t1, e1, e2), "Type Mismatch in Let")]
                                         else type_check e2 ((x,(Some ty))::env))
    | Id x -> lookup x env
    | Not x -> (match (type_check x env) with
                |OK Bool_type -> OK Bool_type
                | _ -> Errs [(x, "expected Bool type")] )
    | App (e1, e2) -> (match type_check e1 env, type_check e2 env with
                      | OK Int_type, _ -> Errs[(e1, "expected function type")]
                      | OK Bool_type, _->  Errs[(e1, "expected function type")]
                      | Errs errs, OK x -> Errs errs
                      | OK y, Errs errs -> Errs errs
                      | OK Func_type (first_type,second_type) , OK any_type->
                        (if first_type = any_type then OK second_type
                        else Errs[(e1, "type mismatch in function application")] )
                      | Errs errs, Errs errs1 -> Errs (errs@errs1) )
    | Lam (x, y, e1) -> (match type_check e1 ([(x, Some y)]@env) with
                        | OK any_type -> OK (Func_type (any_type, y))
                        | Errs errs -> Errs errs )
    | LetRec (x, y, e1, e2) -> (let re1 = (type_check e1 ([(x, Some y)]@env)) in
                                match re1 with
                                | Errs errs -> Errs errs
                                | OK ty -> if ty <> y then  Errs [(e1, "Type Mismatch in LetRec")]
                                           else type_check e2 ((x,(Some ty))::env)
                               )
    | If (e1, e2, e3) -> (match type_check e1 env, type_check e2 env, type_check e3 env with
                        | OK Bool_type, OK Int_type, OK Int_type -> OK Int_type
                        | OK Int_type, _, _ -> Errs [(e1, "expected Bool type")]
                        | OK Func_type (_, _), _, _ -> Errs [(e1, "expected Bool type")]
                        | OK Bool_type, OK Func_type (_,_), _ -> Errs [(e2, "expected Bool or Int type")]
                        | OK Bool_type, _, OK Func_type (_,_)-> Errs [(e3, "expected Bool or Int type")]
                        | OK Bool_type, first_type, second_type ->
                                        if first_type = second_type then first_type
                                        else Errs [(If (e1,e2,e3), "type mismatch in then and else branches of if")]
                        |Errs errs, _, _ -> Errs errs )




let check e = type_check e []


(* Part 4. Evaluation *)


let rec eval (env: value_environment) (e:expr) : value =
  match e with
  | Val v -> v

  | Add (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int v1, Int v2 -> Int (v1 + v2)
       | _ -> raise (Failure "incompatible types, Add")
     )
  | Let (n, t, dexpr, body) ->
      let v = eval env dexpr in
      eval ( (n,v)::env ) body

  | _ -> failwith "complete this function"


let evaluate e = eval [] e


(* some sample expressions *)

let e1 = Add (Val (Int 3), Val (Int 5))
let e2 = Add (Val (Int 3), Val (Bool true))
let e3 = Mul (Val (Bool true), Val (Int 5))
let e4 = Add (e2, e3)

let e5 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
              Add (Id "x", Val (Int 5))
           )

let e6 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
              Lt (Id "x", Val (Int 5))
           )

(* ``let x = 3 < 5 in x && let x = 1 + 2 in x = 3 *)
let e7 = Let ("x", Bool_type,
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "x",
                   Let ("x", Int_type,
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )


(* ``let x = 3 < 5 in y && let x = 1 + 2 in x = 3 *)
let e8 = Let ("x", Bool_type,
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "y",
                   Let ("x", Int_type,
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )

let err_1 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
                 And (Id "x", Val (Bool true))
              )

let err_2 = Let ("x", Int_type, Add (Id "x", Val (Int 4)),
                 And (Id "y", Val (Bool true))
              )

let inc_use = Let ("inc", Func_type (Int_type, Int_type),
                   Lam ("n", Int_type, Add (Id "n", Val (Int 1))),
                   App (Id "inc", Val (Int 3))
                )

let sumToN : expr =
    LetRec ("sumToN", Func_type (Int_type, Int_type),
            Lam ("n", Int_type,
                 If (Eq (Id "n", Val (Int 0)),
                     Val (Int 0),
                     Add (Id "n",
                          App (Id "sumToN",
                               Sub (Id "n", Val (Int 1))
                              )
                       )
                    )
                ),
            Id "sumToN"
           )



let sumTo3 = App (sumToN, Val (Int 4))
