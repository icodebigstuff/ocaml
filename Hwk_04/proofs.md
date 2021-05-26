# Homework 4: Reasoning about Correctness.

by Samira Osman

## Problem One

```ocaml
let rec prod = function
  | [] -> 1
  | y::ys -> y * prod ys
```
```
Show: prod (l1 @ l2) = prod l1 * prod l2

Principal of Induction:
  P(l2) holds if P([]) holds and if P(l2') -> P(𝓿 :: l2') for any value v

Base Case: prod ([] @ l2) = prod [] * l2
Inductive case: prod ((x::xs) @ l2) = prod (x::xs) * prod l2
Inductive Hypothesis : prod (xs @ l2) =prod xs *  prod l2
```
`Proof of Base Case`
```
prod ([] @ l2) =

= prod l2
  by property of lists and append

= 1 * prod l2
  by arithmetic

= prod [] * prod l2
  by definition of prod
```
`Proof of Inductive case`
```
prod ((x::xs) @ l2)

= prod (x :: (xs @ l2))
  by properties of lists and append

= x * prod (xs @ l2)
  by definition of prod

= x * (prod xs * prod l2)
  by inductive hypothesis

= (x * prod xs) * prod ls
  by associativity of multiplication (arithmetic)

= prod (x::xs) * prod ls
  by definition of prod

```

## Problem Two

```ocaml
let rec sum = function
  | [] -> 0
  | y::ys -> y + sum ys

let rec length = function
  | [] -> 0
  | y::ys -> 1 + length ys

let rec inc_all = function
  | [] -> []
  | y::ys -> (y+1) :: inc_all ys

```
```
Show: sum (inc_all l) = length l + sum l

Principal of Induction:
P(l) holds if P([]) holds and if P(l') -> P(𝓿 :: l') for any value v

Base Case: sum (inc_all []) = length [] + sum []
Inductive Case: sum (inc_all (x::xs)) = length x::xs + sum x::xs
Inductive Hypothesis : sum (inc_all xs) = length xs + sum xs
```
  `Proof of Base Case `
```

 sum (inc_all [])

 = sum([])
  by definition of inc_all

 = 0 + sum ([])
  by arithmetic

 = length [] + sum []
  by definition of length

```
  `Proof of Inductive Case`
```
  sum (inc_all (x::xs))

= sum ((x+1) :: inc_all xs)
    by definition of inc_all

= (x+1) + sum inc_all xs
    by definition of sum

= (x+1) + length xs + sum xs
    by inductive hypothesis

= (1 + length xs) + (x + sum xs)
    by associativity of addition (arithmetic)

= length x::xs + sum x::xs
    by definitions of length and sum

```

## Problem Three

```ocaml
let rec inc_all = function
  | [] -> []
  | y::ys -> (y+1) :: inc_all ys

let rec map f l = match l with
  | [] -> []
  | y::ys -> f y :: map f ys

let inc x = x + 1
```
```
Show: map inc l = inc_all l

Principal of Induction:
P(l) holds if P([]) holds and if P(l') -> P(𝓿 :: l') for any value v

Base Case: map inc [] = inc_all []
Inductive Case : map inc x::xs = inc_all x::xs
Inductive Hypothesis : map inc xs = inc_all xs

```
 `Proof of Base case `

 ```
  map inc []

= []
  by definition of map

= inc_all []
  by definition of inc_all
```
` Proof of Inductive case`
```
  map inc x::xs

  = inc x :: map inc xs
    by definition of map

  = x + 1 :: map inc xs
    by definition of inc

  = (x+1) :: inc_all xs
    by inductive hypothesis

  = inc_all x::xs
    by definition of inc_all

```
## Problem Four

```ocaml
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

let rec to_list (t: 'a tree) : 'a list = match t with
  | Empty -> []
  | Node (v, tl, tr) -> to_list tl @ [v] @ to_list tr

  let rec prod = function
    | [] -> 1
    | y::ys -> y * prod ys

let rec product (t: int tree) : int =
  match t with
  |Empty -> 1
  |Node (v, tl , tr) -> v * product tl * product tr

```
```
Show: prod (to_list t) = product t

Principle of Induction:
P(t) holds if P(Empty) holds and if P(t1) and P(t2) hold then P(Node(v, t1, t2)) holds

Base case: prod(to_list Empty) = product Empty
Inductive case: prod (to_list Node(v, tl, tr)) = product (Node(v, tl, tr))
Inductive Hypothesis : prod (to_list tl) = product tl and prod(to_list tr) = product tr
```
` Proof of Base Case`
```
  prod(to_list Empty)

= prod []
  by definition of to_list

= 1
  by definition of prod

= product Empty
  by definition of product
```

` Proof of Inductive Case `
```
prod (to_list Node(v, tl, tr))

= prod (to_list tl @ [v] @ to_list tr)
  by definition of to_list

= to_list tl * prod ([v] @ to_list tr)
  by definition of prod

= to_list tl * v * prod(to_list tr)
  by definition of prod

= product tl * v * product tr
  by inductive hypothesis

= v * product tl * product tr
  by associativity of multiplication (arithmetic)

= product Node(v, tl, tr)
  by definition of product

```
## Problem Five

```ocaml
let rec size (tree: 'a tree) : int =
    match tree with
    |Empty -> 0
    |Node (root,left,right) -> 1+ size left + size right

let size_r (tree: 'a tree): int =
    reduce (tree) (0)  (fun root left right -> 1 + left + right)

let rec reduce (t: 'a tree) (b: 'b) (f: 'a -> 'b-> 'b ->'b)   : 'b =
    match t with
    | Empty -> b
    | Node (root, left, right) -> f root (reduce left b f) (reduce right b f)

```
```
Show: size t = size_r t

Principle of Induction:
P(t) holds if P(Empty) holds and if P(t1) and P(t2) hold then P(Node(v, t1, t2)) holds

Base Case: size Empty = size_r Empty
Inductive Case: size Node(root, left, right) = size Node(root, left, right)
Inductive Hypothesis : size left = size_r left and size right = size_r right

```

`Proof of Base Case`

```
  size Empty
= 0
  by definition of size
= reduce Empty 0  (fun root left right -> 1 + left + right)
  by definition of reduce
= size_r Empty
  by definition of size_r

```

`Proof of Inductive Case`

```
  size Node(root, left, right)

= 1 + size left + size right
  by definition of size

= 1 + size_r left + size_r right
  by inductive hypothesis

= 1 + reduce (left) (0)  (fun _ left right -> 1 + left + right) + reduce (right) (0)  (fun _ left right -> 1 + left + right)
  by definition of size_r

= (fun _ left right -> 1 + left + right) 0 (reduce (left) (0)  (fun _ left right -> 1 + left + right)) (reduce (right) (0)  (fun _ left right -> 1 + left + right))
  by properties of reduce

= reduce Node(root, left, right) 0 (fun _ left right -> 1 + left + right)
 by definition of reduce

 = size_r Node(root, left, right)
  by definition of size_r

```
