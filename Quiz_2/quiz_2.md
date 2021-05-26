#Part 2 Proof
```ocaml
let even x = x mod 2 = 0

let rec all_evens (lst: int list) : int list =
  match lst with
  | [] -> []
  | x::xs when even x -> x :: (all_evens xs)
  | _::xs -> all_evens xs

let rec filter (f: 'a -> bool) (lst: 'a list) : 'a list =
  match lst with
  | [] -> []
  | x::xs when f x -> x :: filter f xs
  | _::xs -> filter f xs

let all_evens_f lst = filter even lst

```
```
show: all_evens lst = all_evens_f lst

Principal of Induction: P(lst) holds if P([]) holds and if P(lst') -> P(v :: lst') for any value v

Base Case: all_evens [] = all_evens_f []
Inductive Case: all_evens (y::ys) = all_evens_f (y::ys)
Inductive Hyp: all_evens ys = all_evens_f ys

```
```
# Proof of Base Case

  all_evens []
= []
  by def of all_evens
= filter even []
  by def of filter
= all_evens_f []
  by def of all_evens_f
```
```
#Proof of Inductive Case


When y is even    

all_evens (y::ys)
= y :: (all_evens ys)     
  by def of all_evens    
= y :: (all_evens_f ys)  
  by inductive hyp.   
= y :: (filter even ys)
  by def of all_evens_f
= filter even y::ys
  by def of filter  
= all_evens_f y :: ys
  by def of all_evens_f

When y is not even

  all_evens (y::ys)
= all_evens ys
  by def of all_evens
= all_evens_f ys
  by inductive hyp.
= filter even ys
  by def of all_evens_f
= filter even y::ys
  by assumption that the tail of the list is not empty
=  all_evens_f y::ys
  by def of all_evens_f




  ```
