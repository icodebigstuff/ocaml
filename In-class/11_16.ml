let length_a lst =
    let rec accum len lst' =
    match lst' with
    | [] -> len
    | x::xs -> accum (len + 1) xs
    in accum 0 lst

let fib' n =
    let rec f n1 n2  n'=
    if n' = n
    then n2
    else f (n1 + n2) n1 (n' + 1)
  in
  f 1 0 0
