let partition f lst=
  let first second (pass, fail) =
    if f second then (second::pass, fail) else (pass, second::fail)
  in
  List.fold_right first lst ([], [])


let group_by_3 lst =
  let accum : 'a list * 'a list list = ([], [])
  in
  let f (elems, groups) elem =
    if List.length elems = 2
    then ([], ((elem::elems) ::groups)) else elem (elem::elems, groups)
  in
  List.fold_left  f accum (List.rev lst)
