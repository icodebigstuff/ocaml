  (*Samira Osman - osman293@umn.edu , Jeanne Chan - chan1736@umn.edu , David Buyck - buyc0007@umn.edu*)
  (* TODO: No ;; in code to get rid of *)

  (* No changes made to circle_circum_v1 function *)
  let circle_circum_v1 radius = 3.1415 *.radius *.2.0


  (*No changes made to circle_circum_v2 function*)
  let circle_circum_v2 radius =
    let pi = 3.1415
    in
    radius *. 2.0 *.pi

  (* No changes made to product function*)
  let rec product xs = match xs with
    |[] -> 1
    | x::rest ->x * product rest

  (* Changed the returning 0s for when the list is empty or only one element to raise an exception
    This improves  the function because it gets rid of zeros that don't explain the identity of the operation *)
  let rec sum_sqrdiffs xs = match xs with
    |[] -> raise (Failure "sum_sqrdiffs input list needs at least two elements")
    |_::[] -> raise (Failure "sum_sqrdiffs input list needs at least two elements")
    |x::y::[] -> (x-y)*(x-y)
    |x::(y::rest)-> (x-y)*(x-y) + sum_sqrdiffs(y::rest)

  (*No change made to distance function *)
  let distance (x1,y1) (x2, y2) =
    sqrt((x2 -. x1) ** 2.0 +. (y2 -. y1) ** 2.0)

  (* Changed  triangle_perimeter so that the third distance call was on the same line as the others.
    This improves readability and style by eliminating unneeded indentation *)
  let triangle_perimeter (p1, p2) (p3, p4) (p5, p6) =
    distance(p1, p2) (p3, p4) +. distance(p3, p4) (p5, p6) +. distance(p5, p6) (p1, p2)
