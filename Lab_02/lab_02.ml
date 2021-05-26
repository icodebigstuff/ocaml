let circle_circum_v1 radius = 3.1415 *.radius *.2.0


let circle_circum_v2 radius =
  let pi = 3.1415
  in
  radius *. 2.0 *.pi

let rec product xs = match xs with
  |[] -> 1
  | x::rest ->x * product rest

let rec sum_sqrdiffs xs = match xs with
  |[] -> 0
  |_::[] -> 0
  |x::y::[] -> (x-y)*(x-y)
  |x::(y::rest)-> (x-y)*(x-y) + sum_sqrdiffs(y::rest)

let distance (x1,y1) (x2, y2) =
    sqrt((x2 -. x1) ** 2.0 +. (y2 -. y1) ** 2.0)

let triangle_perimeter (p1, p2) (p3, p4) (p5, p6) =
  distance(p1, p2) (p3, p4) +. distance(p3, p4) (p5, p6)
  +.distance(p5, p6) (p1, p2)
