(* Constructing lazy values in OCaml *)

(* Lazy datatypes and functions *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee =
  ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let demand (l: 'a lazee) : 'a =
  force l;
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

(* Streams, using lazy values *)
type 'a stream = Cons of 'a * 'a stream lazee


(* Some examples streams from files developed in class. *)
let rec from n =
  Cons ( n, delay ( fun () -> from (n+1) ) )

let ones =
  let rec mk_ones () = Cons (1, delay ( mk_ones ) )
  in mk_ones ()

let nats = from 1


(* Some helpful functions from files developed in class. *)
let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v

let tail (s :'a stream) : 'a stream = match s with
  | Cons (_, tl) -> demand tl

let rec take (n: int) (s: 'a stream) : 'a list =
  match n with
  | 0 -> []
  | _ -> (match s with
          | Cons (h, t) -> h :: take (n-1) (demand t)
         )

let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) ->
     let rest = delay (fun () -> filter p (demand tl)) in
     if p hd
     then Cons (hd, rest)
     else demand rest

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (h, t) -> Cons (f h, delay (fun () -> map f (demand t)))

let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (h1, t1), Cons (h2, t2) ->
     Cons (f h1 h2, delay (fun () -> zip f (demand t1) (demand t2)))


(* Below is a stream of factorials.  It uses, the same definition of
   factorials as we developed in class except that the built in
   multiplication operator is replaced by a function `mul_p` that
   still multiplies its arguments but prints out those arguments as
   well.  *)

let mul_p x y =
  let () = print_endline ("multiplying " ^ string_of_int x ^ " and " ^
                            string_of_int y ^ ".")
  in x * y

let rec factorials () =
  Cons (1, delay (fun () -> zip mul_p nats (factorials ())))

let facts = factorials ()

let () =
  assert (take 5 facts = [1; 1; 2; 6; 24])



(* Please write your solutions below. *)

let rec cubes_from (n:int) : int stream =
    Cons (n*n*n, delay (fun () -> cubes_from (n+1)))

let cubes_from_map (n:int) : int stream =
  map (fun n -> n*n*n) (from n)

let cubes_from_zip (n: int) : int stream =
  zip (fun h1 _ -> h1*h1*h1) (from n) (from n)

(* facts' has a dummy value in it that has a reference back to nats.
    this allows us to build up the data structure so it always refers back
    to itself rather then have a new call each time.*)
let facts' =
  let dummy = ref nats
  in
  let fact = Cons (1, delay (fun () -> zip mul_p nats (! dummy)))
  in
  let () = dummy := fact
  in
  fact

  let () =
    assert (take 5 facts' = [1; 1; 2; 6; 24])

let sift (n: int) (stream: int stream) : int stream =
    filter (fun x-> x mod n <> 0) stream

let rec sieve (stream: int stream) : int stream =
    match stream with
    | Cons (n, rest) -> Cons (n, delay (fun () -> sieve (sift n (demand rest))))

let primes = sieve (from 2)

let () =
  print_endline "testing sieve";
  assert ( take 10 primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29] );
  print_endline "sieve passed"

let () =
  print_endline "running asserts";
  assert (take 5 (cubes_from 1) = [1; 8; 27; 64; 125]);
  assert (take 5 (cubes_from_map 1) = [1; 8; 27; 64; 125]);
  assert (take 5 (cubes_from_zip 1) = [1; 8; 27; 64; 125]);
  assert (take 3 (cubes_from 3) = [27; 64; 125]);
  assert (take 3 (cubes_from_map 3) = [27; 64; 125]);
  assert (take 3 (cubes_from_zip 3) = [27; 64; 125]);
  print_endline "passed asserts"
