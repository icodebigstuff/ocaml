open LazeeModules
module type StreamSig = sig
  type 'a lazee
  val delay: (unit -> 'a) -> 'a lazee
  val demand: 'a lazee -> 'a

  type 'a t = Cons of 'a * 'a t lazee
  val head: 'a t -> 'a
  val tail: 'a t -> 'a t
  val take: int -> 'a t -> 'a list
  val filter: ('a -> bool) -> 'a t -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val zip: ('a -> 'b -> 'c) -> ('a t) -> ('b t) -> ('c t)

end

module Stream (L: LazeeSig) : StreamSig = struct
  type 'a lazee = 'a L.t
  let delay = L.delay
  let demand = L.demand

type 'a t= Cons of 'a * 'a t lazee
  let head s = match s with
      | Cons (h, _) -> h
  let tail s = match s with
      | Cons (_, tail) -> demand tail

  let rec take n s  =
      match n with
      | 0 -> []
      | _ -> (match s with
                | Cons (h, tail) -> h :: take (n-1) (demand tail))

  let rec filter f s =
      match s with
      | Cons (h, tail) ->
                    let rest = delay (fun () -> filter f (demand tail)) in
                    if f h then Cons (h, rest) else demand rest
  let rec map f s =
      match s with
      | Cons (h, tail) -> Cons (f h, delay (fun () -> map f (demand tail) ) )

  let rec zip f s1 s2  =
      match s1, s2 with
      | Cons (h1, t1), Cons (h2, t2) ->
              Cons (f h1 h2, delay (fun () -> zip f (demand t1) (demand t2)))

end
