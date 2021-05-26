type 'a tree = Leaf of 'a
             | Fork of 'a tree * 'a tree

let t : int tree =
    Fork(Leaf 1, Fork(Fork (Leaf 3, Leaf 4), Fork (Leaf 6, Leaf 8)))

let rec sum (tree: int tree) : int =
    match tree with
    | Leaf a -> a
    | Fork (t1, t2) -> sum t1 + sum t2

let rec prod (tree: int tree) : int =
    match tree with
    | Leaf a -> a
    | Fork (t1, t2) -> prod t1 * prod t2

let rec reduce (t: 'a tree) (b: 'a -> 'b) (f:'b-> 'b ->'b)   : 'b =
    match t with
    | Leaf a -> b a
    | Fork (t1, t2) -> f (reduce t1 b f) (reduce t2 b f)


let sumr (tree: int tree) : int =
    reduce (tree) (fun x -> x) (fun t1 t2 -> t1 + t2)

let prodr (tree: int tree) : int =
    reduce (tree) (fun x -> x) (fun t1 t2 -> t1 * t2)

let string_of_int_tree (tree: int tree): string =
  reduce (tree) (fun x -> string_of_int x) (fun t1 t2 ->  t1 ^ t2)



  (*  let () =
      print_string "Quiz Testing " ;
      try
      assert(sum t = 22);
      assert(prod t = 576);
      assert(sumr t = 22);
      assert(sum t = sumr t);
      assert(prod t = prodr t);
    assert(string_of_int_tree (Leaf 4) = "4");
      assert(string_of_int_tree (Fork (Leaf 1, Leaf 23)) = "123");
      assert(string_of_int_tree (Fork (Leaf 1, Fork (Leaf 2, Leaf 3))) = "123");
      print_string "tests passed.\n"
      with
      Assert_failure (file, line, column) ->
      let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^
          ", column " ^ string_of_int column ^ "\n\n\n\n"
      in print_string msg *)
