
type 'a btree = Nil
              | Leaf of 'a
              | Fork of 'a btree * 'a * 'a btree

(*insert_by function that inserts an element into a binary tree *)
let rec insert_by (compare: 'a -> 'a -> int) (elem: 'a) (tree: 'a btree) : 'a btree =
    match tree with
    | Nil -> Leaf elem
    |Leaf a-> if elem = a then Leaf a else if compare elem a > 0 then Fork (Nil,a, Leaf elem)
          else Fork(Leaf elem, a, Nil)
    |Fork(left, root, right) -> if elem = root then Fork (left, root, right) else if (compare elem root) > 0
                        then Fork (left,root, insert_by compare elem right)
                        else Fork(insert_by compare elem left , root, right)

(*from_list function that takes a list and adds it to a tree *)
let from_list (compare: 'a -> 'a -> int) (lst: 'a list): 'a btree =
    List.fold_left(fun acc x -> insert_by compare x acc) Nil lst


  (*Credit to October 7 Lecture Examples for reduce (although I altered it to
    convert a tree into a list of its values) *)
let rec reduce (b: 'b) (f:'b->'a->'b->'b) (tree: 'a btree): 'a list =
    match tree with
    |Nil-> b
    |Leaf a -> [a]
    |Fork(x,y,z)-> f (reduce b f x) (y) (reduce b f z)

(*to_list function that extracts the elements of a tree in order and returns them in a list. Calls REDUCE
  to convert the tree into a list of its values.*)
let to_list (tree:'a btree) : 'a list =
      reduce [] (fun x y z-> (x@y::z)) tree


(*ASSERT TEST FUNCTIONS *)

let () =
    print_string "Testing part 4 ... " ;
    try
    assert (insert_by compare 4 Nil = Leaf 4);
    assert (insert_by compare 2 (insert_by compare 4 Nil) =
          Fork (Leaf 2, 4, Nil));
    assert (insert_by compare 4 (insert_by compare 2 Nil) =
            Fork (Nil, 2, Leaf 4));
    assert (insert_by compare 4 (insert_by compare 4 Nil) =
              insert_by compare 4 Nil);
    assert (insert_by compare 1 (insert_by compare 0 (Fork(Nil, 1, Leaf 4)))=
              Fork(Leaf 0, 1, Leaf 4));
    assert (insert_by compare 4 (insert_by compare 4 Nil) = Leaf 4);
    assert (from_list compare [4;2;5;3;6;7;8] =
            Fork (Fork (Nil, 2, Leaf 3), 4, Fork (Nil, 5, Fork (Nil, 6, Fork (Nil, 7, Leaf 8)))));
    assert (from_list compare [1;2;3;4] = Fork(Nil, 1, Fork(Nil, 2, Fork(Nil, 3, Leaf 4))));
    assert (from_list compare [1] = (Leaf 1));
    assert (from_list compare [] = Nil);
    assert (List.sort compare [4;2;5;3;6;7;8] = to_list (from_list compare [4;2;5;3;6;7;8]));
    assert (List.sort compare [1;2;3;4;10;8] = to_list (from_list compare [1;2;3;4;10;8]));
    assert (List.sort compare [] = to_list (from_list compare []));
    assert (List.sort compare [1] = to_list (from_list compare [1]));
    assert (List.sort compare [10;9;8;7;6;5] = to_list (from_list compare [10;9;8;7;6;5]));
    print_string "tests passed.\n"
    with
    Assert_failure (file, line, column) ->
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^
                                        ", column " ^ string_of_int column ^ "\n\n\n\n"
                            in print_string msg
