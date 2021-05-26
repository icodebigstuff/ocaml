
(*A tree type declaration *)
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

(* ASSERT TESTING TREES *)

let ints_tree: int list tree =
  Node ([1;3],
        Node ([4;5;6],
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )
let test_one : string list tree =
  Node(["ABCDEFGHI"], Empty, Node(["JKLMNOPQRS"], Node(["TUVWXYZ"], Empty, Empty), Empty))

let int_list_tree : int list tree =
    Node ([1;4], Empty, Node([3;100], Empty, Empty))

let strs_tree: string list tree =
    Node (["Ocaml!  "; "It "; "must "; "be "],
    Node (["do "; "love "],
                           Node (["I "; "really "], Empty, Empty), Empty),
                     Node (["your "; "favorite "; "too!"], Empty, Empty)
                    )
let int_tree : int tree =
  Node (3,
        Node (1,
              Node (4, Empty, Empty), Empty),
        Node (2, Empty, Empty)
       )

let empty_lists : 'a list tree =
        Node([], Node([], Node([], Empty, Empty), Empty), Empty)

let str_tree : string tree =
  Node ("love ",
        Node ("really ", Node ("I ", Empty, Empty), Empty), Node ("OCaml!", Empty, Empty)
       )

let string_one : string tree =
         Node ("Only One", Empty, Empty
              )

let empty_string : string tree =
         Empty
let test1 : string tree =
    Empty
let test2 : string tree =
    Node ("Hello", Empty, Empty )

let test1_sum: int tree =
    Node (1, Empty, Empty )

let empty_int: int tree =
      Empty
let test3_sum: int tree =
    Node (1, Node (2, Node (10, Empty, Node (15, Empty,Empty)), Empty), Empty )

let test4_sum: int tree =
      Node(-1,Empty, Node(-2, Empty, Node(4, Empty, Empty)))

(*Credit to October 7 Lecture Examples for reduce *)
let rec reduce (t: 'a tree) (b: 'b) (f: 'a -> 'b-> 'b ->'b)   : 'b =
    match t with
    | Empty -> b
    | Node (root, left, right) -> f root (reduce left b f) (reduce right b f)

(* PART THREE FUNCTIONS (USE REDUCE) *)

(*size function that counts the number of values stored in a tree *)
let size (tree: 'a tree): int =
    reduce (tree) (0)  (fun root left right -> 1 + left + right)

(*sum function that sums all the values in an int tree *)
let sum (tree: int tree): int =
    reduce (tree) (0) (fun root left right -> root + left + right)

(*product function that multiples all the values in an int tree *)
let product (tree: int tree): int =
    reduce (tree) (1) (fun root left right -> root * left * right)

(*Charcount function that determines the total number of characters in a string tree  *)
let charcount (tree: string tree): int =
  reduce (tree) (0) (fun root left right -> String.length root + left + right)

(*Concat function that concats all of the strings in a string tree *)
let concat (tree: string tree) : string =
  reduce (tree) ("") (fun root left right -> left ^ root ^ right)

(*list_tree_size function: This is a function with the type 'a list tree -> int and counts the number of elements in the lists in the nodes in the tree. *)
let list_tree_size (tree: 'a list tree): int =
  reduce (tree) (0) (fun root left right -> (List.fold_left(fun a _ -> a+1 ) 0 root) +left+ right)

  (* list_tree_sum function This is a function with the type int list tree -> int that adds up all the int elements
     in the lists in the nodes of a int list tree.  *)
let list_tree_sum (tree: int list tree) : int =
    reduce (tree) (0) (fun root left right -> (List.fold_left(fun a y -> y+a ) 0 root) +left+ right)

    (*list_tree_product: This is a function with the type int list tree -> int that multiplies all the int elements
    in the lists in the nodes of a int list tree. *)
let list_tree_product (tree: int list tree) : int =
    reduce (tree) (1) (fun root left right -> (List.fold_left(fun a y -> y*a ) 1 root) *left*right)

    (*list_tree_charcount: This is a function with the type string list tree -> int that counts the number of characters
      in the string lists inside a string list tree. *)
let list_tree_charcount (tree: string list tree) : int =
    reduce (tree) (0) (fun root left right -> (List.fold_left (fun a y -> String.length y + a) 0 root)+ left + right)

    (*list_tree_concat function: This is a function with the type string list tree -> string that concatenates all the strings in
    the string lists inside a string list tree. *)
let list_tree_concat (tree: string list tree) : string =
    reduce (tree) ("") (fun root left right ->  left ^(List.fold_left (fun a y -> a^ y) "" root) ^ right)


(*ASSERT TEST FUNCTIONS *)
let () =
      print_string "Testing part 3 ... " ;
      try
      assert (size str_tree = 4);
      assert (size test1 = 0);
      assert (size test2 = 1);
      assert (size int_tree = 4);
      assert (sum int_tree = 10);
      assert (sum test1_sum = 1);
      assert (sum empty_int = 0);
      assert (sum test3_sum = 28);
      assert (sum test4_sum = 1);
      assert (product int_tree = 24);
      assert (product empty_int = 1);
      assert (charcount str_tree = 20);
      assert (charcount test1 = 0);
      assert (charcount test2 = 5);
      assert (concat str_tree = "I really love OCaml!");
      assert (concat empty_string = "");
      assert (concat string_one = "Only One");
      assert (list_tree_size strs_tree = 11);
      assert (list_tree_size empty_lists = 0);
      assert (list_tree_sum ints_tree = 45);
      assert (list_tree_sum empty_lists = 0);
      assert (list_tree_product ints_tree = 311040);
      assert (list_tree_product int_list_tree = 1200);
      assert (list_tree_product empty_lists = 1);
      assert (list_tree_charcount strs_tree = 54);
      assert (list_tree_charcount empty_lists = 0);
      assert (list_tree_charcount test_one = 26);
      assert (list_tree_concat strs_tree =
                "I really do love Ocaml!  It must be your favorite too!");
      assert (list_tree_concat empty_lists = "");
      assert (list_tree_concat test_one = "ABCDEFGHITUVWXYZJKLMNOPQRS");
           print_string "tests passed.\n"
         with
           Assert_failure (file, line, column) ->
           let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^
                       ", column " ^ string_of_int column ^ "\n\n\n\n"
           in print_string msg
