(* A tree type declaration. *)
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree


(* Assert Test Trees *)
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
    Node ("Only One", Empty, Empty)

let empty_string : string tree =
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


(* PART ONE FUNCTIONS *)

(*size function that counts the number of values stored in a tree *)
let rec size (tree: 'a tree) : int =
    match tree with
    |Empty -> 0
    |Node (root,left,right) -> 1+ size left + size right


(*sum function that sums all the values in an int tree *)
let rec sum (tree: int tree) : int =
    match tree with
    | Empty -> 0
    | Node (root,left,right) -> root + sum left + sum right


(*product function that multiples all the values in an int tree *)
let rec product (t: int tree) : int =
  match t with
  |Empty -> 1
  |Node (root, left, right) -> root * product left * product right


(*Charcount function that determines the total number of characters in a string tree  *)
let rec charcount (tree: string tree) : int =
    match tree with
    |Empty -> 0
    |Node (root, left, right) -> String.length root + charcount left + charcount right

(*Concat function that concats all of the strings in a string tree *)
let rec concat (tree: string tree) : string =
    match tree with
    | Empty -> ""
    | Node (root,left,right) ->    concat left ^root ^concat right



(* PART TWO FUNCTIONS *)

(*list_tree_size function: This is a function with the type 'a list tree -> int and counts the number of elements in the lists in the nodes in the tree. *)
let rec list_tree_size (tree: 'a tree): int =
  match tree with
  | Empty -> 0
  | Node(root, left, right) -> List.fold_left(fun a _ -> a+1 ) 0 root + list_tree_size left + list_tree_size right


(*sum_list helper function that will sum a list of values within a tree's nodes *)
let sum_list (lst : int list) : int =
    List.fold_left (fun acc x -> acc + x) 0 lst

(* list_tree_sum function This is a function with the type int list tree -> int that adds up all the int elements
   in the lists in the nodes of a int list tree. Calls helper sum_list function. *)
let rec list_tree_sum (tree: int list tree) : int =
  match tree with
  | Empty -> 0
  | Node(root, left, right) -> sum_list root + list_tree_sum left + list_tree_sum right


(*prod_list helper function that finds the product of a given list *)
let prod_list (lst : int list) : int =
    List.fold_left (fun acc x -> acc * x) 1 lst

(*list_tree_product: This is a function with the type int list tree -> int that multiplies all the int elements
in the lists in the nodes of a int list tree. Calls prod_list find the product of the list at the alternating root nodes*)
let rec list_tree_product (tree: int list tree) : int =
    match tree with
    | Empty -> 1
    | Node(root, left, right) -> prod_list root * list_tree_product left * list_tree_product right

(*string_list helper function that takes a list of strings and counts the number of characters in it*)
let rec string_list (lst: string list) :  int =
    match lst with
    | [] -> 0
    |x::xs -> String.length x + string_list xs

(*list_tree_charcount: This is a function with the type string list tree -> int that counts the number of characters
  in the string lists inside a string list tree. Calls string_list on the root's list. *)
let rec list_tree_charcount (tree: string list tree) : int =
    match tree with
    | Empty -> 0
    | Node(root, left, right) -> string_list root + list_tree_charcount left + list_tree_charcount right

(*concat helper function that concatanes the strings within a string list *)
let rec concat_list (lst: string list ) : string =
  match lst with
  | [] -> ""
  |x::xs -> x ^ concat_list xs

(*list_tree_concat function: This is a function with the type string list tree -> string that concatenates all the strings in
the string lists inside a string list tree. Calls concat_list on the root's list *)
let rec list_tree_concat (tree: string list tree): string =
    match tree with
    | Empty -> ""
    |Node(root, left, right) -> list_tree_concat left ^ concat_list root ^ list_tree_concat right


(* ASSERT CALLS FOR PART ONE*)

let () =
  print_string "Testing part 1 ... " ;
  try
  assert (size str_tree = 4);
  assert (size empty_string= 0);
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
  assert (charcount empty_string = 0);
  assert (charcount test2 = 5);
  assert (concat str_tree = "I really love OCaml!");
  assert (concat empty_string = "");
  assert (concat string_one = "Only One");
  print_string "tests passed.\n"
  with
  Assert_failure (file, line, column) ->
  let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^
      ", column " ^ string_of_int column ^ "\n\n\n\n"
  in print_string msg


(* ASSERT CALLS FOR PART TWO*)
  let () =
    print_string "Testing part 2 ... " ;
    try
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
