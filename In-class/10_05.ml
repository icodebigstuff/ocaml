
  let 'a btree = Empty
                  |Node of 'a * 'a btree * 'a btree

  let t = Node (5, Node (4,Node(1, Empty, Empty), Empty), Node (2,Empty,Empty))
