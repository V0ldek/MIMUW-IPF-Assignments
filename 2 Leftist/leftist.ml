(****************************************)
(* Implementation of leftist.mli        *)
(* Author: Mateusz Gienieczko 394302    *)
(* Reviewer: Wojciech Bączkowski 394071 *)
(****************************************)

(* Type representing a priority queue implemented as a leftist tree *)
type 'a queue = 
    | Null
    | Node of 'a node

and 'a node = { value: 'a; depth: int; left: 'a queue; right: 'a queue }

(* Empty queue *)
let empty = Null

(* True if the priority queue is empty, false otherwise *)
let is_empty q =
  match q with
    | Null -> true
    | _    -> false

(* Exception raised when trying to access an element of an empty queue *)
exception Empty

(* Create Node *)
let make value depth left right = 
  Node { value = value; depth = depth; left = left; right = right }

(* Depth of the tree, empty tree has depth 0 *)
let depth t =
  match t with
    | Null   -> 0
    | Node n -> n.depth
    
(* Priority queue containing all elements from q1 and q2 *)
let rec join q1 q2 =
  (* Tree rooted at root with lsub and rsub as its left and right *)
  (* subtrees respectively. Makes sure that the rightmost path is *)
  (* always the shortest, swapping the subtrees if needed         *)
  let rec join_subtrees lsub root rsub = 
    if depth rsub > depth lsub
    then join_subtrees rsub root lsub
    else make root.value ((depth rsub) + 1) lsub rsub
  in
  (* Use greatest element as new root, merge its right subtree    *)
  (* with the other queue, recursively. At most O(logn) recursive *)
  (* calls because of the length of the rightmost path            *)
  match q1, q2 with
    | Null, _          -> q2
    | _, Null          -> q1
    | Node n1, Node n2 ->
      if n1.value > n2.value then join q2 q1
      else
        let new_subtree = join n1.right q2
        in
	    join_subtrees n1.left n1 new_subtree

(* Pair of the topmost element in the priority queue   *)
(* and the queue with said element removed             *)
let delete_min q =
  match q with
    | Null   -> raise Empty
    | Node n -> (n.value, join n.left n.right)

(* Priority queue containing all elements from q and the element e *)
let add e q =
  let eq = make e 1 Null Null
  in
  join q eq

;;
