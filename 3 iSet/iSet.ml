(*************************************)
(* Implementation of iSet.mli        *)
(* Author: Mateusz Gienieczko 394302 *)
(* Reviewer: Maciej Nadolski 394491  *)
(*************************************)


(******************************************************************************)
(* AVL-Binary-Search-Trees (or Sets, used interchangeably)                    *)
(* holding intervals of integers                                              *)
(* Assumes all integers are from range [min_int, max_int]                     *)
(* Intervals in the Set are always disjoint. Moreover, any 'neighbouring'     *)
(* intervals are connected together ([x, y] and [y + 1, z] turns into [x, z]) *)
(* All operations have time complexity of O(log(n)), excluding fold, iter     *)
(* and elements, which perform in O(n), and the Set will never occupy more    *)
(* than O(n) memory, where n is the amount of disjoint intervals in the Set   *)
(******************************************************************************)
(* Interval comparison used within this implementation can be specified as:   *)
(* [x1, y1] < [x2, y2] iff x1 < x2                                            *)
(******************************************************************************)
(* Trees created using this implementation, as well as all of their subtrees, *)
(* satisfy following invariants:                                              *)
(* 1) AVL invariant:                                                          *)
(* the difference between longest paths to a leaf of any left and right       *)
(* subtree does not exceed 2                                                  *)
(* 2) BST invariant:                                                          *)
(* all values in the left subtree are strictly lesser than the root value     *)
(* while all values in the right subtree are strictly greater                 *)
(******************************************************************************)

type interval = int * int
type t = 
  | Empty 
  | Set of t * interval * t * int * int

(* Interval in the root of Set s *)
(* s cannot be Empty             *)
let value s =
  match s with
  | Set (_, v, _, _, _) -> v 
  | Empty -> failwith "Empty set passed to function: value"
    
(* Distance to furthest leaf from the root of s *)
let height s =
  match s with
  | Set (_, _, _, h, _) -> h 
  | Empty -> 0

(* Number of singular integer values (NOT intervals) contained within s *)
let cardinality s =
  match s with
  | Set (_, _, _, _, c) -> c
  | Empty -> 0

(* Empty Set constant *)
let empty = Empty

(* Whether s is empty or not *)
let is_empty s =
  s = Empty

(**


(** TESTING FUNCTIONS **)


(* Test if the Set s satisfies the AVL invariant *)
(* Time complexity: O(size s)                    *)
let rec test_avl s =
  match s with
    | Set (l, _, r, _, _) ->
        abs ((height r) - (height l)) <= 2 && test_avl l && test_avl r
    | Empty -> true

(* Test if the Set s satisfies the BST invariant *)
(* Time complexity: O(size s)                    *)
let rec test_bst s =
  match s with
    | Set (l, (x, y), r, _, _) ->
        test_bst l && test_bst r &&
        (match l, r with
        | Empty, Empty -> true
        | Empty, _ -> fst (value r) > y + 1
		| _, Empty -> snd (value l) < x - 1
        | _, _ -> fst (value r) > y + 1 && snd (value l) < x - 1)
    | Empty -> true

(* Test Set s for both invariants *)
(* Time complexity: O(size s)     *)
let test_invariants s =
  test_avl s && test_bst s

**)


(** HELPER FUNCTIONS **)


(* Applies function f to all Sets along the binary-search path to n          *)
(* Finds the greatest (by value comparison) interval that holds values       *) 
(* lesser or equal to n (lower bound),                                       *)
(* let us name the Set root containing that interval as r                    *)
(* and the direct predecessor of set x as P(x). Then the value computed is:  *)
(* f ( f ( f (... f a r) P(r) ) P(P(r)) ) ... ) s                            *)
(* Time complexity: O(height s)                                              *)
(* Not tail-recursive                                                        *)
let rec fold_path f a n s =
    match s with
    | Set (l, (x, y), r, _, _) ->
        let res =
	      if n < x then fold_path f a n l
  	      else if n > y then fold_path f a n r
	      else a
        in
        f res s
    | Empty -> f a s
 
(* Safe addition. If the result would be greater than max_int, gives max_int  *)
(* Assumes that the real result of a + b is positive, does not work otherwise *)
let plus a b =
  if a + b < 0 then max_int
  else a + b

(* Number of integers in an interval. If greater than max_int, gives max_int *)
let cnt ((x, y) : interval) =
  if y - x + 1 <= 0 then max_int
  else y - x + 1

(* Creates a Set with l and r as its left and right subtrees respectively     *)
(* and (x, y) as root value. Updates height and cardinality values            *)
(* Does NOT guarantee that the resultant Set satisfies any of the invariants  *)
let set l ((x, y) : interval) r : t =
  Set (l, (x, y), r, (max (height l) (height r)) + 1,
       plus (plus (cardinality l) (cardinality r)) (cnt (x, y)))

(* Balances the Set l v r so that it satisfies the AVL invariant *)
(* Resultant Set satisfies the BST invariant if and only if      *)
(* both l and r satisfied the BST invariant and all values in    *)
(* l were lesser than v and all values in r were greater than v  *)
(* Time complexity: O(abs((height l) - (height r)))              *)
let rec balance l v r =
  if (height l) > (height r) + 2
  then match l with
    | Set (ll, lv, lr, _, _) ->
	    if height ll >= height lr
  	    then set ll lv (balance lr v r)
	    else (match lr with
	      | Set (lrl, lrv, lrr, _, _) ->
	          set (set ll lv lrl) lrv (balance lrr v r)
	      | Empty -> failwith "Unsupported case (l->lr->Empty) in fun: balance")
    | Empty -> failwith "Unsupported case (l->Empty) in fun: balance"
  else if (height r) > (height l) + 2
  then match r with
    | Set (rl, rv, rr, _, _) ->
	    if height rr >= height rl
	    then set (balance l v rl) rv rr
	    else (match rl with
	      | Set (rll, rlv, rlr, _, _) ->
	          set (balance l v rll) rlv (set rlr rv rr)
	      | Empty -> failwith "Unsupported case (r->rl->Empty) in fun: balance")
      | Empty -> failwith "Unsupported case (r->Empty) in fun: balance"
  else
    set l v r

(* Returns a pair of the least interval in s and s without that interval    *)
(* Should not be used for an empty Set, returns ((min_int, min_int), Empty) *)
(* Time complexity: O(height s)                                             *)
let remove_min s =
  fold_path (fun (va, sa) s ->
    match s with
    | Set (l, v, r, _, _) ->
	    if l = Empty then (v, r)
	    else (va, balance sa v r)
    | Empty -> (va, sa)) ((min_int, min_int), Empty) min_int s

(* Returns a pair of the greatest interval in s and s without that interval *)
(* Should not be used for an empty Set, returns ((max_int, max_int), Empty) *)
(* Time complexity: O(height s)                                             *)
let remove_max s =
  fold_path (fun (va, sa) s ->
    match s with
    | Set (l, v, r, _, _) ->
	    if r = Empty then (v, l)
	    else (va, balance l v sa)
    | Empty -> (va, sa)) ((max_int, max_int), Empty) max_int s    

(* Merges two Sets l r into one. The resultant Set satisfies the AVL and BST *)
(* invariants if and only if l and r satisfied both invariants               *)
(* and all values in l were lesser than all values in r                      *)
(* Time complexity: O(abs((height l) - (height r)))                          *)
let merge l r =
  match l, r with
  | Empty, _ -> r
  | _, Empty -> l
  | _ ->
    let (v, nr) = remove_min r
    in
    balance l v nr


(** SELECTORS **)


(* Applies function f to all intervals in s in the infix order *)
(* Time complexity: O(n)                                       *)
let rec iter (f : interval -> unit) s =
  match s with
  | Set (l, v, r, _, _) -> iter f l; f v; iter f r
  | Empty -> ()

(* Applies function f to all intervals in s in the infix order *)
(* Empty Set returns a                                         *)
(* Time complexity: O(n)                                       *)
let rec fold (f : interval -> 'a -> 'a) s a =
  match s with
  | Set (l, v, r, _, _) -> fold f r (f v (fold f l a))
  | Empty -> a

(* Returns a sorted list of all intervals within s *)
(* Time complexity: O(n)                           *)
let elements s : interval list =
  let rec aux a s =
    match s with
    | Set (l, v, r, _, _) -> aux (v::(aux a r)) l
    | Empty -> a
  in
  aux [] s

(* Whether or not integer n is contained within any of the intervals in s *)
(* Time complexity: O(height s)                                           *)
let mem n s =
  fold_path (fun acc s ->
    match s with
    | Set (_, (x, y), _, _ , _) ->
        if x <= n && n <= y then true
        else acc
    | Empty -> false) false n s

(* Amount of singular integers (NOT intervals) within s lesser or equal to n *)
(* Time complexity: O(height s)                                              *)
let below n s =
  fold_path(fun acc s ->
    match s with
    | Set(l, (x, y), _, _, _) ->
	    if n >= x
	    then plus acc (plus (cnt (x, (min n y))) (cardinality l))
	    else acc
    | Empty -> acc) 0 n s


(** MODIFIERS **)


(* Returns a triplet (l, b, r) where:                                *)
(* l is a Set containing all integers from s strictly lesser than n  *)
(* b is a logic value true if and only if n was contained within s   *)
(* r is a Set containing all integers from s strictly greater than n *)
(* If the passed Set s satisfied the AVL and BST invariants then     *)
(* it is guaranteed that both l and r will satisfy both invariants   *)
(* Time complexity: O(height s)                                      *)
let split n s =
  fold_path (fun (la, ba, ra) s ->
    match s with
    | Set (l, (x, y), r, _, _) ->
	    let nl =
	      if x < n then balance l (x, min y (n - 1)) la
	      else if x == n then merge l la      (* will fire only once *)
	      else la
	    and nr =
	      if y > n then balance ra (max x (n + 1), y) r
	      else if y == n then merge r ra      (* will fire only once *)
	      else ra
	    in
	    if x <= n && y >= n
	    then (nl, true, nr)
	    else (nl, ba, nr)
    | Empty -> (la, ba, ra)) (Empty, false, Empty) n s

(* Returns Set s with the interval [nx, ny] added                *)
(* Uses split to divide intervals into lesser and greater        *)
(* than [nx, ny], sets [nx, ny] as root, merges the neighbouring *)
(* intervals (there can be at most one lesser and one greater    *)
(* and balances the resultant tree. If passed Set s satisfied    *)
(* both AVL and BST invariants then it is guaranteed that the    *)
(* resultant Set will also satisfy both                          *)
(* Time complexity: O(height s)                                  *)
let add ((nx, ny) : interval) s =
  let (l, _, tr) = split nx s
  in
  let (_, _, r) = split ny tr
  in
  let ((nx, _), l) =
    if l != Empty && mem (nx - 1) l
    then remove_max l
    else ((nx, ny), l)
  and ((_, ny), r) =
    if r != Empty && mem (ny + 1) r
    then remove_min r
    else ((nx, ny), r)
  in
  balance l (nx, ny) r

(* Returns Set s with all values within [nx, ny] removed         *)
(* Uses split to divide intervals into lesser and greater        *)
(* than [nx, ny], and merges them together                       *)
(* Time complexity: O(height s)                                  *)
let remove ((nx, ny) : interval) s =
  let (l, _, tr) = split nx s
  in
  let (_, _, r) = split ny tr
  in
  merge l r

;;
