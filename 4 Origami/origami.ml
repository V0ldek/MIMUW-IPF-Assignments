(* Implementation of origami.mli     *)
(* Author: Mateusz Gienieczko 394302 *)
(* Reviewer: Piotr Borowski 394091   *)

(* Simulates a paper sheet on a 2D plane (for origami enthusiasts)          *)
(* Thickness of a sheet is defined as the number of layers at a given point *)
(* In other words the number of times a pin would pierce the paper when     *)
(* applied at a given point                                                 *)

(* Represents a vector on a 2D plane *)
type point = float * float

(* Function equal to the thickness of the paper sheet at a given point *)
(* Calculation time of this type of function is O(thickness)           *)
(* which can be O(2^n). Takes up to O(n) memory as an object, up to    *)
(* O(thickness) during call (not tail recursive).                      *)
(* (n is number of <zloz> function calls)                              *)
type kartka = point -> int

(* Constant to deal with float inaccuracies *)
let eps = epsilon_float *. 1000000.;;
    
(** HELPER FUNCTIONS **)
    

(* a^2 *)
let square a =
  a *. a

(** Operations on vectors **)

(* The cross product of vectors [x1 - x, y1 - y] and [x2 - x, y1 - y] *)
let crossproduct ((x, y) : point) ((x1, y1) : point) ((x2, y2) : point) =
  (x1 -. x) *. (y2 -. y) -. (x2 -. x) *. (y1 -. y)

(* The dot product of vectors [x1, y1] and [x2, y2] *)
let dotproduct ((x1, y1) : point) ((x2, y2) : point) =
  (x1 *. x2) +. (y1 *. y2)

(* ||[x, y]|| *)
let norm ((x, y) : point) =
  sqrt(square(x) +. square(y))

(* Vector addition *)
let add ((ax, ay) : point) ((bx, by) : point) : point =
  (ax +. bx, ay +. by)

(* Vector subtraction *)
let sub ((ax, ay) : point) ((bx, by) : point) : point =
  (ax -. bx, ay -. by)

(* Scalar multiplication *)
let mult ((x, y) : point) c : point =
  (c *. x, c *. y)

(* Scalar division *)
let div ((x, y) : point) c : point =
  (x /. c, y /. c)

(* Point symmetrical to (x, y) over a line through points a and b     *)
(* Calculates the normal directional vector of the line (as l),       *)
(* then the orthogonal projection of p onto the line (as q),          *)
(* so the p image is p + two times the vector from p to q             *)
let symmetry (p : point) (a : point) (b : point) : point =
  let l = div (sub b a) (norm (sub b a))           (* l = (b - a) / ||b - a|| *)
  in
  let q = add a (mult l (dotproduct (sub p a) l))  (* q = a + ((p - a) * l)l  *)
  in
  sub (mult q 2.) p                                (* p' = 2q - p             *)


(** CONSTRUCTORS **)
    
    
(* A rectangular sheet of paper with its left and right corners *)
(* at (x1, y1) and (x2, y2) respectively 			*)
let prostokat ((x1, y1) : point) ((x2, y2) : point) : kartka =
  fun ((x, y) : point) ->
    if x1 -. x <= eps && x -. x2 <= eps
	&& y1 -. y <= eps && y -. y2 <= eps
    then 1
    else 0

(* A circular sheet of paper with its origin at (x1, y1) and radius r *)
let kolko (s : point) r : kartka =
  fun (p : point) ->
    if (norm (sub s p)) -. r <= eps
    then 1
    else 0


(** MODIFIERS **)


(* Fold the sheet f by line going through points p1 and p2              *)
(* The points have to be distinct                                       *)
(* Every part of the sheet on the right to the line                     *)
(* (defined as when looking from point p1 to p2)                        *)
(* is symmetrically flipped to the left.                                *)
(* Thickness exactly at the line remains unchanged                      *)
(* Value for p on the left of the line is equal to f p before the fold  *)
(* and the value for the image p' over the line (p1 p2), value for      *)
(* p on the line remains unchanged and on the right it is equal to zero *)
(* Time complexity is O(1), but can potentially double the thickness    *)
let zloz (a : point) (b : point) (f : kartka) : kartka =
  fun (p : point) ->
    let cp = crossproduct a b p (* Positive means p is to the left *)
    in
    if abs_float(cp) <= eps
    then f p 
    else if cp > 0.0
    then f p + f (symmetry p a b)
    else 0

(* Fold the sheet f through all lines in list l                            *)
(* (line defined by two distinct points belonging to it)                   *)
(* Result for list [(p1, q1); (p2, q2); ...; (pn, qn)] is:                 *)
(* zloz pn qn (zloz pn-1 qn-1 (... (zloz p2 q2 (zloz p1 q1 f))...))        *)
(* Time complexity O(n), where n is the length of the list, tail recursive *)
let skladaj (l : (point * point) list) (f : kartka) : kartka =
  List.fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) f l

;;
