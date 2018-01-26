(*************************************)
(* Implementation of arytmetyka.mli  *)
(* Author: Mateusz Gienieczko 394302 *)
(* Reviewer: Jan Klinkosz 394342     *)
(*************************************)

(* Note -- all functions' and types names are in English, *)
(* then they are mapped to the specification at the end   *)

(* Fact: all values that can be achieved within our specification   *)
(* can be presented as a closed interval or a sum of two intervals. *)
(* Additionally, a single interval is equivalent to a sum of that   *)
(* interval and an empty one                                        *)

type interval = float * float
type interval_pair = interval * interval

(* === Helper functions === *)
      
(* Returns true if a is nan, false otherwise *)
let is_nan (a : float) =
  compare a nan = 0

(* Smaller of two floats and nan is greater than anything *)
let min (a : float) (b : float) =
  if is_nan a || a > b then b
  else a

(* Greater of two floats and nan is smaller than anything *)
let max (a : float) (b : float) =
  if is_nan a || a < b then b
  else a

(* Minimum of 4 floats *)
let min4 (a : float) (b : float) (c : float) (d : float) =
  min (min a b) (min c d)

(* Maximum of 4 floats *)
let max4 (a : float) (b : float) (c : float) (d : float) =
  max (max a b) (max c d)


(* === Constructors === *)
    

(* Closed interval of real numbers *)
let interval (a : float) (b : float) : interval =
  (a, b)

(* Sum of two intervals of real numbers *)
let interval_pair (x : interval) (y : interval) : interval_pair =
  (x, y)

(* Constant for an empty interval *)
let empty = interval nan nan

(* True if the passed interval is an empty interval, false otherwise *)
let is_empty ((a, b) : interval) =
    is_nan a && is_nan b

(* Constant for an interval [0.0, 0.0] *)
let zero = interval 0.0 0.0

(* An interval_pair containing only elements from range [a, b] *)
let value_range (a : float) (b : float) =
  interval_pair (interval a b) empty

(* An interval_pair containing only the element a *)
let exact_value (a : float) =
  interval_pair (interval a a) empty

(* An interval_pair containing values a +- p% *)
let inaccurate_value (a : float) (p : float) =
  let deviation = abs_float (a *. (p /. 100.))
  in
  value_range (a -. deviation) (a +. deviation)

    
(* === Selectors === *)

    
(* Lower bound of an interval *)
let min_value ((a, b) : interval) =
  min a b

(* Lower bound of an interval_pair *)
let min_value_pair ((x, y) : interval_pair) =
    min (min_value x) (min_value y)

(* Upper bound of an interval *)		      
let max_value ((a, b) : interval) =
  max a b
				      
(* Upper bound of an interval_pair *)
let max_value_pair ((x, y) : interval_pair) =
  max (max_value x) (max_value y)

(* The average value of an interval_pair, nan if undetermined *)
let average_value_pair ((x, y) : interval_pair) =
  (max_value_pair (x, y) +. min_value_pair (x, y)) /. 2.0

(* True if value l is contained within the given interval, false otherwise *)
(* Note that it is always false for nan *)
let in_interval ((a, b) : interval) (l : float) =
  not (is_empty (a, b)) && l >= a && l <= b

(* True if value l is contained within an interval_pair, false otherwise *)
(* Note that it is always false for nan *)
let in_interval_pair ((x, y) : interval_pair) (l : float) =
  in_interval x l || in_interval y l


(* === Modifiers === *)
    
    
(* Sums the interval_pair with a new interval for an interval_sum *)
(* representing (x1, y1) \sum (x2, y2) \sum (c, d)                *)
let merge (((x1, y1), (x2, y2)) : interval_pair) ((c, d) : interval) =
  if is_empty (c, d) then ((x1, y1), (x2, y2))
  else if in_interval (x1, y1) c || in_interval (x1, y1) d then
    interval_pair ((min x1 c), (max y1 d)) (x2, y2)
  else interval_pair (x1, y1) ((min x2 c), (max y2 d))

(* Sums 4 intervals into one *)
let merge4 (a : interval) (b : interval) (c : interval) (d : interval) =
  merge (merge (interval_pair a b) c) d

(* Sums two interval_pairs into one *)      
let merge_pair (x : interval_pair) ((y1, y2) : interval_pair) =
  merge (merge x y1) y2

(* Sums 4 interval_pairs into one *)
let merge_pair4 (a : interval_pair) (b : interval_pair)
    (c : interval_pair) (d : interval_pair) : interval_pair =
  merge_pair (merge_pair (merge_pair a b) c) d

(* Splits the given interval into two intervals (a1, b1) (a2, b2), such that *)
(* a1 <= b1 <= 0 <= a2 <= b2 and (a1, b1) \sum (a2, b2) = (a, b)             *)
let split ((a, b) : interval) =
  if (a, b) = zero then interval_pair zero zero
  else if a < 0.0 && b > 0.0 then
    interval_pair (interval a (-0.0)) (interval 0.0 b)
  else if a < 0.0 && b = 0.0 then
    interval_pair (interval a (-0.0)) empty
  else interval_pair (interval a b) empty

(* Interval of all possible values x + y where *)
(* x is from [a, b] and y is from [c, d]       *)
let sum ((a, b) : interval) ((c, d) : interval) =
  interval (min4 (a +. c) (a +. d) (b +. c) (b +. d))
           (max4 (a +. c) (a +. d) (b +. c) (b +. d))

(* Interval of all possible values x + y where         *)
(* (x \in x1 \or x \in y1) \and (y \in x2 \or y \in y2 *)  
let sum_pair ((x1, y1) : interval_pair) ((x2, y2) : interval_pair) =
  merge4 (sum x1 x2) (sum x1 y2)
         (sum y1 x2) (sum y1 y2)
				      
(* Interval of all possible values x - y       *)
(* where x is from [a, b] and y is from [c, d] *)
let difference ((a, b) : interval) ((c, d) : interval) =
  interval (min4 (a -. c) (a -. d) (b -. c) (b -. d))
           (max4 (a -. c) (a -. d) (b -. c) (b -. d))

(* Interval of all possible values x - y                     *)
(* where (x \in x1 \or x \in y1) \and (y \in x2 \or y \in y2 *) 
let difference_pair ((x1, y1) : interval_pair) ((x2, y2) : interval_pair) =
  merge4 (difference x1 x2) (difference x1 y2)
         (difference y1 x2) (difference y1 y2)

(* Interval of all possible values x * y                        *)
(* where x is from [a, b] and y is from [c, d]                  *)
(* First splits the intervals to negatives and positives,       *)
(* calculates products separately and then merges them together *)
let product ((a, b) : interval) ((c, d) : interval) =
  let (x1, y1) = split (a, b) and (x2, y2) = split (c, d)
  (* Takes two intervals not crossing through zero, gives the interval *)
  (* of all possible x * y where x from [a, b], y from [c, d]          *)
  and simple_product ((a, b) : interval) ((c, d) : interval) : interval =
    interval (min4 (a *. c) (a *. d) (b *. c) (b *. d))
             (max4 (a *. c) (a *. d) (b *. c) (b *. d))
  in
  merge4 (simple_product x1 x2) (simple_product x1 y2)
         (simple_product y1 x2) (simple_product y1 y2)

(* Interval of all possible values x * y                     *)
(* where (x \in x1 \or x \in y1) \and (y \in x2 \or y \in y2 *) 
let product_pair ((x1, y1) : interval_pair) ((x2, y2) : interval_pair) =
  merge_pair4 (product x1 x2) (product x1 y2)
	      (product y1 x2) (product y1 y2)

(* Interval of all possible values x / y                         *)
(* where x is from [a, b] and y is from [c, d]                   *)
(* First splits the intervals to negatives and positives,        *)
(* calculates quotients separately and then merges them together *)
let quotient ((a, b) : interval) ((c, d) : interval) =
  if (c, d) = zero then interval_pair empty empty
  else
  let (x1, y1) = split (a, b) and (x2, y2) = split (c, d)
  (* Takes two intervals not crossing through zero, gives the interval *)
  (* of all possible x / y where x from [a, b], y from [c, d]          *)
  and simple_quotient (a, b) (c, d) =
    interval (min4 (a /. c) (a /. d) (b /. c) (b /. d))
	     (max4 (a /. c) (a /. d) (b /. c) (b /. d))
  in
  merge4 (simple_quotient x1 x2) (simple_quotient x1 y2)
	 (simple_quotient y1 x2) (simple_quotient y1 y2)

(* Interval of all possible values x / y                     *)
(* where (x \in x1 \or x \in y1) \and (y \in x2 \or y \in y2 *) 
let quotient_pair ((x1, y1) : interval_pair) ((x2, y2) : interval_pair) =
  merge_pair4 (quotient x1 x2) (quotient x1 y2)
	      (quotient y1 x2) (quotient y1 y2) 

    
(* Translate to specification's namespace                      *)
(* It was easier to work on the project using English names :) *)

type wartosc = interval_pair

let wartosc_dokladnosc = inaccurate_value
let wartosc_od_do = value_range
let wartosc_dokladna = exact_value
    
let in_wartosc = in_interval_pair
let min_wartosc = min_value_pair
let max_wartosc = max_value_pair
let sr_wartosc = average_value_pair
    
let plus = sum_pair
let minus = difference_pair
let razy = product_pair
let podzielic = quotient_pair

;;
