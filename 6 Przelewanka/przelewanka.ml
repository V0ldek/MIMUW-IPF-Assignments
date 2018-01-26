(* Implementation of przelewanka.mli *)
(* Author: Mateusz Gienieczko 394302 *)
(* Reviewer: Marek Zochowski 395071  *)

(* Solves the przelewanka problem                                             *)
(* There are n empty cups, each with a given maximal volume v_i               *)
(* and a goal g_i. Available operations are:                                  *)
(* fill i -> fill the cup i with water to its max volume v_i                  *)
(* empty i -> empty the cup i                                                 *)
(* pour i j -> pour water from i to j until either i is empty or j is filled  *)
(* Function przelewanka, taking an array of pairs (v_i, g_i) for each cup i,  *)
(* returns the minimal number of operations needed, so that all the cups      *)
(* have the goal amount of water g_i in them (if it's impossible, returns -1) *)
(* Time complexity O(product (v_i) * n^3),                                    *)
(* memory complexity O(product (v_i) * n)                                     *)
(* Note that the complexity is an upper estimate, performs way better on avg  *)
(* Used performance heuristics:                                               *)
(* -> Filters out all v_i = 0 (don't affect the solution)                     *)
(* -> If any g_i is not divisible by gcd of v_i, then the answer is -1 (O(n)) *)
(* -> If for every i v_i != g_i and v_i != 0, then the answer is -1 (O(n))    *)

(* Problem specification: v_i and g_i arrays *)
type spec_t = { volume: int array;
	            goal: int array    }
      
(* One state of the problem: current water levels *)
(* and number of cups where level_i <> g_i        *)
type state_t = { current: int array;
	             remaining: int ref  }
   
(* Helper function that splits an array of pairs *)
(* [|(a1, b1); (a2, b2); ...; (an, bn)|]         *)
(* into a pair of arrays                         *)
(* ([|a1; a2; ...; an|], [|b1; b2; ...; bn|])    *)
(* Time complexity O(n), memory complexity O(n)  *)
let array_split a =
  let n = Array.length a
  in
  (Array.init n (fun i -> fst a.(i)),
   Array.init n (fun i -> snd a.(i)))

(* Returns the greatest common dividor of a and b            *)
(* Time complexity O(log (a + b)), memory complexity O(1)    *)
let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)

(* Returns a deep copy of the passed state      *)
(* Time complexity O(n), memory complexity O(n) *)
let copy_state state =
  { current = Array.copy state.current;
    remaining = ref (!(state.remaining)) }

(* Adds chng to the current water level of cup i, updates remaining if needed *)
(* Time complexity O(1), memory complexity O(1)                               *)
let change_state spec state i chng =
  if state.current.(i) = spec.goal.(i)
  then incr state.remaining;
  
  state.current.(i) <- state.current.(i) + chng;
  
  if state.current.(i) = spec.goal.(i)
  then decr state.remaining;
;;

(* True if the passed state is equivalent to goal, false otherwise *)
(* Time complexity O(1), memory complexity O(1)                    *)
let is_goal state = 
  !(state.remaining) = 0

(* Creates a goal state from given specification *)
(* Time complexity O(n), memory complexity O(n)  *)
let goal_state spec =
  { current = Array.copy spec.goal;
    remaining = ref 0               }

(* For the given state returns the list of changes  *)
(* resulting from all legal operations              *)
(* Ignores operations that result in no change      *)
(* Time complexity O(n^2), memory complexity O(n^2) *)
let possible_states spec state =
  let n = Array.length state.current
  in
  (* Extends the list by all states from (fill x) operations, i <= x <= n  *) 
  let rec add_fill i l =
    if i = n then l
    else
      let chng = spec.volume.(i) - state.current.(i)
      in
      if chng = 0 then add_fill (i + 1) l
      else
	    let new_state = copy_state state
	    in
	    change_state spec new_state i chng;
	    add_fill (i + 1) (new_state::l)

  (* Extends the list by all states from (empty x) operations, i <= x <= n *)
  and add_empty i l =
    if i = n then l
    else
      let chng = -state.current.(i)
      in
      if chng = 0 then add_empty (i + 1) l
      else
	    let new_state = copy_state state
	    in
	    change_state spec new_state i chng;
	    add_empty (i + 1) (new_state::l)

  (* Extends the list by all states from (pour x y) operations             *)
  (* For each x in (i, n) processes each y in [0, n)                       *)
  (* For x = i processes each y in [j, n)                                  *) 
  and add_pour i j l =
    if i = n then l
    else if j = n then add_pour (i + 1) 0 l
    else if i = j then add_pour i (j + 1) l
    else
      let chng = min state.current.(i) (spec.volume.(j) - state.current.(j))
      in
      if chng = 0 then add_pour i (j + 1) l
      else
	    let new_state = copy_state state
	    in
	    change_state spec new_state i (-chng);
	    change_state spec new_state j chng;
	    add_pour i (j + 1) (new_state::l)
  in
  add_pour 0 0 (add_empty 0 (add_fill 0 []))

(* Performs a breadth-first search from source until a vertex that fulfills  *)
(* is_target predicate is reached or                                         *)
(* all reachable vertices are visited, whichever happens first               *)
(*****************************************************************************)
(* Parameters:                                                               *)
(* visit v d -> should mark v as visited with distance d                     *)
(* distance v -> should return the distance vertex v was visited with,       *)
(* or -1 if it was not visited yet                                           *)
(* neighbors v -> should return the list of vertices v has outgoing edges to *)
(*****************************************************************************)
(* Time complexity O(|V| + |E|), memory complexity O(|V|)                    *)
(* This assumes visit, distance and is_target are O(1) while neighbors is    *)
(* O(|V|) time and memory. Complexity will be multiplied by any deviations   *)
(* from this assumptions                                                     *)
let bfs visit distance neighbors is_target source =
  let q = Queue.create ()
  and found = ref (is_target source)
  in
  visit source 0;
  Queue.push source q;
  while not (Queue.is_empty q) && not !found do
    let v = Queue.pop q
    in
    let d = distance v
    in
    let process u =
      if distance u = -1 then begin
	    visit u (d + 1);
	    Queue.push u q;
	    found := !found || is_target u
      end;
    in
    List.iter process (neighbors v)
  done

(* For the given inital_state and spec returns the minimal     *)
(* number of operations to change initial_state into spec.goal *)
(* or -1 if it is impossible                                   *)
(* Time complexity O(product (v_i) * n^3),                     *)
(* memory complexity O(product (v_i) * n)                      *)
let solve spec initial_state =
  let hashtbl_size = Array.fold_left (fun a x -> x * a) 1 spec.volume
  in
  (* hashtbl_size may overflow, hence the max with 10000 *)
  let been = Hashtbl.create (max 10000 (min 10000000 hashtbl_size))
  in
  let visit v d =
    Hashtbl.add been v d
  and distance v =
    try Hashtbl.find been v
    with Not_found -> -1
  in
  bfs visit distance (possible_states spec) is_goal initial_state;
  distance (goal_state spec)

(* Filter out all cups with v_i = 0 from a      *)
(* Time complexity O(n), memory complexity O(n) *)
let filter_useless a =
  let n = Array.length a
  in
  let rec filter f i l =
    if i = n then l
    else
      if f a.(i) then
	filter f (i + 1) (a.(i)::l)
      else
	filter f (i + 1) l
  in
  Array.of_list (filter (fun (v, _) -> v <> 0) 0 [])

(* True if all goals are divisible by gcd (v_i), false otherwise *)
(* Time complexity O(n log n), memory complexity O(1)            *)
let gcd_heuristic a =
  let v_gcd = Array.fold_left (fun a (v, _) -> gcd a v) 0 a
  in
  Array.for_all (fun (_, g) -> g mod v_gcd = 0) a

(* True if there exists a cup where v_i = g_i or g_i = 0, false otherwise *)
(* Time complexity O(n), memory complexity O(1)                           *)
let empty_full_heuristic a =
  Array.exists (fun (v, g) -> v = g || g = 0) a    

(* False if the solution surely does not exist, true if undetermined *)
(* Time complexity O(n log n), memory complexity O(1)                *)
let heuristics a =
  gcd_heuristic a && empty_full_heuristic a
    
(* For the given array of maximal volumes and goals *)
(* [|(v_1, g_1); (v_2, g_2); ...; (v_n, g_n)|]      *)
(* returns the solution to the przelewanka problem  *)
(* Time complexity O(product (v_i) * n^3),          *)
(* memory complexity O(product (v_i) * n)           *)
let przelewanka a =
  let filtered = filter_useless a
  in
  if heuristics filtered then
    let (volume, goal) = array_split (filter_useless a)
    in
    let spec = { volume = volume;
		 goal = goal      }
    and n = Array.length volume
    in
    let rem = Array.fold_left
	(fun a x -> if x = 0 then a - 1 else a)
	n spec.goal
    in
    let initial_state = { current = Array.make n 0;
                          remaining = ref rem       }
    in
    solve spec initial_state
  else if filtered = [||] then
    0
  else
    -1
      
;;

