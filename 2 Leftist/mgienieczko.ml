(* Tests for Leftist task                    *)
(* Divided into performance and correctness  *)
(* Licensed under GNU General Public License *)
(* Copyright (C) 2017 Mateusz Gienieczko     *)

open Leftist;;

exception WA;;

(* Returns true if ALL values from q are taken out in the order given in l *)
let test q l =
    try
      let (b, nq) = List.fold_left (fun a x -> 
        let (e, nq) = delete_min (snd a)
        in 
        if(compare x e != 0) then raise WA 
        else (true, nq)) 
                                   (true, q) l
      in
      b && (is_empty nq)
    with WA -> false
;;

(* Adds iter times all elements from list l to a using add function f *)
let rec add_lots f a l iter =
  if iter = 0 then a
  else
    add_lots f (List.fold_left (fun q x -> f x q) a l) l (iter - 1)

(* Size of a queue *)
let size q =
  let rec aux q a =
    if is_empty q then a
    else aux (snd (delete_min q)) (a + 1)
  in
  aux q 0

(* Wrapper for (::) *)
let app h t = h::t
;;

print_endline "Beginning test run. Expected running time is 40-50 seconds (bytecode).";;

(* Integer correctness tests *)

print_endline "Integer correctness tests... ";;

let q1 = empty |> add 3 |> add 5 |> add 10 |> add 2 |> add 2 |> add 7 |> add 22
let q2 = empty |> add 1 |> add 2 |> add 3 |> add 4 |> add 5

let q3 = join q1 q2
let q4 = join q3 q3

let l1 = List.sort compare [3; 5; 10; 2; 2; 7; 22]
let l2 = List.sort compare [1; 2; 3; 4; 5]
let l3 = List.sort compare (l1 @ l2)
let l4 = List.sort compare (l3 @ l3);;

assert(is_empty empty);;

assert(test q1 l1);;
assert(test q2 l2);;
assert(test q3 l3);;
assert(test q4 l4);;
assert(not(test q4 l3));;
assert(not(test q3 l4));;
assert(test empty []);;
assert(try test empty l1 with Empty -> true);;

print_endline "OK.";;

(* Integer performance tests *)

print_endline "Integer performance tests... ";;

let q5 = add_lots add empty [1] 50000
let q6 = add_lots add q5 [2] 50000
let q7 = add_lots add empty [1; 2; 4; 5; 8] 20000
let q8 = join (join q5 q5) (join q6 q6)
let q9 = join (join q5 q6) (join q7 q8)

let l5a = add_lots app [] [1] 50000
let l5b = 1::l5a
let l5c = List.tl(l5a)
let l6 = l5a @ (add_lots app [] [2] 50000)
let l7 = List.sort compare (add_lots app [] [1; 2; 4; 5; 8] 20000)
let l8 = List.sort compare (l5a @ (l5a @ (l6 @ l6)))
let l9 = List.sort compare (l5a @ l6 @ l7 @ l8);;

assert(test q5 l5a);;
assert(try test q5 l5b with Empty -> true);;
assert(not(test q5 l5c));;
assert(test q6 l6);;
assert(test q7 l7);;
assert(test q8 l8);;
assert(test q9 l9);;

print_endline "OK.";;

(* Float correctness tests *)

print_endline "Float correctness tests... ";;

let q1 = empty |> add 2.3 |> add 1.1 |> add 3.2 |> add 0.01 |> add 222.1 |> add 42.42 |> add 1.03
let q2 = empty |> add 1.5 |> add 2.4 |> add 3.3 |> add 4.2 |> add 5.1

let q3 = join q1 q2
let q4 = join q3 q3

let l1 = List.sort compare [2.3; 1.1; 3.2; 0.01; 222.1; 42.42; 1.03]
let l2 = List.sort compare [1.5; 2.4; 3.3; 4.2; 5.1]
let l3 = List.sort compare (l1 @ l2)
let l4 = List.sort compare (l3 @ l3);;

assert(is_empty empty);;

assert(test q1 l1);;
assert(test q2 l2);;
assert(test q3 l3);;
assert(test q4 l4);;
assert(not(test q4 l3));;
assert(not(test q3 l4));;
assert(test empty []);;
assert(try test empty l1 with Empty -> true);;

print_endline "OK.";;

(* Float performance tests *)

print_endline "Float performance tests... ";;

let q5 = add_lots add empty [-0.123] 50000
let q6 = add_lots add q5 [21.1] 50000
let q7 = add_lots add empty [-3.1; 22.1; -43.2; 115.0; 8.8] 20000
let q8 = join (join q5 q5) (join q6 q6)
let q9 = join (join q5 q6) (join q7 q8);;

let app h t = h::t;;

let l5a = add_lots app [] [-0.123] 50000;;
let l5b = (-0.123)::l5a;;
let l5c = List.tl(l5a);;
let l6 = l5a @ (add_lots app [] [21.1] 50000);;
let l7 = List.sort compare (add_lots app [] [-3.1; 22.1; -43.2; 115.0; 8.8] 20000);;
let l8 = List.sort compare (l5a @ (l5a @ (l6 @ l6)));;
let l9 = List.sort compare (l5a @ l6 @ l7 @ l8);;

assert(test q5 l5a);;
assert(try test q5 l5b with Empty -> true);;
assert(not(test q5 l5c));;
assert(test q6 l6);;
assert(test q7 l7);;
assert(test q8 l8);;
assert(test q9 l9);;

print_endline "OK.";;

(* String correctness tests *)

print_endline "String correctness tests... ";;

let q1 = empty |> add "abba" |> add "acca" |> add "baba" |> add "abbabb" |>
add "aabbab" |> add "aaabbaa" |> add "1.23"
let q2 = empty |> add "aab" |> add "aba" |> add "abb" |> add "baa" |> add "bab"

let q3 = join q1 q2
let q4 = join q3 q3

let l1 = List.sort compare ["abba"; "acca"; "baba"; "abbabb"; "aabbab"; "aaabbaa"; "1.23"]
let l2 = List.sort compare ["aab"; "aba"; "abb"; "baa"; "bab"]
let l3 = List.sort compare (l1 @ l2)
let l4 = List.sort compare (l3 @ l3);;

assert(is_empty empty);;

assert(test q1 l1);;
assert(test q2 l2);;
assert(test q3 l3);;
assert(test q4 l4);;
assert(not(test q4 l3));;
assert(not(test q3 l4));;
assert(test empty []);;
assert(try test empty l1 with Empty -> true);;

print_endline "OK.";;

(* String performance tests *)

print_endline "String performance tests... ";;

let q5 = add_lots add empty [".24dz455"] 50000
let q6 = add_lots add q5 ["forty-two"] 50000
let q7 = add_lots add empty ["lorem"; "ipsum"; "dolor"; "sit"; "amet"] 20000
let q8 = join (join q5 q5) (join q6 q6)
let q9 = join (join q5 q6) (join q7 q8);;

let app h t = h::t;;

let l5a = add_lots app [] [".24dz455"] 50000;;
let l5b = (".24dz455")::l5a;;
let l5c = List.tl(l5a);;
let l6 = l5a @ (add_lots app [] ["forty-two"] 50000);;
let l7 = List.sort compare (add_lots app [] ["lorem"; "ipsum"; "dolor"; "sit"; "amet"] 20000);;
let l8 = List.sort compare (l5a @ (l5a @ (l6 @ l6)));;
let l9 = List.sort compare (l5a @ l6 @ l7 @ l8);;

assert(test q5 l5a);;
assert(try test q5 l5b with Empty -> true);;
assert(not(test q5 l5c));;
assert(test q6 l6);;
assert(test q7 l7);;
assert(test q8 l8);;
assert(test q9 l9);;

print_endline "OK.";;

(* Other *)

print_endline "Other correctness and performance tests... ";;

let qe : int queue = empty;; 
let q1 = add qe empty;;
let q2 = add q1 empty;;
let q3 = add ([ []; [ [2] ]; [ [2; 5; 1]; [5; 11; 3] ]; [ [22; 22; 22] ] ]) empty;;
let q4 = add q3 empty;;
let q5 = qe |> join empty |> join empty |> join empty |> join empty;;
let q6 = add_lots add empty [q3] 500000;;

let l6 = add_lots app [] [q3] 500000;;

assert(is_empty qe);;
assert(not(is_empty q1));;
assert(not(is_empty q2));;
assert(not(is_empty q3));;
assert(not(is_empty q4));;
assert(is_empty q5);;
assert(test q6 l6);;

print_endline "OK.";;

print_endline "\nAll tests OK";;

