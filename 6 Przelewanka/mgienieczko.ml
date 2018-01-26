(* Tests for topol task                                      *)
(* Divided into performance and correctness                  *)
(* Licensed under GNU General Public License                 *)
(* Copyright (C) 2017 Mateusz Gienieczko                     *)

open Przelewanka;;

Printf.printf "\027[33m
=============================================================================
Mateusz Gienieczko's tests to Przelewanka task.
(I learnt how to use colors, now nothing's the same.)
=============================================================================";;
print_newline ();;

let num = ref 1;;

let test a answ =
  Printf.printf "\027[34mTest #%d: \027[31m" !num;
  flush stdout;
  incr num;
  let start_time = Sys.time ()
  in
  let usr_answ = przelewanka a
  in
  if answ <> usr_answ then
    Printf.printf "\027[31mWrong answer: read %d, expected %d " usr_answ answ
  else
    Printf.printf "\027[32mOK ";
  Printf.printf "in %.2f s" ((Sys.time ()) -. start_time);
  print_newline ();
  answ = usr_answ
;;

Printf.printf "\027[34mBeginning simple, small tests";;
print_newline ();;

let a = [|(1, 1); (2, 1)|];;
assert(test a 2);;

let a = [||];;
assert(test a 0);;

let a = [|(10, 5); (4, 3); (3, 2); (2, 0)|];;
assert(test a 5);;

let a = [|(50, 50); (50, 48); (2, 2)|];;
assert(test a 3);;

let a = [|(50, 50); (50, 47); (2, 2)|];;
assert(test a (-1));;

let a = [|(13, 9); (17, 3); (7, 2); (2, 2)|];;
assert(test a 9);;

let a = [|(1, 0); (1000000, 999999)|];;
assert(test a 3);;

let a = [|(1, 0); (1000000, 999997)|];;
assert(test a 7);;

let a = [|(9, 6); (12, 9); (12, 3); (999, 411)|];;
assert(test a (-1));;

let a = [|(37, 35); (55, 36)|];;
assert(test a (-1));;

let a = [|(2, 1); (0, 0); (4, 2);|];;
assert(test a (-1));;

let a = [|(0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (99, 66); (3, 3)|];;
assert(test a 22);;

let a = [|(37, 3); (42, 37); (69, 33)|];;
assert(test a (-1));;

let a = [|(1, 0); (1000, 999); (1000000, 999999); (1000000000000, 999999999999)|];;
assert(test a 9);;

let a = [|(24, 13); (12, 5); (6, 2); (1, 0)|];;
assert(test a 10);;

let a = [|(100, 0); (50, 0); (100000, 0); (35, 0)|];;
assert(test a 0);;

Printf.printf "\027[33m\
=============================================================================
These may take a little longer, but not more than 10 - 30 seconds each
=============================================================================";;
print_newline ();;

let a = [|(100, 50); (1000, 500); (50, 25); (5, 5)|];;
assert(test a 20);;

let a = [|(1, 0); (2, 1); (4, 3); (8, 7); (16, 15); (32, 31)|];;
assert(test a 11);;

let a = [|(100, 33); (25, 11); (13, 11); (3, 0); (1, 1)|];;
assert(test a 13);;

let a = [|(1, 0); (2, 1); (3, 1); (4, 2); (5, 2); (6, 3); (7, 3); (8, 4)|];;
assert(test a 10);;

let a = [|(6, 3); (9, 3); (12, 3); (15, 3); (18, 3); (477, 3); (0, 0)|];;
assert(test a (-1));;

let a = [|(1000, 999); (2000, 1998); (2, 0); (0, 0)|];;
assert(test a (-1));;

let a = [|(1000, 498); (2000, 1498); (2, 0)|];;
assert(test a 503);;

let a = [|(1, 0); (77, 43); (150, 149); (333, 37); (37, 2)|];;
assert(test a 12);;

let a = [|(5, 0); (50, 25); (500, 200); (2500, 1500)|];;
assert(test a 23);;

let a = [|(1, 0); (1000000, 500002)|];;
assert(test a 999997);;
	 
let a = [|(1, 0); (1000000, 499991)|];;
assert(test a 999982);;

let a = [|(831, 132); (17, 3); (81, 54); (9, 9)|];;
assert(test a 22);;

let a = [|(4, 0); (6, 4); (20, 12); (50, 20); (101, 98)|];;
assert(test a 13);;

let a = [|(3, 3); (11, 8); (22222, 12321)|];;
assert(test a 1807);;

let a = [|(999, 1); (100, 1); (1000, 1); (0, 0)|];;
assert(test a (-1));;

let a = [|(999,1); (100, 1); (1000, 1); (2, 0); (8, 1)|];;
assert(test a 13);;

Printf.printf "\027[33m\
=============================================================================
All tests \027[32mOK!
\027[33m\
=============================================================================";;
print_newline ();;
