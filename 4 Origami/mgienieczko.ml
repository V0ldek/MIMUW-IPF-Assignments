(* Tests for origami task                                    *)
(* Divided into performance and correctness                  *)
(* Licensed under GNU General Public License                 *)
(* Copyright (C) 2017 Mateusz Gienieczko                     *)

open Origami;;

let centr = (0., 0.);;

print_endline "Correctness tests for zloz function:";;

print_string "Rectangle #1 ";;

let a = prostokat centr (10., 10.);;

assert(a centr = 1);;
assert(a (5., 5.) = 1);;
assert(a (10., 10.) = 1);;
assert(a (10., 0.) = 1);;
assert(a (0., 10.) = 1);;
assert(a (10.1, 0.) = 0);;
assert(a (0., 10.1) = 0);;
assert(a (10.1, 10.1) = 0);;

let a = zloz (5., 0.) (5., 377.) a;;

assert(a centr = 2);;
assert(a (-377., 0.) = 0);;
assert(a (5., 2.5) = 1);;
assert(a (2.5, 3.5) = 2);;
assert(a (5., 5.) = 1);;
assert(a (5.1, 5.) = 0);;
assert(a (5.1, 5.1) = 0);;

let a = zloz (5., 0.) (5., 1.) a;;

assert(a centr = 2);;
assert(a (-377., 0.) = 0);;
assert(a (5., 2.5) = 1);;
assert(a (2.5, 3.5) = 2);;
assert(a (5., 5.) = 1);;
assert(a (5.1, 5.) = 0);;
assert(a (5.1, 5.1) = 0);;

print_endline "OK";;

print_string "Rectangle #2 ";;

let b = zloz (-7., -7.) (300., 300.) a;;

assert(b centr = 2);;
assert(b (0., 5.) = 3);;
assert(b (2.5, 2.5) = 2);;
assert(b (1., 2.) = 4);;
assert(b (2.5, 5.) = 3);;
assert(b (2.5, 6.) = 2);;
assert(b (2.5, 2.) = 0);;
assert(b (5., 5.) = 1);;
assert(b (5., 0.) = 0);;
assert(b (4., 2.) = 0);;
assert(b (7., 9.) = 0);;
assert(b (7., 2.) = 0);;
assert(b (5., 2.5) = 0);;
assert(b (10., 0.) = 0);;
assert(b (10., 10.) = 0);;
assert(b (10., 2.5) = 0);;

print_endline "OK";;

print_string "Rectangle #3 ";;

let c = zloz (-6., -6.) (-6.1, -6.1) a;;

assert(c centr = 2);;
assert(c (0., 5.) = 0);;
assert(c (2.5, 2.5) = 2);;
assert(c (1., 2.) = 0);;
assert(c (2.5, 5.) = 0);;
assert(c (2.5, 6.) = 0);;
assert(c (2.5, 2.) = 4);;
assert(c (5., 5.) = 1);;
assert(c (5., 0.) = 3);;
assert(c (4., 2.) = 4);;
assert(c (7., 9.) = 0);;
assert(c (7., 2.) = 2);;
assert(c (7., 3.8) = 2);;
assert(c (5., 2.5) = 3);;
assert(c (10., 0.) = 2);;
assert(c (10., 10.) = 0);;
assert(c (10., 2.5) = 2);;

print_endline "OK";;

print_string "Rectangle #4 ";;

let d = zloz (9., 5.) (4., 2.) c;;

assert(d centr = 0);;
assert(d (2.9, 1.9) = 0);;
assert(d (5., 5.) = 0);;
assert(d (7., 1.) = 2);;
assert(d (7.1, 1.45) = 2);;
assert(d (7.1, 1.5) = 4);;
assert(d (7., 3.) = 4);;
assert(d (7., 3.8) = 2);;
assert(d (7., 3.81) = 0);;
assert(d (5., 0.) = 3);;
assert(d (5., 0.5) = 3);;
assert(d (5., 1.) = 7);;
assert(d (5., 2.) = 7);;
assert(d (5., 3.) = 0);;
assert(d (5., 5.) = 0);;
assert(d (9., 5.) = 1);;
assert(d (4., 0.) = 4);;
assert(d (3., 0.) = 4);;
assert(d (2., 0.) = 8);;
assert(d (1., 0.) = 8);;
assert(d (0., 0.) = 0);;
assert(d (0.8, -0.2) = 4);;
assert(d (10., 3.) = 2);;
assert(d (4., 1.) = 8);;

print_endline "OK";;

print_string "Circle #1 ";;

let a = kolko (3., 3.) 7.;;

assert(a centr = 1);;
assert(a (3., 3.) = 1);;
assert(a (8., 7.5) = 1);;
assert(a (10., 3.) = 1);;
assert(a (3., 10.) = 1);;
assert(a (-4., 3.) = 1);;
assert(a (3., -4.) = 1);;
assert(a (10.1, 3.) = 0);;
assert(a (10., 3.1) = 0);;
assert(a (-4.1, 3.) = 0);;
assert(a (-3.9, 3.) = 1);;

let a = zloz (5., -10.) (5., 100.) a;;

assert(a centr = 1);;
assert(a (0.67, 0.) = 1);;
assert(a (0.68, 0.) = 2);;
assert(a (0.69, 0.69) = 2);;
assert(a (1., 0.) = 2);;
assert(a (2., 2.) = 2);;
assert(a (3., 0.) = 2);;
assert(a (5., 0.) = 1);;
assert(a (5.1, 0.) = 0);;
assert(a (3., 3.) = 2);;
assert(a (3., 10.) = 1);;
assert(a (-1., -1.) = 1);;
assert(a (7., 7.) = 0);;
assert(a (10., 0.) = 0);;

let a = zloz (5., 0.) (5., 0.01) a;;

assert(a centr = 1);;
assert(a (0.67, 0.) = 1);;
assert(a (0.68, 0.) = 2);;
assert(a (0.69, 0.69) = 2);;
assert(a (1., 0.) = 2);;
assert(a (2., 2.) = 2);;
assert(a (3., 0.) = 2);;
assert(a (5., 0.) = 1);;
assert(a (5.1, 0.) = 0);;
assert(a (3., 3.) = 2);;
assert(a (3., 10.) = 1);;
assert(a (-1., -1.) = 1);;
assert(a (7., 7.) = 0);;
assert(a (10., 0.) = 0);;

print_endline "OK";;

print_string "Circle #2 ";;

let a = zloz (1., 0.) (1., -1.) a;;

assert(a centr = 0);;
assert(a (1., 0.) = 2);;
assert(a (1.1, 0.) = 4);;
assert(a (5., 3.) = 2);;
assert(a (3., 3.) = 3);;
assert(a (7., 2.) = 0);;
assert(a (6., 3.) = 1);;
assert(a (6., 2.9) = 0);;
assert(a (6.1, 3.) = 0);;

print_endline "OK";;

print_string "Circle #3 ";;

let a = zloz (5., 10.) (1., 0.) a;;

assert(a centr = 0);;
assert(a (1., 0.) = 2);;
assert(a (2., 0.) = 3);;
assert(a (5., 0.) = 2);;
assert(a (6., 0.) = 0);;
assert(a (2., 2.) = 7);;
assert(a (3., 3.) = 7);;
assert(a (5., 5.) = 5);;
assert(a (6., 6.) = 2);;
assert(a (8., 8.) = 0);;
assert(a (4., 3.) = 3);;
assert(a (5., 3.) = 2);;
assert(a (6., 3.) = 1);;
assert(a (7., 3.) = 0);;
assert(a (1., -1.) = 1);;
assert(a (1., -3.) = 1);;
assert(a (1., -4.) = 0);;
assert(a (3., -1.) = 3);;
assert(a (3., -2.) = 3);;
assert(a (3., -3.) = 1);;
assert(a (3., -4.) = 1);;
assert(a (3., -5.) = 0);;

print_endline "OK";;

print_string "Circle #4 ";;

let a = zloz (1., 0.) (5., 10.) a;;

assert(a centr = 3);;
assert(a (1., 0.) = 2);;
assert(a (2., 0.) = 0);;
assert(a (5., 0.) = 0);;
assert(a (6., 0.) = 0);;
assert(a (2., 2.) = 0);;
assert(a (3., 3.) = 0);;
assert(a (5., 5.) = 0);;
assert(a (6., 6.) = 0);;
assert(a (8., 8.) = 0);;
assert(a (4., 3.) = 0);;
assert(a (5., 3.) = 0);;
assert(a (6., 3.) = 0);;
assert(a (7., 3.) = 0);;
assert(a (1., -1.) = 0);;
assert(a (1., -3.) = 0);;
assert(a (1., -4.) = 0);;
assert(a (3., -1.) = 0);;
assert(a (3., -2.) = 0);;
assert(a (3., -3.) = 0);;
assert(a (3., -4.) = 0);;
assert(a (3., -5.) = 0);;
assert(a (0., 4.) = 3);;
assert(a (0., 5.) = 1);;
assert(a (0., 6.) = 1);;
assert(a (0., 7.) = 0);;
assert(a (0., -1.) = 2);;
assert(a (0., -2.) = 0);;
assert(a (2., 3.) = 7);;
assert(a (1., 3.) = 5);;
assert(a (0., 3.) = 3);;
assert(a (-1., 3.) = 3);;
assert(a (-2., 3.) = 1);;
assert(a (-3., 3.) = 0);;
assert(a (1., 5.) = 5);;
assert(a (2., 5.) = 6);;
assert(a (3., 5.) = 3);;
assert(a (4., 5.) = 0);;
assert(a (3., 6.) = 6);;
assert(a (3., 4.) = 0);;
assert(a (3., 7.) = 6);;
assert(a (3., 8.) = 3);;
assert(a (3., 9.) = 1);;
assert(a (3., 10.) = 1);;
assert(a (3., 10.1) = 0);;

print_endline "OK";;


print_endline "Correctness tests for skladaj function:";;

print_string "Rectangle ";;

let l = [((5., 0.), (5., 377.)); ((5., 0.), (5., 1.));
	 ((-6., -6.), (-6.1, -6.1)); ((9., 5.), (4., 2.))];;

let a = prostokat centr (10., 10.);;

let a = skladaj l a;;

assert(a centr = 0);;
assert(a (2.9, 1.9) = 0);;
assert(a (5., 5.) = 0);;
assert(a (7., 1.) = 2);;
assert(a (7.1, 1.45) = 2);;
assert(a (7.1, 1.5) = 4);;
assert(a (7., 3.) = 4);;
assert(a (7., 3.8) = 2);;
assert(a (7., 3.81) = 0);;
assert(a (5., 0.) = 3);;
assert(a (5., 0.5) = 3);;
assert(a (5., 1.) = 7);;
assert(a (5., 2.) = 7);;
assert(a (5., 3.) = 0);;
assert(a (5., 5.) = 0);;
assert(a (9., 5.) = 1);;
assert(a (4., 0.) = 4);;
assert(a (3., 0.) = 4);;
assert(a (2., 0.) = 8);;
assert(a (1., 0.) = 8);;
assert(a (0., 0.) = 0);;
assert(a (0.8, -0.2) = 4);;
assert(a (10., 3.) = 2);;
assert(a (4., 1.) = 8);;

print_endline "OK";;

print_string "Circle ";;

let l = [((5., -10.), (5., 100.)); ((5., 0.), (5., 0.01));
	 ((1., 0.), (1., -1.)); ((5., 10.), (1., 0.));
	 ((1., 0.), (5., 10.))];;

let a = kolko (3., 3.) 7.;;

let a = skladaj l a;;

assert(a centr = 3);;
assert(a (1., 0.) = 2);;
assert(a (2., 0.) = 0);;
assert(a (5., 0.) = 0);;
assert(a (6., 0.) = 0);;
assert(a (2., 2.) = 0);;
assert(a (3., 3.) = 0);;
assert(a (5., 5.) = 0);;
assert(a (6., 6.) = 0);;
assert(a (8., 8.) = 0);;
assert(a (4., 3.) = 0);;
assert(a (5., 3.) = 0);;
assert(a (6., 3.) = 0);;
assert(a (7., 3.) = 0);;
assert(a (1., -1.) = 0);;
assert(a (1., -3.) = 0);;
assert(a (1., -4.) = 0);;
assert(a (3., -1.) = 0);;
assert(a (3., -2.) = 0);;
assert(a (3., -3.) = 0);;
assert(a (3., -4.) = 0);;
assert(a (3., -5.) = 0);;
assert(a (0., 4.) = 3);;
assert(a (0., 5.) = 1);;
assert(a (0., 6.) = 1);;
assert(a (0., 7.) = 0);;
assert(a (0., -1.) = 2);;
assert(a (0., -2.) = 0);;
assert(a (2., 3.) = 7);;
assert(a (1., 3.) = 5);;
assert(a (0., 3.) = 3);;
assert(a (-1., 3.) = 3);;
assert(a (-2., 3.) = 1);;
assert(a (-3., 3.) = 0);;
assert(a (1., 5.) = 5);;
assert(a (2., 5.) = 6);;
assert(a (3., 5.) = 3);;
assert(a (4., 5.) = 0);;
assert(a (3., 6.) = 6);;
assert(a (3., 4.) = 0);;
assert(a (3., 7.) = 6);;
assert(a (3., 8.) = 3);;
assert(a (3., 9.) = 1);;
assert(a (3., 10.) = 1);;
assert(a (3., 10.1) = 0);;

print_endline "OK";;

print_endline "Performance tests (estimated time: 1 minute):";;

print_endline "Rectangle...";;

let gen n =
  let rec aux acc i =
    if i > n then acc
    else
      aux (((float_of_int(1 lsl i), 0.),
	   (float_of_int(1 lsl i), 1.))::acc) (i + 1)
  in
  aux [] 0;;

let const = 24;;

let l = gen const;;

let a = prostokat ((-.max_float) /. 4., (-.max_float) /. 4.)
                  (max_float /. 4., max_float /. 4.);;

let a = skladaj l a;;

assert(a centr = (1 lsl const) + 1);;
assert(a (1., 1.) = 1 lsl const);;
assert(a (-1., -1.) = 2);;
assert(a ((-.max_float) /. 2., (-.max_float) /. 2.) = 0);;
assert(a (max_float /. 4., max_float /. 4.) = 0);;

for i = -2 downto -100000 do
  assert(a (float_of_int(i), float_of_int(i)) = 2)
done;;

print_endline "OK";;

print_endline "Circle...";;
flush_all;;

let const = 24;;

let l = gen const;;

let a = kolko centr (max_float /. 4.);;

let a = skladaj l a;;

assert(a centr = (1 lsl const) + 1);;
assert(a (1., 1.) = 1 lsl const);;
assert(a (-1., -1.) = 2);;
assert(a ((-.max_float) /. 2., (-.max_float) /. 2.) = 0);;
assert(a (max_float /. 4., max_float /. 4.) = 0);;

for i = -2 downto -100000 do
  assert(a (float_of_int(i), float_of_int(i)) = 2)
done;;

print_endline "OK";;

print_endline "All tests OK.";;


let a = prostokat (-100., -100.) (-99., -99.);;

let const = 5000;;

let gen n =
  let rec aux acc i =
    if i > n then acc
    else 
      aux (((float_of_int i, 0.), (float_of_int i, 1.))::acc) (i + 1)
  in
  aux [] 0

let l = gen const

let a = skladaj l a;;

