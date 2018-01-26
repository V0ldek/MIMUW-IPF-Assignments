(* Tests for Arytmetyka task                 *)
(* Performance is not tested                 *)
(* Licensed under GNU General Public License *)
(* Copyright (C) 2017 Mateusz Gienieczko     *)

open Arytmetyka

let a = wartosc_od_do 3. 7.;;                (* [3., 7.]                      *)

assert(min_wartosc a = 3.0);;
assert(max_wartosc a = 7.0);;
assert(in_wartosc a 4.);;
assert(not (in_wartosc a 2.));;

let b = wartosc_od_do (-2.) 5.;;             (* [-2., 5.]                     *)

assert(sr_wartosc b = 1.5);;
assert(min_wartosc b = -2.);;
assert(max_wartosc b = 5.);;
assert(in_wartosc b (-0.));;

let c = podzielic a b;;                      (* [-inf, -1.5] U [0.6, inf]     *)

assert(not (in_wartosc c 0.));;
assert(in_wartosc c 100.);;

let d = podzielic c b;;                      (* [-inf, -0.3] U [0.12, inf]    *)

assert(compare (sr_wartosc d) nan = 0);;
assert(in_wartosc d (-3. /. 10.));;
assert(not (in_wartosc d (-3. /. 10.000000001)));;
assert(max_wartosc d = infinity);;
assert(min_wartosc d = neg_infinity);;

let e = plus d (wartosc_dokladna 2.);;       (* [-inf, 1.7] U [2.12, inf]     *)

assert(in_wartosc e 0.);;
assert(in_wartosc e 1.7);;
assert(in_wartosc e 2.12);;
assert(not (in_wartosc e 1.700000000001));;
assert(not (in_wartosc e 2.119999999999));;

let f = razy d b;;                           (* [-inf, inf]                   *)

assert(in_wartosc f 1000000.231232333);;
assert(in_wartosc f (-3.14159));;
assert(min_wartosc f = neg_infinity);;
assert(max_wartosc f = infinity);;
assert(compare (sr_wartosc f) nan = 0);;
assert(in_wartosc f (-0.2));;
assert(in_wartosc f (0.11));;
assert(in_wartosc f 0.0);;
assert(in_wartosc f (-0.0));;

let g = minus d (wartosc_dokladna 3.);;      (* [-inf, -3.3] U [-2.88, inf]   *)

assert(compare (sr_wartosc g) nan = 0);;
assert(in_wartosc g (-4.));;
assert(not (in_wartosc g (-3.)));;
assert(in_wartosc g (-2.));;

let h = wartosc_dokladna 0.;;                (* [0., 0.]                      *)
let i = wartosc_dokladna (-0.);;             (* [0., 0.]                      *)

assert((min_wartosc h) = (min_wartosc i));;
assert((max_wartosc h) = (max_wartosc i));;
assert((sr_wartosc h) = (sr_wartosc i));;
assert((min_wartosc h) = 0.);;
assert((max_wartosc h) = 0.);;
assert((sr_wartosc h) = 0.);;


let j = podzielic f i;;                      (* empty set                     *)
let k = podzielic b h;;                      (* empty set                     *)

assert(compare (min_wartosc j) (min_wartosc k) = 0);;
assert(compare (max_wartosc j) (max_wartosc k) = 0);;
assert(compare (min_wartosc j) nan = 0);;
assert(compare (max_wartosc j) nan = 0);;
assert(compare (sr_wartosc j) (sr_wartosc k) = 0);;
assert(compare (sr_wartosc j) nan = 0);;

let l = razy g (wartosc_dokladna (1.1));;    (* [-inf, -3.63] U [-3.168, inf] *)

assert(compare (sr_wartosc l) nan = 0);;
assert(in_wartosc l (-3.63));;
assert(not (in_wartosc l (-3.62)));;
assert(not (in_wartosc l (-3.169)));;
assert(in_wartosc l (-3.168));;
assert(in_wartosc l 0.0);;

let m = wartosc_dokladnosc (-3.0) (273.3);; (* [-11.199, 5.199]               *)

assert(min_wartosc m = -11.199);;
assert(max_wartosc m = 5.199);;
assert(sr_wartosc m = (-3.0));;
assert(in_wartosc m 0.0);;

print_endline "All tests OK.";;

