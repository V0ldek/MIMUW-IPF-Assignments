# IPF-Assignments
Assignments from Introduction to Programming (Functional approach), 
MIM UW, year 1 (language: OCaml)

Copyright (C) 2017-2018 Mateusz Gienieczko

This repository contains 6 problems given as credited assignments
to be implemented in OCaml on the first year of Informatics at MIM UW.

The solutions and tests are written by me only, but for the assignments
we were working in a peer review system -- in pairs we reviewed each other's
codes, tested them and pointed out mistakes and possible improvements.
Each tasks' reviewer is listed in the header comment in the solution files.

There were two weeks for the completion of each task.

Solutions in this repository are correct, with correct meaning they passed
all the official and unofficial tests, and were graded for maximum points.

Each problem folder contains:

- Short task description in ID.txt
- OCaml interface file for the task in ID.mli
- My own solution in OCaml in ID.ml
- My own tests in mgienieczko.ml

Some tasks (namely iSet and Topol) containt additional libraries
provided along with the assignment.

Some of the function names in the solutions are in Polish 
(as demanded by the specification), 
but all comments and descriptions are in English.

The interface files were not changed, hence they're mainly in Polish.

Some descriptions contain my own notes, marked as
["note" - own. note] (owner's note)

The tests are designed to cover most corner cases, generic cases as well as
performance where it is of essence. Note that the estimate execution times
included in some of the tests are for bytecode-compiled files, native will
be much faster.

