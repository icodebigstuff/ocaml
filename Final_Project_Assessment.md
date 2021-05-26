# Final_Project Assessment

Run on December 20, 17:17:33 PM.

*Test results are for the code in this repository as it appeared on December 19 at 00:15 CT.*

+ Pass: Change into directory "Final_Project".

The following tests that check if certain files exist are here only to indicate in the gradebook which option you choose.  These scoress should be ignored; they are not part of the total score for this assignment.

+  _0_ / _1_ : Fail: Check that file `hwk_02.ml` exists.

     `hwk_02.ml` not found.

+ Fail: Check that file `hwk_02.ml` exists.

     `hwk_02.ml` not found.

+  _1_ / _1_ : Pass: Check that file `qu_quiz.ml` exists.

+ Pass: Check that file `qu_quiz.ml` exists.

+  _1_ / _1_ : Pass: Check that file `it_takes_two.ml` exists.

+ Pass: Check that file `it_takes_two.ml` exists.

+  _0_ / _1_ : Fail: Check that file `expr.ml` exists.

     `expr.ml` not found.

+ Fail: Check that file `expr.ml` exists.

     `expr.ml` not found.

+ Pass: Testing continuing even if some previous tests fail.

## Running tests for Final Project option 2

It appears that you have decided to submit work for this option. If you did not intend to submit work for this option please email the TAs at csci2041@umn.edu to indicate that the option selection mechanism has failed.



Because `qu_quiz.ml` is in your `Final_Project` directory you have choosen option 2.  Below are tests for the word problems.

The files `words-small.txt` and has had any words that are involved in two or more answers removed and was then sorted. The results are in the file `words-small-v3.txt` that is automatically copied here to run these tests.

### QU Quiz tests

+ Pass: Check that an OCaml file `qu_quiz.ml` has no syntax  or type errors.

    OCaml file `qu_quiz.ml` has no syntax or type errors.



+ Pass: Ensure that contents of file "words-small-v3.txt" match what was provided.

+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem ("crooner", "conqueror") (qu_quiz "words-small-v3.txt")
   ```
   matches the pattern `true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem ("stir", "squirt") (qu_quiz "words-small-v3.txt")
   ```
   matches the pattern `true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem ("teens", "sequent") (qu_quiz "words-small-v3.txt")
   ```
   matches the pattern `true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem ("italy", "quality") (qu_quiz "words-small-v3.txt")
   ```
   matches the pattern `true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   List.length (qu_quiz "words-small-v3.txt")
   ```
   matches the pattern `4`.

   




+  _5_ / _5_ : Pass: The code for QU Quiz makes a reasonable effort to solve the problem.

    

### It Takes Two tests

+ Pass: Check that file `it_takes_two.ml` exists.

Because `it_takes_two.ml` is in your `Final_Project` directory you have choosen option 2 B as well.

+ Pass: Check that an OCaml file `it_takes_two.ml` has no syntax  or type errors.

    OCaml file `it_takes_two.ml` has no syntax or type errors.



+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem ("lane", "planet") (it_takes_two "words-small-v3.txt")
   ```
   matches the pattern `true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem ("refi", "prefix") (it_takes_two "words-small-v3.txt")
   ```
   matches the pattern `true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem ("nigh", "knight") (it_takes_two "words-small-v3.txt")
   ```
   matches the pattern `true`.

   




+  _3_ / _3_ : Pass: The code for It Takes Two makes a reasonable effort to solve the problem.

    be careful of long long lines!!

#### Total score: _24_ / _24_

