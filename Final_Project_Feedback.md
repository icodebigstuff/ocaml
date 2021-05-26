# Final_Project Feedback

Run on December 13, 12:07:45 PM.

+ Pass: Change into directory "Final_Project".

## Running tests for Final Project option 2

It appears that you have decided to submit work for this option. If you did not intend to submit work for this option please email the TAs at csci2041@umn.edu to indicate that the option selectin mechanism has failed.



Because `qu_quiz.ml` is in your `Final_Project` directory you have choosen option 2.  Below are tests for the word problems.

The files `words-small.txt` and has had any words that are involved in two or more answers removed and was then sorted. The results are in the file `words-small-v2.txt` that is automatically copied here to run these tests.

+ Pass: Ensure that contents of file "words-small-v2.txt" match what was provided.

### QU Quiz tests

+ Pass: Check that an OCaml file `qu_quiz.ml` has no syntax  or type errors.

    OCaml file `qu_quiz.ml` has no syntax or type errors.



+ Pass: Ensure that contents of file "words-small-v2.txt" match what was provided.

+ Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem ("crooner", "conqueror") (qu_quiz "words-small-v2.txt")
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem ("stir", "squirt") (qu_quiz "words-small-v2.txt")
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem ("teens", "sequent") (qu_quiz "words-small-v2.txt")
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem ("italy", "quality") (qu_quiz "words-small-v2.txt")
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```ocaml
   List.length (qu_quiz "words-small-v2.txt")
   ```
   matches the pattern `4`.

   




### It Takes Two tests

+ Pass: Check that file `it_takes_two.ml` exists.

Because `it_takes_two.ml` is in your `Final_Project` directory you have choosen option 2 B as well.

+ Pass: Check that an OCaml file `it_takes_two.ml` has no syntax  or type errors.

    OCaml file `it_takes_two.ml` has no syntax or type errors.



+ Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem ("lane", "planet") (it_takes_two "words-small-v2.txt")
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem ("refi", "prefix") (it_takes_two "words-small-v2.txt")
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```ocaml
   List.mem ("nigh", "knight") (it_takes_two "words-small-v2.txt")
   ```
   matches the pattern `true`.

   




