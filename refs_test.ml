(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                             Refs Testing
 *)

open CS51Utils ;;
open Absbook ;;

(* Make your refs solution available for testing *)
open Refs ;;

(* Establish some mutable lists for testing. *)
let list1a = ref (Cons (2, ref Nil)) ;;
let list1b = ref (Cons (2, list1a)) ;;
let list1 = ref (Cons (1, list1b)) ;;

let listend = ref (Cons (2, ref Nil)) ;;
let list2 = ref (Cons (1, ref (Cons (2, listend)))) ;;
let _ = listend := !list2 ;;

(* Some example tests. You'll want more. *)
let tests () =
  unit_test (not (has_cycle list1)) "list1 has no cycle";
  unit_test (has_cycle list2)       "list2 has cycle";

  (* More tests go here. . . *) 
  unit_test (mlength list1 == 5) "list1 has length 5";
  unit_test (mlength list2 == 3) "list2 has length 3";
  flatten list2;
  unit_test (not (has_cycle list2)) "flatten list2 works";
  unit_test (mlength list2 == 3) "(flattened) list2 has length 3";
  () ;;

(* Run all the tests *)
let _ = tests () ;;
