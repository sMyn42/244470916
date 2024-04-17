(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                   Part 1: Mutable Lists and Cycles
 *)

(* The type of mutable lists. *)
type 'a mlist = 'a mlist_internal ref
 and 'a mlist_internal =
   | Nil
   | Cons of 'a * 'a mlist ;;

(*......................................................................
Problem 1.1: Write a function `has_cycle` that returns `true` if a
mutable list has a cycle, `false` otherwise. You may want a recursive
auxiliary function. You needn't worry about space usage of your
function.

For instance, we can establish a cyclic and an acyclic mutable list
like this:

    # let sample_end = ref Nil ;;
    # let cyclic = ref (Cons (1, ref (Cons (2, sample_end)))) ;;
    # sample_end := !cyclic ;;
    # let acyclic = ref (Cons (3, ref (Cons(4, ref Nil)))) ;;

and test for cycles using `has_cycle`:

    # has_cycle cyclic ;;
    - : bool = true
    # has_cycle acyclic ;;
    - : bool = false
......................................................................*)
                                      
let has_cycle (lst : 'a mlist) : bool =
  let rec cycle_helper (mlst : 'a mlist) (lst : 'a mlist_internal list) : bool =
    match !mlst with
    | Nil -> false
    | Cons (_h1, t1) ->
      match !t1 with
      | Nil -> false
      | Cons (_h2, t2) ->
        if List.exists (fun x -> x == !t2) lst then true
        else cycle_helper t1 (!t1 :: lst) in
  cycle_helper lst [] ;;

(*......................................................................
Problem 1.2: Write a function `flatten` that flattens a list (removes
its cycles if it has any) destructively. Again, you may want a
recursive auxiliary function, and you shouldn't worry about space.
......................................................................*)

let flatten (lst : 'a mlist) : unit =
  let rec flatten_helper (mlst : 'a mlist) (lst : 'a mlist_internal list) : unit =
    match !mlst with
    | Nil -> ()
    | Cons (_h1, t1) ->
      match !t1 with
      | Nil -> ()
      | Cons (_h2, t2) ->
        if List.exists (fun x -> x == !t2) lst then
          match !t2 with
          | Cons (_h3, t3) -> t3 := Nil
          | Nil -> ()
        else flatten_helper t1 (!t1 :: lst) in
  if has_cycle lst then (flatten_helper lst []) else () ;;

(*......................................................................
Problem 1.3: Write a function `mlength`, which nondestructively returns
the number of nodes (that is `Cons`es) in a mutable list that may have
cycles.
......................................................................*)
let mlength (lst : 'a mlist) : int =
  let rec mlength_helper (mlst : 'a mlist) (lst : 'a mlist_internal list) : int =
    match !mlst with
    | Nil -> 1 + List.length lst
    | Cons (_h1, t1) ->
      match !t1 with
      | Nil -> 1 + List.length lst
      | Cons (_h2, t2) ->
        if List.exists (fun x -> x == !t2) lst then 1 + List.length lst
        else 1 + mlength_helper t1 (!t1 :: lst) in
  mlength_helper lst [] ;;

(*======================================================================
Reflection on the problem set

After each problem set, we'll ask you to reflect on your experience.
We care about your responses and will use them to help guide us in
creating and improving future assignments.

........................................................................
Please give us an honest (if approximate) estimate of how long (in
minutes) this problem set (in total, not just this file) took you to
complete. (If you worked with a partner, we're asking for how much time
each of you (on average) spent on the problem set, not in total.)
......................................................................*)

let minutes_spent_on_pset () : int =
  720 ;;

(*......................................................................
It's worth reflecting on the work you did on this problem set, where
you ran into problems and how you ended up resolving them. What might
you have done in retrospect that would have allowed you to generate as
good a submission in less time? Please provide us your thoughts in the
string below.
......................................................................*)

let reflection () : string =
  "Pretty good pset, though it took a while. I didn't actually get the chance to listen, but I hope they're correct.";;
