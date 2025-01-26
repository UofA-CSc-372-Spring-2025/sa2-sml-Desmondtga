(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Desmond Goodman-Ahearn             *)
(* Time spent on HW6:
*)

(* Collaborators and references:
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)

fun mynull []       = true
  | mynull (_::_)   = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true

val () =
    Unit.checkExpectWith Bool.toString "mynull [1] should be false"
    (fn () => mynull [1])
    false

val () =
    Unit.checkExpectWith Bool.toString "mynull [1,2,3] should be false"
    (fn () => mynull [1,2,3])
    false

val () =
    Unit.checkExpectWith Bool.toString "mynull [[],[],[]] should be false"
    (fn () => mynull [[],[],[]])
    false

(**** Problem B ****)

fun firstVowel (#"a"::_) = true
  | firstVowel (#"e"::_) = true
  | firstVowel (#"i"::_) = true
  | firstVowel (#"o"::_) = true
  | firstVowel (#"u"::_) = true
  | firstVowel _ = false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'bck' should be false"
    (fn () => firstVowel [#"b",#"c",#"k"])
    false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'a' should be true"
    (fn () => firstVowel [#"a"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'e' should be true"
    (fn () => firstVowel [#"e"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'i' should be true"
    (fn () => firstVowel [#"i"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'o' should be true"
    (fn () => firstVowel [#"o"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'u' should be true"
    (fn () => firstVowel [#"u"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel '' should be false"
    (fn () => firstVowel [])
    false

(**** Problem C ****)

fun reverse l = foldl (fn (x, acc) => x::acc) [] l

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [] should be []"
  (fn () => reverse [])
  []

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1] should be [1]"
  (fn () => reverse [1])
  [1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2,3] should be [3,2,1]"
  (fn () => reverse [1,2,3])
  [3,2,1]

(**** Problem D ****)

fun minlist [] = raise Match
  | minlist (x0::xs) = foldl (fn (x, m) => Int.min(m, x)) x0 xs

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4] should be 1"
  (fn () => minlist [1,2,3,4])
  1

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,~3,4] should be ~3"
  (fn () => minlist [1,2,~3,4])
  ~3

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1] should be 1"
  (fn () => minlist [1])
  1

(**** Problem E ****)

exception Mismatch

fun zip ([], (_::_)) = raise Mismatch
  | zip ((_::_), []) = raise Mismatch
  | zip ([], []) = []
  | zip (x::xs, y::ys) = (x,y) :: zip (xs, ys)

val () =
  Unit.checkExnWith (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip ([], [1]) should raise an exception"
  (fn () => zip ([]: int list, [1]))

val () =
  Unit.checkExnWith (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip ([1], []) should raise an exception"
  (fn () => zip ([1], []: int list))

val () =
  Unit.checkExpectWith (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip ([], []) should return []"
  (fn () => zip ([], []))
  []

val () =
  Unit.checkExpectWith (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip ([1], [1]) should return [(1,1)]"
  (fn () => zip ([1], [1]))
  [(1,1)]

val () =
  Unit.checkExpectWith (Unit.listString (Unit.pairString Int.toString Int.toString))
  "zip ([1], [1]) should return [(1,1)]"
  (fn () => zip ([1,2], [2,1]))
  [(1,2),(2,1)]

(**** Problem F ****)

(* fun concat xs = xs *)

(**** Problem G ****)
(*
fun isDigit _    = false;
*)
(**** Problem H ****)
(*
fun isAlpha c = false
*)
(**** Problem I ****)
(*
fun svgCircle (cx, cy, r, fill) = "NOT IMPLEMENTED YET"

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";
*)
(**** Problem J ****)
(*
fun partition p (x :: xs) = ([],[])

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);
*)

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
