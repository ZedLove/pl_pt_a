(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3.sml";
(*
val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2a = longest_string1 ["sixteen", "one", "two", "three", "four", "fifteen"] = "sixteen"
val test2b = longest_string1 ["fives", "one", "two", "three", "seven"] = "fives"
val test2c = longest_string1 [ "one", "two", "three", "seven"] = "three"
									     
val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3a = longest_string2 ["sixteen", "one", "two", "three", "four", "fifteen"] = "fifteen"
val test3b = longest_string2 ["fives", "one", "two", "three", "seven"] = "seven"
val test3c = longest_string2 [ "one", "two", "three", "seven"] = "seven"
									     
val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8b = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []

val test9a = count_wildcards Wildcard = 1
val test9a2 = count_wildcards(Variable("var")) = 0

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b2 = count_wild_and_variable_lengths Wildcard = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true
val test10a = check_pat(ConstructorP ("hi",TupleP[Variable "x",Variable "x"])) = false
val test10b = check_pat(ConstructorP("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])])) = false
*)														      
val test10c = check_pat(ConstructorP ("egg",ConstructorP ("egg",ConstP 4)))
val test10d = check_pat(TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))])
val test10e = check_pat(TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)])
val test10f = check_pat(TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]])
(*					     
val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
*)
