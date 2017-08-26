(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw1.sml";

val l = [(2012,2,28),(2011,3,30),(2011,3,31),(2011,4,28),(2016,3,3),(2018,3,4)];

val test1 = is_older ((1,2,3),(2,3,4)) = true
val test1_a = is_older ((2017,1,1),(12,12,12)) = false
val test1_b = is_older ((2011,1,1),(2011,1,2)) = true
val test1_c = is_older ((2018,1,2),(2018,1,2)) = false
val test1_d = is_older ((0,0,0),(0,0,0)) = false 
						     
val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2_a = number_in_month (l, 3) = 4
val test2_b = number_in_month ((2000,7,12)::l, 6) = 0
val test2_c = number_in_month([], 0) = 0

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3_a = number_in_months ([(2012,12,28),(2013,12,1),(2011,3,31),(2011,4,28),(12,1,12)],[12,3,4]) = 4
val test3_b = number_in_months ([(2012,12,28),(2013,12,1),(2011,12,31),(2011,12,28)],[1,2,3,4,5,6,7,8,9,10,11]) = 0
val test3_c = number_in_months ([(2012,12,28),(2013,12,1),(2011,12,31),(2011,12,28)],[]) = 0
val test3_d = number_in_months ([],[1]) = 0
val test3_e = number_in_months ([],[]) = 0
					      
val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4_a = dates_in_month (l,12) = []
val test4_b = dates_in_month ([],2) = []
val test4_c = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4_d = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
								 
val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6_a = get_nth (["hi", "there", "how", "are", "you"], 0) = ""
val test6_b = get_nth (["hi", "there", "how", "are", "you"], 12) = ""

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
