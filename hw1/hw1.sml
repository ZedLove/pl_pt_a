(* Zak Nitsch, Homework1, Programming Languages Part A *)

(* Dates are of type int*int*int : year*month*day
   A “reasonable” date has a positive year, a month between 1 and 12, and a day no
   greater than 31 (or fewer depending on the month). *)

fun is_older (date1 : int*int*int, date2 : int*int*int) =
  let
      val y1 = #1 date1;
      val m1 = #2 date1;
      val d1 = #3 date1;
      val y2 = #1 date2;
      val m2 = #2 date2;
      val d2 = #3 date2;
  in
      if y1 < y2
      then true
      else if y1 <= y2 andalso m1 < m2
      then true
      else if y1 <= y2 andalso m1 <= m2 andalso d1 < d2
      then true
      else false
  end

(* A helper function for counting appearances of a given month
   in a list of dates.  *)
fun count_month_matches (dlist : (int*int*int) list, matcher : int, matches : int) =
  if null dlist
  then matches
  else
      if #2(hd(dlist)) = matcher
      then count_month_matches(tl(dlist), matcher, (matches + 1))
      else count_month_matches(tl(dlist), matcher, matches)
      
fun number_in_month (dlist : (int*int*int) list, m : int) =
  count_month_matches(dlist, m, 0)

fun number_in_months (dlist : (int*int*int) list, months : int list) =
  if null months
  then 0
  else
      number_in_month(dlist, hd(months)) + number_in_months(dlist, tl(months))

fun dates_in_month (dlist : (int * int * int) list, month : int) =
    if null dlist
    then dlist
    else
	let
	    val date = hd(dlist)
	in
            if #2(date) = month
            then date :: dates_in_month(tl(dlist), month)
            else dates_in_month(tl(dlist), month)
	end

fun dates_in_months (dlist : (int * int * int) list, months : int list) =
  if null dlist orelse null months
  then []
  else
      dates_in_month(dlist, hd(months))@dates_in_months(dlist, tl(months))

fun get_nth (slist : string list, n : int) =
  if null slist
  then ""
  else
      if n > length(slist)
      then ""
      else if n = 1
      then hd(slist)
      else if n = length(slist)
      then hd(rev(slist))
      else if n < length(slist)
      then get_nth(tl(slist), n - 1)
      else hd(slist)

(* Simpler Solution
fun get_nth (lst : string list, n : int) =
    if n=1
    then hd lst
    else get_nth(tl lst, n-1)
*)

	     
fun date_to_string (date : int*int*int) =
  let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2(date)) ^ " " ^ Int.toString(#3(date)) ^ ", " ^ Int.toString(#1(date))
  end

fun number_before_reaching_sum (sum : int, nums : int list) =
  let 
      fun count_sum_steps(sum : int, nums : int list, count : int) =
	  if null nums
	  then 0
	  else
	      let
		  val first_two_items = hd(nums) + hd(tl(nums))					   
	      in
		  if first_two_items < sum
		  then count_sum_steps(sum, first_two_items::tl(tl(nums)), count + 1)
		  else count
	      end	
  in
      count_sum_steps(sum, nums, 1)
  end

fun what_month (n : int) =
  let
      val month_totals = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      if n <= 31
      then 1
      else number_before_reaching_sum(n, month_totals) + 1
  end

fun month_range (d1 : int, d2 : int) =
  if d1 > d2
  then []
  else what_month d1::month_range(d1 + 1, d2)

fun oldest (dlist : (int*int*int) list) =
  if null dlist
  then NONE
  else
      let
	  fun non_empty_oldest(dlist : (int*int*int) list) =
	    if null(tl(dlist))
	    then hd(dlist)
	    else
		let
		    val tl_ans = non_empty_oldest(tl(dlist))
		in
		    if is_older(hd(dlist),tl_ans)
		    then hd(dlist)
		    else tl_ans
		end
      in
	  SOME (non_empty_oldest(dlist))
      end
