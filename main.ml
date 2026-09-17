(* Actual Main File*)

(*Helper Func*)
let month (x,_,_) = x (* gets the first element of a triple *)
let day (_,x,_) = x (* gets the second element of a triple *)
let year (_,_,x) = x (* gets the third element of a triple *)

(*
Problem 1
Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.)
*)
let is_older (d1: int * int * int ) (d2: int * int * int) = 
    if year d1 < year d2 then true
    else if year d1 = year d2 && month d1 < month d2 then true
    else if year d1 = year d2 && month d1 = month d2 && day d1 < day d2 then true
    else false

(*
Problem 2
Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month.
*)
let number_in_month (dates : (int * int * int) list) (month: int) = 0

(*
Problem 3
Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem.
*)
let number_in_months (dates : (int * int * int) list) (months: (int) list) = 0

(*
Problem 4
Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given.
*)
let dates_in_month (dates : (int * int * int) list) (month: int) = []

(*
Problem 5
Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and OCaml’s list-append operator (@).
*)
let dates_in_month (dates : (int * int * int) list) (months: (int) list) = []

(*
Problem 6
Write a function get_nth that takes a list of strings and a positive int n and returns the nth element
of the list where the head of the list is 1st. Do not worry about the case where the list has too few
elements: your function may apply List.hd or List.tl to the empty list in this case, which is okay.
*)

(*
Problem 7
Write a function string_of_date that takes a date and returns a string of the form September-10-2015
(for example). Use the operator ^ for concatenating strings and the library function string_of_int
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, use
hyphens exactly as in the example and use English month names: January, February, March, April,
May, June, July, August, September, October, November, December.
*)

(*
Problem 8
Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case.
*)

(*
Problem 9
Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem.
*)

(*
Problem 10
Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1;m2;...;mn] where m1 is the month of day1, m2 is the month of day1+1, . . . , and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1 > day2.
*)

(*
Problem 11
Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to None if the list has no dates else Some d where the date d is the oldest date in the list.
*)

(*
Problem 12
Write a function cumulative_sum that takes a list of numbers and returns a list of the partial sums
of these numbers. For example, cumulative_sum [12;27;13] = [12;39;52]. Hint: Use a helper
function that takes two arguments.
*)

(*
Problem 13 (Challenge)
Write functions number_in_months_challenge and dates_in_months_challenge
that are like your solutions to problems 3 and 5 except having a month in the second argument multiple
times has no more effect than having it once. (Hint: Remove duplicates, then use previous work.)
*)

(*
Problem 14 (Challenge)
Write a function reasonable_date that takes a date and determines if it
describes a real date in the common era. A “real date” has a positive year (year 0 did not exist), a
month between 1 and 12, and a day appropriate for the month. Solutions should properly handle leap
years. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100.
(Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.)
*)
