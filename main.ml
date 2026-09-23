(* Actual Main File*)

(*Notes and Thoughts on OCAML as a language*)
(*
I have no idea if anyone will read this lol

Perhaps it is my code editor's syntax highlighting, but I find ocaml not altogether conducive to writing code step by step;
it requires me to plan out my code before I write it, which is both a boon and very annoying.

I never thought one could hate parantheses so! It cramps my hands.

ocaml does force readable code though, in a strange way. I'm not quite sure I like it.
*)


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
let is_older (d1: int * int * int ) (d2: int * int * int) : bool = 
    if year d1 < year d2 then true
    else if year d1 = year d2 && month d1 < month d2 then true
    else if year d1 = year d2 && month d1 = month d2 && day d1 < day d2 then true
    else false

(*
Problem 2
Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month.
*)
let rec number_in_month (dates : (int * int * int) list) (target: int) : int = 
    if dates = [] then 0
    else if month (List.hd dates) = target then 1 + number_in_month (List.tl dates) target
    else number_in_month (List.tl dates) target

(*
Problem 3
Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem.
*)
let rec number_in_months (dates : (int * int * int) list) (targets: (int) list) : int = 
    if targets = [] then 0
    else number_in_month dates (List.hd targets) + number_in_months dates (List.tl targets)

(*
Problem 4
Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given.
*)
let rec dates_in_month (dates : (int * int * int) list) (target: int) : (int * int * int) list = 
    if dates = [] then []
    else if month (List.hd dates) = target then (List.hd dates)::(dates_in_month (List.tl dates) target)
    else (dates_in_month (List.tl dates) target)

(*
Problem 5
Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and OCaml’s list-append operator (@).
*)
let rec dates_in_months (dates : (int * int * int) list) (targets: (int) list) : (int * int * int) list = 
    if targets = [] then []
    else (dates_in_month dates (List.hd targets))@(dates_in_months dates (List.tl targets))

(*
Problem 6
Write a function get_nth that takes a list of strings and a positive int n and returns the nth element
of the list where the head of the list is 1st. Do not worry about the case where the list has too few
elements: your function may apply List.hd or List.tl to the empty list in this case, which is okay.
*)
let rec get_nth (strings : string list) (index : int) : string = 
    if index = 1 then (List.hd strings)
    else get_nth (List.tl strings) (index - 1)

(*
Problem 7
Write a function string_of_date that takes a date and returns a string of the form September-10-2015
(for example). Use the operator ^ for concatenating strings and the library function string_of_int
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, use
hyphens exactly as in the example and use English month names: January, February, March, April,
May, June, July, August, September, October, November, December.
*)
(*months list*)
let month_names = ["January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"]
let string_of_date (date : int * int * int) : string = 
    (get_nth month_names (month date)) ^ "-" ^ (string_of_int (day date)) ^ "-" ^ (string_of_int (year date))

(*
Problem 8
Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case.
*)
let rec number_before_reaching_sum_helper (sum : int) (nums : int list) (index : int) : int =
    if sum <= (List.hd nums) then index
    else number_before_reaching_sum_helper (sum - (List.hd nums)) (List.tl nums) (index + 1)

let number_before_reaching_sum (sum : int) (nums : int list) : int= 
    (number_before_reaching_sum_helper sum nums 0)

(*
Problem 9
Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem.
*)
let days_in_month = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31]
let what_month (day : int) : int = 
    1 + (number_before_reaching_sum day days_in_month)

(*
Problem 10
Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1;m2;...;mn] where m1 is the month of day1, m2 is the month of day1+1, . . . , and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1 > day2.
*)
let rec month_range (day1 : int) (day2 : int) : (int list) = 
    if day1 > day2 then []
    else (what_month day1)::(month_range (day1 + 1) day2)

(*
Problem 11
Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to None if the list has no dates else Some d where the date d is the oldest date in the list.
*)
let rec oldest (dates : (int * int * int) list) : (int * int * int) option = 
    if dates = [] then None
    else if (Option.is_none (oldest (List.tl dates))) then (Some (List.hd dates))
    else if (is_older (List.hd dates) (Option.get (oldest (List.tl dates)))) then (Some (List.hd dates))
    else (oldest (List.tl dates))

(*
Problem 12
Write a function cumulative_sum that takes a list of numbers and returns a list of the partial sums
of these numbers. For example, cumulative_sum [12;27;13] = [12;39;52]. Hint: Use a helper
function that takes two arguments.
*)
let rec cumulative_sum_helper (nums : int list) (total : int) : int list = 
    if (nums = []) then []
    else total + (List.hd nums)::(cumulative_sum_helper (List.tl nums) (total + (List.hd nums)))

let cumulative_sum (nums : int list) : int list = 
    (cumulative_sum_helper nums 0)

(*
Problem 13 (Challenge)
Write functions number_in_months_challenge and dates_in_months_challenge
that are like your solutions to problems 3 and 5 except having a month in the second argument multiple
times has no more effect than having it once. (Hint: Remove duplicates, then use previous work.)
*)
let number_in_months_challenge (dates : (int * int * int) list) (targets: (int) list) = 0

let rec dates_in_months_challenge (dates : (int * int * int) list) (targets: (int) list) = 0


(*
Problem 14 (Challenge)
Write a function reasonable_date that takes a date and determines if it
describes a real date in the common era. A “real date” has a positive year (year 0 did not exist), a
month between 1 and 12, and a day appropriate for the month. Solutions should properly handle leap
years. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100.
(Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.)
*)
let helper_is_leap_year (date : int * int * int) : bool =
    if year(date) mod 400 = 0 then true
    else if year(date) mod 100 != 0 && year(date) mod 4 = 0 then true
    else false

let reasonable_date (date : int * int * int) : bool = false

(*Testing Func*)
let rec print_dates_list (dates: (int * int * int) list) = 
    if dates = [] then Printf.printf "\n"
    else (Printf.printf "(%d, %d, %d), " (month (List.hd dates)) (day (List.hd dates)) (year (List.hd dates)) ; print_dates_list(List.tl dates))

let rec print_int_list (nums : int list) =
    if nums = [] then Printf.printf "\n"
    else (Printf.printf "%d, " (List.hd nums) ; print_int_list (List.tl nums)) 

let test_dates = [
    (1, 5, 2024); (2, 10, 2023); (1, 20, 2025);
    (12, 31, 2023); (2, 29, 2024); (6, 15, 2022);
    (1, 1, 2024); (9, 22, 2026); (2, 28, 2025)
]
let test_months = [1;2]

let () =
    Printf.printf "is_older: %b \n" (is_older (12, 31, 2023) (1, 1, 2024));

    Printf.printf "number_in_month: %d \n" (number_in_month test_dates 1);

    Printf.printf "number_in_months: %d \n" (number_in_months test_dates [1; 2]);

    print_dates_list (dates_in_month test_dates 1);

    print_dates_list (dates_in_months test_dates test_months);

    Printf.printf "%s \n" (get_nth month_names 3);

    Printf.printf "%s \n" (string_of_date (12, 31, 2025));

    Printf.printf "%d \n" (number_before_reaching_sum 100 days_in_month);

    Printf.printf "%d \n" (what_month 100);

    print_int_list (month_range 5 10);

    Printf.printf "%s \n" (string_of_date (Option.get (oldest test_dates)));

    print_int_list (cumulative_sum days_in_month);