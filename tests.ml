open Hw1

(*Testing Func*)
let rec print_dates_list (dates: (int * int * int) list) = 
    if dates = [] then Printf.printf "\n"
    else (Printf.printf "(%d, %d, %d), " (day (List.hd dates)) (month (List.hd dates)) (year (List.hd dates)) ; print_dates_list(List.tl dates))

let rec print_int_list (nums : int list) =
    if nums = [] then Printf.printf "\n"
    else (Printf.printf "%d, " (List.hd nums) ; print_int_list (List.tl nums)) 

(* Dates use (day, month, year). *)
let test_dates = [
    (5, 1, 2024); (10, 2, 2023); (20, 1, 2025);
    (31, 12, 2023); (29, 2, 2024); (15, 6, 2022);
    (1, 1, 2024); (22, 9, 2026); (28, 2, 2025)
]
let test_months = [1;2]

let test_months_multiple = [1;1]

let () =
    Printf.printf "%b \n" (is_older ((31, 12, 2023), (1, 1, 2024)));

    Printf.printf "%d \n" (number_in_month (test_dates, 1));

    Printf.printf "%d \n" (number_in_months (test_dates, test_months));

    print_dates_list (dates_in_month (test_dates, 1));

    print_dates_list (dates_in_months (test_dates, test_months));

    Printf.printf "%s \n" (get_nth (month_names, 3));

    Printf.printf "%s \n" (string_of_date (31, 12, 2025));

    Printf.printf "%d \n" (number_before_reaching_sum (100, days_in_month));

    Printf.printf "%d \n" (what_month 100);

    print_int_list (month_range (5, 10));

    Printf.printf "%s \n" (string_of_date (Option.get (oldest test_dates)));

    print_int_list (cumulative_sum days_in_month);

    Printf.printf "%d \n" (number_in_months_challenge (test_dates, test_months_multiple));

    print_dates_list (dates_in_months_challenge (test_dates, test_months_multiple));

    Printf.printf "%b \n" (reasonable_date (1, 22, 2023));

    Printf.printf "%b \n" (reasonable_date (1, 1, 2021));
