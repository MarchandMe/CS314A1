open Hw1

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