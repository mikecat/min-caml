let rec print_float_list l =
  match l with
    [] -> ()
  | x :: xs -> print_int (truncate x);
               print_newline ();
               print_float_list xs
in print_float_list [1.; 2.; 3.; 4.; 5.]
