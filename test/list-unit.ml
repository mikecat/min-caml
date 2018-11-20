let rec length l =
  let rec count l c =
    match l with
      [] -> c
    | _ :: xs -> count xs (c + 1)
  in count l 0
in let rec create_unit n =
  let rec cu_i n r =
    if n <= 0 then r
              else cu_i (n - 1) (() :: r)
  in cu_i n []
in let rec print_int_ln n =
  print_int n; print_newline ()
in
print_int_ln (length []);
print_int_ln (length [()]);
print_int_ln (length [(); (); (); (); ()]);
print_int_ln (length (create_unit 1000))
