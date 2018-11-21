let rec length list =
  let rec length_i list count =
    match list with
      [] -> count
    | _ :: xs -> length_i xs (count + 1)
  in length_i list 0
in
print_int (length [1; 2; 3; 4; 5]);
print_newline ();
print_int (length [1.; 2.; 3.; 4.; 5.; 6.; 7.]);
print_newline ();
print_int (length [(); (); ()]);
print_newline ();
print_int (length []);
print_newline ()
