let rec print_int_list l =
  match l with
    [] -> print_newline ()
  | x :: xs -> print_int x; print_int_list xs
in let rec print_float_list l =
  match l with
    [] -> print_newline ()
  | x :: xs -> print_int (truncate x); print_float_list xs
in let rec map f l =
  let rec rev l r =
    match l with
      [] -> r
    | x :: xs -> rev xs (x :: r)
  in let rec mapi l r =
    match l with
      [] -> rev r []
    | x :: xs -> mapi xs ((f x) :: r)
  in mapi l []
in
let li = [3; 1; 4; 1; 5; 9; 2] in
let lf = [3.; 1.; 4.; 1.; 5.; 9.; 2.] in
let rec inc x = x + 1 in
let rec incf x = x +. 1. in
let rec dub x = x + x in
let rec dubf x = x +. x in
print_int_list (map inc li);
print_int_list (map dub li);
print_float_list (map incf lf);
print_float_list (map dubf lf)
