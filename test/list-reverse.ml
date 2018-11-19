let rec print_list l =
  match l with
    [] -> print_newline ()
  | x :: xs -> print_int x; print_list xs
in let rec rev l =
  let rec revi l r =
    match l with
      [] -> r
    | x :: xs -> revi xs (x :: r)
  in revi l []
in let l = [3; 1; 4; 1; 5; 9; 2] in
print_list l;
print_list (rev l)
