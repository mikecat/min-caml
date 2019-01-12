let rec print_length l =
  let rec pi l c = match l with
      [] -> print_int c; print_newline ()
    | x :: xs -> pi xs (c + 1)
  in pi l 0
in let rec func m =
  let op =
    let rec f x = [x] in
    let rec g x = [x; x] in
    if m=0 then f else g
  in
  print_length (op 1);
  print_length (op 1.);
  print_length (op (op ()))
in 
func 0;
func 1
