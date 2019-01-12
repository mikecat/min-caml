let rec f x = [x] in
let rec g x = [x; x] in
let rec func m x =
  let op = if m=0 then f else g in op x
in let rec print_length l =
  let rec pi l c = match l with
      [] -> print_int c; print_newline ()
    | x :: xs -> pi xs (c + 1)
  in pi l 0
in
print_length (func 0 1);
print_length (func 1 1);
print_length (func 0 1.);
print_length (func 1 1.);
print_length (func 0 (func 1 ()))
