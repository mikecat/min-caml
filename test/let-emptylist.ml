let rec print_length l =
  let rec pi l c = match l with
      [] -> print_int c; print_newline ()
    | x :: xs -> pi xs (c + 1)
  in pi l 0
in
let el = [] in
print_length (1 :: el);
print_length (el :: el :: el);
print_length (1. :: el)

