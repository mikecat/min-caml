let rec reverse1 l =
  let rec reverse_i l r =
    match l with
      [] -> r
    | x :: xs -> reverse_i xs (x :: r)
  in reverse_i l []
in let rec reverse2 l =
  let rec reverse_i l r =
    match l with
      [] -> r
    | x :: xs -> reverse_i xs (x :: r)
  in reverse_i l []
in let rec transpose ll =
  let rec is_empty l = match l with [] -> true | _ :: _ -> false
  in let rec is_all_empty l =
    match l with
      [] -> true
    | x :: xs -> if is_empty x then is_all_empty xs else false
  in let rec get_column l col left =
    match l with
      [] -> (reverse1 col, reverse2 left)
    | x :: xs -> match x with
                   [] -> get_column xs col left
                 | y :: ys -> get_column xs (y :: col) (ys :: left)
  in let rec transpose_i l r =
    if is_all_empty l then reverse2 r
    else let (cur, next) = get_column l [] []
         in transpose_i next (cur :: r)
  in transpose_i ll []
in let rec print_list_list ll =
  let rec print_list l =
    match l with
      [] -> print_newline ()
    | x :: xs -> print_int x; print_list xs
  in match ll with
    [] -> ()
  | x :: xs -> print_list x; print_list_list xs
in let matrix = [
  [11; 12; 13; 14];
  [21; 22; 23; 24];
  [31; 32; 33; 34]
] in let transposed = transpose matrix in
print_list_list matrix;
print_newline ();
print_list_list transposed
