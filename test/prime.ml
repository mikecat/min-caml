let rec print_list list =
  match list with
    [] -> ()
  | x :: xs -> print_int x ; print_newline (); print_list xs
in let rec exclude start diff list =
  let rec reverse list result =
    match list with
      [] -> result
    | x :: xs -> reverse xs (x :: result)
  in let rec exclude_i current list result =
    match list with
      [] -> reverse result []
    | x :: xs -> if x = current then exclude_i (current + diff) xs result
                 else if x < current then exclude_i current xs (x :: result)
                 else exclude_i (current + diff) list result
  in exclude_i start list []
in let rec range first last =
  let rec range_i current result =
    if current <= first then current :: result
                        else range_i (current - 1) (current :: result)
  in range_i last []
in let rec prime candidates =
  match candidates with
    [] -> []
  | x :: xs -> x :: (prime (exclude x x xs))
in print_list (prime (range 2 1000))
