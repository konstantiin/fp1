let names =
  let file = "0022_names.txt" in
  let ifile = open_in file in
  ifile |> input_line
  |> Str.global_replace (Str.regexp "\"") ""
  |> Str.split (Str.regexp ",")

let sorted_names = List.sort String.compare names

(* use of map *)
let _ =
  sorted_names
  |> List.mapi (fun idx str ->
         (idx + 1)
         * String.fold_left
             (fun acc c -> acc + (Char.code c - Char.code 'A'))
             0 str)
  |> List.fold_left (fun acc x -> acc + x) 0
  |> string_of_int |> print_endline
