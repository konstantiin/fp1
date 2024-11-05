let names =
  let file = "0022_names.txt" in
  let ifile = open_in file in
  ifile |> input_line
  |> Str.global_replace (Str.regexp "\"") ""
  |> Str.split (Str.regexp ",")

let sorted_names = List.sort String.compare names

(* tail recursive *)
let rec get_sum acc idx = function
  | [] -> acc
  | h :: t ->
      let get_val str =
        String.fold_left
          (fun acc c -> acc + (Char.code c - Char.code 'A'))
          0 str
      in
      get_sum (acc + (get_val h * idx)) (idx + 1) t

let _ = sorted_names |> get_sum 0 1 |> string_of_int |> print_endline

(* not tail recursive *)
let rec get_sum_noacc idx = function
  | [] -> 0
  | h :: t ->
      let get_val str =
        String.fold_left
          (fun acc c -> acc + (Char.code c - Char.code 'A'))
          0 str
      in
      (get_val h * idx) + get_sum_noacc (idx + 1) t

let _ = sorted_names |> get_sum_noacc 1 |> string_of_int |> print_endline

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
