let names =
  let file = "0022_names.txt" in
  let ifile = open_in file in
  ifile |> input_line
  |> Str.global_replace (Str.regexp "\"") ""
  |> Str.split (Str.regexp ",")

let sorted_names = List.sort String.compare names

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
