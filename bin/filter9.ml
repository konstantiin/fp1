(* generate list of possible a and b. Then filter it to get result *)
let possible_ab =
  List.init 334 (fun i ->
      let a = i + 1 in
      let max_b = ((1000 - a) / 2) + 1 in
      List.init (max_b - a) (fun offset -> (a, a + offset)))

let ans =
  possible_ab |> List.flatten
  |> List.filter_map (fun (a, b) ->
         let c = 1000 - a - b in
         if (a * a) + (b * b) = c * c then Some (a * b * c) else None)

let _ =
  (match ans with
  | [] -> "Error"
  | h :: [] -> string_of_int h
  | _ :: _ -> "Error")
  |> print_endline
