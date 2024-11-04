let rec broot_force_tr a b =
  let c = 1000 - a - b in
  if c * c = (a * a) + (b * b) then a * b * c
  else if a >= 334 then -1
  else if a + b >= 1000 then broot_force_tr (a + 1) (a + 1)
  else broot_force_tr a (b + 1)

let _ = broot_force_tr 1 1 |> string_of_int |> print_endline

(*example of not tail recursion, result in stack overflow*)
(* let rec broot_force a b =
     let c = 1000 - a - b in
     let ans =
       if a + b >= 1000 then broot_force (a + 1) (a + 1) else broot_force a (b + 1)
     in
     if c * c = (a * a) + (b * b) then a * b * c else ans

   let _ = broot_force 1 1 |> string_of_int |> print_endline *)

(* 2b = (1000 - a)/2 *)
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
