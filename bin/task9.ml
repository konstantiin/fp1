module Task9 = struct
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

  let solve_filter () = match ans with [] -> -1 | h :: [] -> h | _ :: _ -> -1

  (*using for loop*)
  let print_forloop () =
    for a = 1 to 334 do
      for b = a to 1000 - a do
        let c = 1000 - a - b in
        if (a * a) + (b * b) = c * c then
          a * b * c |> string_of_int |> print_endline
      done
    done

  (*example of not tail recursion, result in stack overflow*)
  let rec broot_force a b =
    let c = 1000 - a - b in
    let ans =
      if a + b >= 1000 then broot_force (a + 1) (a + 1)
      else broot_force a (b + 1)
    in
    if c * c = (a * a) + (b * b) then a * b * c else ans

  let solve_notl () = broot_force 1 1

  (*tail recursion realization*)
  let rec broot_force_tr a b =
    let c = 1000 - a - b in
    if c * c = (a * a) + (b * b) then a * b * c
    else if a >= 334 then -1
    else if a + b >= 1000 then broot_force_tr (a + 1) (a + 1)
    else broot_force_tr a (b + 1)

  let solve_tl () = broot_force_tr 1 1
end
