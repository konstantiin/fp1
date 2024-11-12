(*tail recursion realization*)
let rec broot_force_tr a b =
  let c = 1000 - a - b in
  if c * c = (a * a) + (b * b) then a * b * c
  else if a >= 334 then -1
  else if a + b >= 1000 then broot_force_tr (a + 1) (a + 1)
  else broot_force_tr a (b + 1)

let _ = broot_force_tr 1 1 |> string_of_int |> print_endline
