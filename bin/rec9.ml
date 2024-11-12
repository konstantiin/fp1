(*example of not tail recursion, result in stack overflow*)
let rec broot_force a b =
  let c = 1000 - a - b in
  let ans =
    if a + b >= 1000 then broot_force (a + 1) (a + 1) else broot_force a (b + 1)
  in
  if c * c = (a * a) + (b * b) then a * b * c else ans

let _ = broot_force 1 1 |> string_of_int |> print_endline
