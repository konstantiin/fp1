(*using for loop*)
let _ =
  for a = 1 to 334 do
    for b = a to 1000 - a do
      let c = 1000 - a - b in
      if (a * a) + (b * b) = c * c then
        a * b * c |> string_of_int |> print_endline
    done
  done
