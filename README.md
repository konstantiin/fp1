# Лабораторная работа №1. Лучинкин Константин
## Задача №9
<p>A Pythagorean triplet is a set of three natural numbers, $x`a \lt b \lt c$, for which,
$$a^2 + b^2 = c^2.$$</p>
<p>For example, $3^2 + 4^2 = 9 + 16 = 25 = 5^2$.</p>
<p>There exists exactly one Pythagorean triplet for which $a + b + c = 1000$.<br>Find the product $abc$.</p>

Код:    
```ocaml
(*tail recursion realization*)
let rec broot_force_tr a b =
  let c = 1000 - a - b in
  if c * c = (a * a) + (b * b) then a * b * c
  else if a >= 334 then -1
  else if a + b >= 1000 then broot_force_tr (a + 1) (a + 1)
  else broot_force_tr a (b + 1)

let _ = broot_force_tr 1 1 |> string_of_int |> print_endline

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

(*using for loop*)
let _ =
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
    if a + b >= 1000 then broot_force (a + 1) (a + 1) else broot_force a (b + 1)
  in
  if c * c = (a * a) + (b * b) then a * b * c else ans

let _ = broot_force 1 1 |> string_of_int |> print_endline
```
Результат:
```powershell
~/fp/lab1$ dune build
~/fp/lab1$ dune exec bin/task9.exe
31875000
31875000
31875000
Fatal error: exception Stack_overflow
```
## Задача №22
<p>Using <a href="resources/documents/0022_names.txt">names.txt</a> (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.</p>
<p>For example, when the list is sorted into alphabetical order, COLIN, which is worth $3 + 15 + 12 + 9 + 14 = 53$, is the $938$th name in the list. So, COLIN would obtain a score of $938 \times 53 = 49714$.</p>
<p>What is the total of all the name scores in the file?</p>

Код:    
```ocaml
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

```
Результат:
```powershell
~/fp/lab1$ dune build
~/fp/lab1$ dune exec bin/task22.exe
792134121
792134121
792134121
```
