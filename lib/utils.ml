let group_by (data: 'a list) (p: 'a -> 'a -> bool) =
  let open List in
  let rec part acc = function
    | [] -> acc
    | x :: rest ->
      let (yes, no) = partition (fun y -> p y x) rest in
      part ((x :: yes) :: acc) no
  in
  let result = part [] data in
  let cmp x y = if length x > length y then -1 else 1 in
  stable_sort cmp result

let group_by_if_sorted (data: 'a list) (predicate: 'a -> 'a -> bool) =
  let group acc curr =
    match acc with
    | [] -> [[curr]]
    | (group :: rest) when predicate curr (List.hd group) ->
      (curr :: group) :: rest
    | _ -> [curr] :: acc
  in
  List.fold_left group [] data

let all (data: 'a list) (p: 'a -> bool) =
  let rec f l =
    match l with
    | [] -> failwith "Empty list"
    | [x] -> p x
    | x :: rest -> if p x then f rest else false
  in
  f data
