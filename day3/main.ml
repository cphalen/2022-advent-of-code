open Core

let read_input file = In_channel.read_lines file

module Rucksack = struct
  type t = { left : Int.Set.t; right : Int.Set.t }

  let char_to_int c =
    match (Char.is_lowercase c, Char.is_uppercase c) with
    | true, false -> Some (Char.to_int c - Char.to_int 'a' + 1)
    | false, true -> Some (Char.to_int c - Char.to_int 'A' + 27)
    | _ -> None

  let of_string s =
    let int_set s =
      String.to_list s |> List.filter_map ~f:char_to_int |> Int.Set.of_list
    in
    let len = String.length s in
    let left = String.sub ~pos:0 ~len:(len / 2) s |> int_set in
    let right = String.sub ~pos:(len / 2) ~len:(len / 2) s |> int_set in
    { left; right }
end

let split ~every ls =
  List.foldi ls ~init:([], []) ~f:(fun i (curr, grouped) x ->
      if i % every = 0 then ([ x ], curr :: grouped) else (x :: curr, grouped))
  |> fun (last, rest) -> last :: rest

module V1 = struct
  let _main () =
    read_input "day3/input.txt"
    |> List.map ~f:Rucksack.of_string
    |> List.map ~f:(fun rucksack ->
           Rucksack.(Set.inter rucksack.left rucksack.right))
    |> List.map ~f:(Set.fold ~f:( + ) ~init:0)
    |> List.fold ~f:( + ) ~init:0
    |> Int.to_string
    |> print_endline
end

module V2 = struct
  let main () =
    let groups = read_input "day3/input.txt" |> split ~every:3 in
    let find_badge group =
      let sets =
        List.map group ~f:Rucksack.of_string
        |> List.map ~f:(fun rucksack ->
               Rucksack.(Set.union rucksack.left rucksack.right))
      in
      match sets with
      | [] -> None
      | hd :: tl -> (
          let inter = List.fold ~init:hd ~f:Set.inter tl in
          match Set.to_list inter with [ badge ] -> Some badge | _ -> None)
    in
    let badges = List.map ~f:find_badge groups |> List.filter_map ~f:Fn.id in
    List.fold badges ~f:( + ) ~init:0 |> Int.to_string |> print_endline
end

module Current = V2

let () = Current.main ()
