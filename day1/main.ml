open Core

let read_input file = In_channel.read_lines file
let sum = List.fold ~f:( + ) ~init:0

let sums =
  let last, rest =
    read_input "day1/input.txt"
    |> List.fold ~init:([], []) ~f:(fun (curr, grouped) x ->
           if String.length x = 0 then ([], curr :: grouped)
           else (x :: curr, grouped))
  in
  last :: rest |> List.map ~f:(List.map ~f:Int.of_string) |> List.map ~f:sum

module V1 = struct
  let _main () =
    let max = List.max_elt sums ~compare:( - ) in
    Option.iter ~f:(fun x -> Int.to_string x |> print_endline) max
end

module V2 = struct
  let main () =
    let top_3 = List.take (List.sort ~compare:(fun x y -> y - x) sums) 3 in
    sum top_3 |> Int.to_string |> print_endline
end

module Current = V2

let () = Current.main ()
