open Core

let read_input file = In_channel.read_lines file

module Range = struct
  type t = { low : int; high : int }

  let create ~low ~high () = { low; high }
  let contains self other = self.low <= other.low && other.high <= self.high
  let overlap self other = self.high < other.low || other.high < self.low
end

let parse_range range =
  match String.split_on_chars ~on:[ '-' ] range with
  | [ start_index; end_index ] ->
      let start_index = Int.of_string start_index in
      let end_index = Int.of_string end_index in
      Some (Range.create ~low:start_index ~high:end_index ())
  | _ -> None

let parse_line line =
  match String.split_on_chars ~on:[ ',' ] line with
  | [ left; right ] ->
      Option.bind (parse_range left) ~f:(fun left ->
          Option.map (parse_range right) ~f:(fun right -> (left, right)))
  | _ -> None

let pairwise_contains (left, right) =
  Range.contains left right || Range.contains right left

module V1 = struct
  let _main () =
    read_input "day4/input.txt"
    |> List.map ~f:parse_line
    |> List.filter_map ~f:Fn.id
    |> List.filter ~f:pairwise_contains
    |> List.length
    |> Int.to_string
    |> print_endline
end

module V2 = struct
  let main () =
    read_input "day4/input.txt"
    |> List.map ~f:parse_line
    |> List.filter_map ~f:Fn.id
    |> List.filter ~f:(fun (left, right) -> Range.overlap left right |> not)
    |> List.length
    |> Int.to_string
    |> print_endline
end

module Current = V2

let () = Current.main ()
