open Core
module Position = Tuple.Comparable (Int) (Int)

module Direction = struct
  type t = Up | Right | Down | Left
end

let lines = In_channel.read_lines "day8/input.txt"
let rows = 99
let columns = 99
let check_inbounds (r, c) = 0 <= r && r < rows && 0 <= c && c < columns

let adjacent ~direction (r, c) =
  let pos =
    match direction with
    | Direction.Up -> (r - 1, c)
    | Direction.Right -> (r, c + 1)
    | Direction.Down -> (r + 1, c)
    | Direction.Left -> (r, c - 1)
  in
  match check_inbounds pos with false -> None | true -> Some pos

let taller ~heights self other =
  match (check_inbounds self, check_inbounds other) with
  | false, _ | _, false -> failwith "tree does not exist (invalid coordinates)"
  | true, true -> Map.find_exn heights self > Map.find_exn heights other

let visible_in_direction ~heights ~direction position =
  let rec tallest current =
    let next = adjacent ~direction current in
    match next with
    | None -> true
    | Some next -> taller ~heights position next && tallest next
  in
  tallest position

let is_visible ~heights position =
  List.map
    ~f:(fun direction -> visible_in_direction ~heights ~direction position)
    Direction.[ Up; Right; Down; Left ]
  |> List.fold ~init:false ~f:( || )

let viewing_distance ~heights ~direction position =
  let rec distance current i =
    let next = adjacent ~direction current in
    match next with
    | None -> i
    | Some next -> (
        match taller ~heights position next with
        | false -> i + 1
        | true -> distance next (i + 1))
  in
  distance position 0

let scenic_score ~heights position =
  List.map
    ~f:(fun direction -> viewing_distance ~heights ~direction position)
    Direction.[ Up; Right; Down; Left ]
  |> List.fold ~init:1 ~f:( * )

let heights =
  List.foldi lines ~init:Position.Map.empty ~f:(fun r map line ->
      String.foldi line ~init:map ~f:(fun c map char ->
          Map.set map ~key:(r, c) ~data:(Char.to_string char |> Int.of_string)))

module V1 = struct
  let _main () =
    let visible =
      Map.mapi heights ~f:(fun ~key ~data:_ -> is_visible ~heights key)
    in
    Map.filter visible ~f:Fn.id |> Map.length |> Int.to_string |> print_endline
end

module V2 = struct
  let main () =
    let scenic_scores =
      Map.mapi heights ~f:(fun ~key ~data:_ -> scenic_score ~heights key)
    in
    Map.data scenic_scores
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
    |> Int.to_string
    |> print_endline
end

module Current = V2

let () = Current.main ()
