open Core

module Direction = struct
  type t = Up | Right | Down | Left

  let of_string = function
    | "U" -> Some Up
    | "R" -> Some Right
    | "D" -> Some Down
    | "L" -> Some Left
    | _ -> None

  let _to_string = function
    | Up -> "Up"
    | Right -> "Right"
    | Down -> "Down"
    | Left -> "Left"
end

module Position = Tuple.Comparable (Int) (Int)

module Grid = struct
  module Data = struct
    type t = Head | Tail | Head_and_tail | Empty
  end

  type t = Data.t Position.Map.t

  let empty : t =
    Map.set Position.Map.empty ~key:(0, 0) ~data:Data.Head_and_tail

  let find t ~position = Map.find t position |> Option.value ~default:Data.Empty

  let head t =
    match
      Map.filter t ~f:(function
        | Data.Head | Data.Head_and_tail -> true
        | _ -> false)
      |> Map.keys
    with
    | [ head_pos ] -> head_pos
    | _ -> failwith "multiple heads"

  let tail t =
    match
      Map.filter t ~f:(function
        | Data.Tail | Data.Head_and_tail -> true
        | _ -> false)
      |> Map.keys
    with
    | [ tail_pos ] -> tail_pos
    | _ -> failwith "multiple tails"

  let set_head t ~position =
    let open Data in
    let new_data =
      match find t ~position with
      | Empty -> Head
      | Tail -> Head_and_tail
      | old_data -> old_data
    in
    Map.set t ~key:position ~data:new_data

  let unset_head t ~position =
    let open Data in
    let new_data =
      match find t ~position with
      | Head -> Empty
      | Head_and_tail -> Tail
      | old_data -> old_data
    in
    Map.set t ~key:position ~data:new_data

  let move_head t ~src ~dest =
    unset_head t ~position:src |> set_head ~position:dest

  let set_tail t ~position =
    let open Data in
    let new_data =
      match find t ~position with
      | Empty -> Tail
      | Head -> Head_and_tail
      | old_data -> old_data
    in
    Map.set t ~key:position ~data:new_data

  let unset_tail t ~position =
    let open Data in
    let new_data =
      match find t ~position with
      | Tail -> Empty
      | Head_and_tail -> Head
      | old_data -> old_data
    in
    Map.set t ~key:position ~data:new_data

  let move_tail t ~src ~dest =
    unset_tail t ~position:src |> set_tail ~position:dest

  let step (x, y) ~direction =
    let open Direction in
    match direction with
    | Up -> (x + 1, y)
    | Right -> (x, y + 1)
    | Down -> (x - 1, y)
    | Left -> (x, y - 1)

  let update_head t ~direction =
    let old_head = head t in
    let new_head = step old_head ~direction in
    move_head t ~src:old_head ~dest:new_head

  let update_tail t =
    let old_tail = tail t in
    let new_tail =
      let xh, yh = head t in
      let xt, yt = old_tail in
      match (xh - xt, yh - yt) with
      | -1, -1 | -1, 0 | -1, 1 | 0, -1 | 0, 0 | 0, 1 | 1, -1 | 1, 0 | 1, 1 ->
          (xt, yt) (* if adjacent just stay *)
      | 2, 0 -> step old_tail ~direction:Direction.Up
      | 0, 2 -> step old_tail ~direction:Direction.Right
      | -2, 0 -> step old_tail ~direction:Direction.Down
      | 0, -2 -> step old_tail ~direction:Direction.Left
      | 2, -2 | 2, -1 | 1, -2 ->
          step old_tail ~direction:Direction.Up
          |> step ~direction:Direction.Left
      | 2, 2 | 2, 1 | 1, 2 ->
          step old_tail ~direction:Direction.Up
          |> step ~direction:Direction.Right
      | -2, -2 | -2, -1 | -1, -2 ->
          step old_tail ~direction:Direction.Down
          |> step ~direction:Direction.Left
      | -2, 2 | -2, 1 | -1, 2 ->
          step old_tail ~direction:Direction.Down
          |> step ~direction:Direction.Right
      | _ -> failwith "head too far away from tail"
    in
    move_tail t ~src:old_tail ~dest:new_tail

  let move t ~direction =
    let t = update_head t ~direction in
    let t = update_tail t in
    (t, tail t)

  let move_pos t ~pos =
    let t = move_head t ~src:(head t) ~dest:pos in
    let t = update_tail t in
    (t, tail t)
end

let lines = In_channel.read_lines "day9/input.txt"

let line_to_direction_list line =
  match String.split_on_chars ~on:[ ' ' ] line with
  | [ direction; amount ] ->
      let direction = Direction.of_string direction in
      let amount = Int.of_string amount in
      List.init amount ~f:(fun _ -> direction) |> Option.all
  | _ -> None

module Rope = struct
  type t = {
    one : Grid.t;
    two : Grid.t;
    three : Grid.t;
    four : Grid.t;
    five : Grid.t;
    six : Grid.t;
    seven : Grid.t;
    eight : Grid.t;
    nine : Grid.t;
  }

  let empty =
    {
      one = Grid.empty;
      two = Grid.empty;
      three = Grid.empty;
      four = Grid.empty;
      five = Grid.empty;
      six = Grid.empty;
      seven = Grid.empty;
      eight = Grid.empty;
      nine = Grid.empty;
    }

  let move t ~direction =
    let one, tail_one = Grid.move t.one ~direction in
    let two, tail_two = Grid.move_pos t.two ~pos:tail_one in
    let three, tail_three = Grid.move_pos t.three ~pos:tail_two in
    let four, tail_four = Grid.move_pos t.four ~pos:tail_three in
    let five, tail_five = Grid.move_pos t.five ~pos:tail_four in
    let six, tail_six = Grid.move_pos t.six ~pos:tail_five in
    let seven, tail_seven = Grid.move_pos t.seven ~pos:tail_six in
    let eight, tail_eight = Grid.move_pos t.eight ~pos:tail_seven in
    let nine, tail_nine = Grid.move_pos t.nine ~pos:tail_eight in
    ({ one; two; three; four; five; six; seven; eight; nine }, tail_nine)
end

module V1 = struct
  let _main () =
    let moves =
      List.map lines ~f:line_to_direction_list
      |> List.filter_map ~f:Fn.id
      |> List.concat
    in
    let _, tails =
      List.fold moves
        ~init:(Grid.empty, [ Grid.tail Grid.empty ])
        ~f:(fun (grid, tails) direction ->
          let grid, tail = Grid.move grid ~direction in
          (grid, tail :: tails))
    in
    Position.Set.of_list tails |> Set.length |> Int.to_string |> print_endline
end

module V2 = struct
  let main () =
    let moves =
      List.map lines ~f:line_to_direction_list
      |> List.filter_map ~f:Fn.id
      |> List.concat
    in
    let _, tails =
      List.fold moves
        ~init:(Rope.empty, [ Grid.tail Grid.empty ])
        ~f:(fun (rope, tail_nines) direction ->
          let rope, tail_nine = Rope.move rope ~direction in
          (rope, tail_nine :: tail_nines))
    in
    Position.Set.of_list tails |> Set.length |> Int.to_string |> print_endline
end

module Current = V2

let () = Current.main ()
