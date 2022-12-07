open Core

let read_input file = In_channel.read_lines file

module Move = struct
  module V1 = struct
    type t = Rock | Paper | Scissors

    let of_string = function
      | "A" | "X" -> Some Rock
      | "B" | "Y" -> Some Paper
      | "C" | "Z" -> Some Scissors
      | _ -> None
  end

  module V2 = struct
    module Play = struct
      type t = Rock | Paper | Scissors

      let of_string = function
        | "A" -> Some Rock
        | "B" -> Some Paper
        | "C" -> Some Scissors
        | _ -> None
    end

    module Result = struct
      type t = Win | Loss | Draw

      let of_string = function
        | "X" -> Some Loss
        | "Y" -> Some Draw
        | "Z" -> Some Win
        | _ -> None
    end
  end
end

module Game = struct
  module V1 = struct
    module Move = Move.V1

    type t = { other : Move.t; self : Move.t }

    let _of_input s =
      match String.split_on_chars ~on:[ ' ' ] s with
      | [ other; self ] ->
          Option.bind (Move.of_string other) ~f:(fun other ->
              Option.map (Move.of_string self) ~f:(fun self -> { other; self }))
      | _ -> None

    let _points g =
      let move_points =
        match g.self with Rock -> 1 | Paper -> 2 | Scissors -> 3
      in
      let play_points =
        match (g.self, g.other) with
        (* losing games *)
        | Rock, Paper | Paper, Scissors | Scissors, Rock -> 0
        (* tied games *)
        | Rock, Rock | Paper, Paper | Scissors, Scissors -> 3
        (* winning games *)
        | Rock, Scissors | Paper, Rock | Scissors, Paper -> 6
      in
      move_points + play_points
  end

  module V2 = struct
    module Move = Move.V2

    type t = { other : Move.Play.t; self : Move.Result.t }

    let of_input s =
      match String.split_on_chars ~on:[ ' ' ] s with
      | [ other; self ] ->
          Option.bind (Move.Play.of_string other) ~f:(fun other ->
              Option.map (Move.Result.of_string self) ~f:(fun self ->
                  { other; self }))
      | _ -> None

    let will_play play result =
      let open Move.Play in
      let open Move.Result in
      match (play, result) with
      | Rock, Draw | Paper, Loss | Scissors, Win -> Rock
      | Rock, Win | Paper, Draw | Scissors, Loss -> Paper
      | Rock, Loss | Paper, Win | Scissors, Draw -> Scissors

    let points g =
      let play = will_play g.other g.self in
      let move_points =
        match play with Rock -> 1 | Paper -> 2 | Scissors -> 3
      in
      let play_points =
        let open Move.Play in
        match (play, g.other) with
        (* losing games *)
        | Rock, Paper | Paper, Scissors | Scissors, Rock -> 0
        (* tied games *)
        | Rock, Rock | Paper, Paper | Scissors, Scissors -> 3
        (* winning games *)
        | Rock, Scissors | Paper, Rock | Scissors, Paper -> 6
      in
      move_points + play_points
  end

  module Current = V2
  include Current
end

let sum = List.fold ~f:( + ) ~init:0

let points =
  read_input "day2/input.txt"
  |> List.map ~f:Game.of_input
  |> List.filter_map ~f:Fn.id
  |> List.map ~f:Game.points
  |> sum

let () = print_endline (Int.to_string points)
