open Core

let lines = In_channel.read_lines "day15/input.txt"
let row = 2000000
let lower_bound = 0
let upper_bound = 4000000

module Range : sig
  type t

  val empty : t
  val add : t -> int * int -> t
  val endpoints : t -> (int * int) list
  val size : t -> int
  val splice : t -> on:int -> t
end = struct
  type t = (int * int) list

  let empty = []

  let combine (start1, stop1) (start2, stop2) =
    match (stop1 < start2, stop2 < start1) with
    | true, true -> Some (start1, stop1) (* the ranges are identical *)
    | true, false | false, true -> None (* the ranges are disjoint *)
    | false, false ->
        let start = min start1 start2 in
        let stop = max stop1 stop2 in
        Some (start, stop)

  let rec add t (start1, stop1) =
    match t with
    | [] -> [ (start1, stop1) ]
    | (start2, stop2) :: rest -> (
        match combine (start1, stop1) (start2, stop2) with
        | None -> (
            match start1 < start2 with
            | true -> (start1, stop1) :: (start2, stop2) :: rest
            | false -> (start2, stop2) :: add rest (start1, stop1))
        | Some range -> add rest range)

  let endpoints t = t

  let size t =
    List.map t ~f:(fun (start, stop) -> stop - start + 1)
    |> List.sum (module Int) ~f:Fn.id

  let rec splice t ~on =
    match t with
    | [] -> []
    | (start, stop) :: rest -> (
        match start <= on && on <= stop with
        | false -> (start, stop) :: splice rest ~on
        | true -> (
            match (on = start, on = stop) with
            | true, true -> rest
            | true, false -> (start + 1, stop) :: rest
            | false, true -> (start, stop - 1) :: rest
            | false, false -> (start, on - 1) :: (on + 1, stop) :: rest))
end

module Coord : sig
  type t

  include Comparable.S with type t := t

  val create : x:int -> y:int -> t
  val x : t -> int
  val y : t -> int
  val distance : t -> t -> int
  val distance_to_row : t -> row:int -> int
end = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare, equal]
  end

  include T
  include Comparable.Make (T)

  let create ~x ~y = (x, y)
  let x = fst
  let y = snd
  let distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
  let distance_to_row (_, y) ~row = abs (y - row)
end

module Sensor : sig
  type t

  val beacon : t -> Coord.t
  val of_string : string -> t option
  val on_row_within_radius : t -> row:int -> (int * int) option
end = struct
  type t = { sensor : Coord.t; beacon : Coord.t } [@@deriving fields]

  let re =
    Re2.create_exn
      "Sensor at x=([-0-9]+), y=([-0-9]+): closest beacon is at x=([-0-9]+), \
       y=([-0-9]+)"

  let of_string s =
    match Re2.get_matches_exn re s with
    | [ m ] ->
        let open Option.Let_syntax in
        let%bind sensor_x = Re2.Match.get ~sub:(`Index 1) m >>| Int.of_string in
        let%bind sensor_y = Re2.Match.get ~sub:(`Index 2) m >>| Int.of_string in
        let%bind beacon_x = Re2.Match.get ~sub:(`Index 3) m >>| Int.of_string in
        let%bind beacon_y = Re2.Match.get ~sub:(`Index 4) m >>| Int.of_string in
        let sensor = Coord.create ~x:sensor_x ~y:sensor_y in
        let beacon = Coord.create ~x:beacon_x ~y:beacon_y in
        Some { sensor; beacon }
    | _ -> None

  let _to_string t =
    let sensor_x = Coord.x t.sensor in
    let sensor_y = Coord.y t.sensor in
    let beacon_x = Coord.x t.beacon in
    let beacon_y = Coord.y t.beacon in
    Printf.sprintf "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
      sensor_x sensor_y beacon_x beacon_y

  let radius t = Coord.distance t.sensor t.beacon

  let _range left right =
    let start = min left right in
    let size = abs (left - right) + 1 in
    List.init size ~f:(fun i -> start + i)

  module Stable = struct
    module V1 = struct
      let _on_row_within_radius t ~row =
        let sensor = t.sensor in
        let r = radius t in
        let d = Coord.distance_to_row sensor ~row in
        match r >= d with
        | false -> None
        | true ->
            let offset = r - d in
            let start = Coord.x sensor - offset in
            let stop = Coord.x sensor + offset in
            Some (start, stop)
    end

    module V2 = struct
      let on_row_within_radius t ~row =
        let sensor = t.sensor in
        let r = radius t in
        let d = Coord.distance_to_row sensor ~row in
        match r >= d with
        | false -> None
        | true ->
            let offset = r - d in
            let start =
              Coord.x sensor - offset |> max lower_bound |> min upper_bound
            in
            let stop =
              Coord.x sensor + offset |> max lower_bound |> min upper_bound
            in
            Some (start, stop)
    end
  end

  module Current = Stable.V2
  include Current
end

module Stable = struct
  module V1 = struct
    let _main () =
      let sensors =
        List.map lines ~f:Sensor.of_string |> Option.all |> Option.value_exn
      in
      let beacons = List.map ~f:Sensor.beacon sensors |> Coord.Set.of_list in
      List.map sensors ~f:(Sensor.on_row_within_radius ~row)
      |> List.fold ~init:Range.empty ~f:(fun acc range_opt ->
             match range_opt with
             | None -> acc
             | Some range -> Range.add acc range)
      |> fun range ->
      Set.fold beacons ~init:range ~f:(fun acc beacon ->
          match row = Coord.y beacon with
          | false -> acc
          | true -> Range.splice acc ~on:(Coord.x beacon))
      |> Range.size
      |> Int.to_string
      |> print_endline
  end

  module V2 = struct
    let check_row sensors ~row =
      List.map sensors ~f:(Sensor.on_row_within_radius ~row)
      |> List.fold ~init:Range.empty ~f:(fun acc range_opt ->
             match range_opt with
             | None -> acc
             | Some range -> Range.add acc range)

    let rec find_row sensors ~row =
      match row <= upper_bound with
      | false -> None
      | true -> (
          let range = check_row sensors ~row in
          match Range.size range = upper_bound with
          | true -> (
              match Range.endpoints range with
              | [ (_, stop); _ ] -> Some (Coord.create ~x:(stop + 1) ~y:row)
              | _ -> None)
          | false -> find_row sensors ~row:(row + 1))

    let tuning_frequency coord = (Coord.x coord * 4000000) + Coord.y coord

    let main () =
      let sensors =
        List.map lines ~f:Sensor.of_string |> Option.all |> Option.value_exn
      in
      match find_row sensors ~row:lower_bound with
      | None -> print_endline "No distress beacon found"
      | Some coord -> tuning_frequency coord |> Int.to_string |> print_endline
  end
end

module Current = Stable.V2
include Current

let () = main ()
