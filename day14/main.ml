open Core

let lines = In_channel.read_lines "day14/input.txt"

module Scan : sig
  type t

  val empty : t
  val parse_line : t -> string -> t option
  val populate : t -> t
  val count : t -> int
end = struct
  module Stable = struct
    module V1 = struct
      module Coord = Tuple.Comparable (Int) (Int)

      module Data = struct
        type t = Air | Sand | Rock | Source [@@deriving equal]

        let to_string = function
          | Air -> "Air"
          | Sand -> "Sand"
          | Rock -> "Rock"
          | Source -> "Source"
      end

      type t = Data.t Coord.Map.t

      let source_coord = (500, 0)

      let empty =
        Coord.Map.empty |> Coord.Map.set ~key:source_coord ~data:Data.Source

      let get t ~x ~y = Map.find t (x, y) |> Option.value ~default:Data.Air
      let set t ~x ~y ~data = Coord.Map.set t ~key:(x, y) ~data

      let range (x1, y1) (x2, y2) =
        match (x1 - x2, y1 - y2) with
        | d, 0 ->
            let base = if d < 0 then x1 else x2 in
            List.init (abs d + 1) ~f:(fun i -> base + i)
            |> List.map ~f:(fun x -> (x, y1))
            |> Some
        | 0, d ->
            let base = if d < 0 then y1 else y2 in
            List.init (abs d + 1) ~f:(fun i -> base + i)
            |> List.map ~f:(fun y -> (x1, y))
            |> Some
        | _ -> None

      let rec add_segments t = function
        | start :: stop :: rest ->
            let open Option.Let_syntax in
            let%bind range = range start stop in
            let t =
              List.fold range ~init:t ~f:(fun t (x, y) ->
                  set t ~x ~y ~data:Data.Rock)
            in
            add_segments t (stop :: rest)
        | _ -> Some t

      let parse_line t s =
        let open Option.Let_syntax in
        let%bind coords =
          String.split_on_chars ~on:[ ' '; '>'; '-' ] s
          |> List.filter ~f:(fun s -> String.length s > 0)
          |> List.map ~f:(fun s ->
                 match String.split ~on:',' s with
                 | [ x; y ] -> Some (Int.of_string x, Int.of_string y)
                 | _ -> None)
          |> Option.all
        in
        add_segments t coords

      let _to_string t =
        Map.mapi t ~f:(fun ~key:(x, y) ~data ->
            Printf.sprintf "(%i, %i) -> %s" x y (Data.to_string data))
        |> Map.data
        |> String.concat ~sep:"\n"

      let height t =
        Map.keys t
        |> List.map ~f:snd
        |> List.max_elt ~compare:Int.compare
        |> Option.value ~default:0

      let step t =
        let open Option.Let_syntax in
        let rec place_sand ~pos:(x, y) =
          let height = height t in
          match y >= height with
          | true -> None
          | false -> (
              match
                [ (x, y + 1); (x - 1, y + 1); (x + 1, y + 1) ]
                |> List.map ~f:(fun (x, y) -> get t ~x ~y)
              with
              | [ Air; _; _ ] -> place_sand ~pos:(x, y + 1)
              | [ _; Air; _ ] -> place_sand ~pos:(x - 1, y + 1)
              | [ _; _; Air ] -> place_sand ~pos:(x + 1, y + 1)
              | _ -> Some (x, y))
        in
        let%map x, y = place_sand ~pos:source_coord in
        set t ~x ~y ~data:Data.Sand

      let rec _populate t =
        match step t with None -> t | Some new_t -> _populate new_t

      let _count t = Map.count t ~f:(Data.equal Data.Sand)
    end

    module V2 = struct
      include V1

      let height t =
        Map.filter t ~f:(Data.equal Data.Rock)
        |> Map.keys
        |> List.map ~f:snd
        |> List.max_elt ~compare:Int.compare
        |> Option.map ~f:(Int.( + ) 2)
        |> Option.value ~default:0

      let get t ~x ~y =
        match y < height t with
        | false -> Data.Rock
        | true -> Map.find t (x, y) |> Option.value ~default:Data.Air

      let q = Queue.create ()
      let counter = ref 0

      let rec coords_beneath t =
        match Queue.dequeue q with
        | None -> ()
        | Some (x, y) -> (
            match get t ~x ~y with
            | Source | Air ->
                Int.incr counter;
                [ (x, y + 1); (x - 1, y + 1); (x + 1, y + 1) ]
                |> List.iter ~f:(fun pos ->
                       if not (Queue.mem q pos ~equal:Coord.equal) then
                         Queue.enqueue q pos);
                coords_beneath t
            | _ -> coords_beneath t)

      let populate t =
        Queue.enqueue q source_coord;
        coords_beneath t;
        t

      let count (_ : t) = !counter
    end
  end

  module Current = Stable.V2
  include Current
end

module Stable = struct
  module V1 = struct
    let _main () =
      let scan =
        List.fold lines ~init:(Some Scan.empty) ~f:(fun scan_opt line ->
            Option.bind scan_opt ~f:(fun scan -> Scan.parse_line scan line))
      in
      match scan with
      | None -> ()
      | Some scan ->
          let populated_scan = Scan.populate scan in
          Scan.count populated_scan |> Int.to_string |> print_endline
  end

  module V2 = struct
    let main () =
      let scan =
        List.fold lines ~init:(Some Scan.empty) ~f:(fun scan_opt line ->
            Option.bind scan_opt ~f:(fun scan -> Scan.parse_line scan line))
      in
      match scan with
      | None -> ()
      | Some scan ->
          let populated_scan = Scan.populate scan in
          Scan.count populated_scan |> Int.to_string |> print_endline
  end
end

module Current = Stable.V2
include Current

let () = main ()
