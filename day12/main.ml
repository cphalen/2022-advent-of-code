open Core

module Position = struct
  type t = int * int

  include Tuple.Comparable (Int) (Int)
  include Tuple.Hashable (Int) (Int)

  let neighbors (r, c) =
    [ (r - 1, c); (r, c - 1); (r + 1, c); (r, c + 1) ] |> Set.of_list

  let _to_string (r, c) = Printf.sprintf "(%i, %i)" r c
end

module Square = struct
  type t = Start | End | Height of int

  let of_char = function
    | 'S' -> Start
    | 'E' -> End
    | char -> Char.to_int char - Char.to_int 'a' |> Height

  let _to_string = function
    | Start -> "Start"
    | End -> "End"
    | Height h -> Printf.sprintf "Height: %i" h

  let height = function Start -> 0 | End -> 25 | Height h -> h
  let traversable ~src ~dest = height src + 1 >= height dest
end

module Height_map = struct
  type t = Square.t Position.Map.t

  let of_lines lines : t =
    List.foldi lines ~init:Position.Map.empty ~f:(fun r map line ->
        String.foldi line ~init:map ~f:(fun c map char ->
            Map.set map ~key:(r, c) ~data:(Square.of_char char)))

  let _to_string t ~distances =
    Map.mapi distances ~f:(fun ~key:(r, c) ~data ->
        let height = Map.find_exn t (r, c) |> Square.height in
        match c = 0 with
        | true -> Printf.sprintf "\n%04d(%02d)" data height
        | false -> Printf.sprintf " %04d(%02d)" data height)
    |> Map.data
    |> String.concat

  let start_pos =
    Map.fold ~init:None ~f:(fun ~key:pos ~data:square acc ->
        match square with Square.Start -> Some pos | _ -> acc)

  let end_pos =
    Map.fold ~init:None ~f:(fun ~key:pos ~data:square acc ->
        match square with Square.End -> Some pos | _ -> acc)

  let neighbors t ~pos =
    let current = Map.find_exn t pos in
    Position.neighbors pos
    |> Set.filter ~f:(fun neighbor_pos ->
           match Map.find t neighbor_pos with
           | None -> false
           | Some neighbor -> Square.traversable ~src:neighbor ~dest:current)

  let end_distances t =
    let end_pos = end_pos t |> Option.value_exn in
    let distances = Hashtbl.create (module Position) in
    let () = Hashtbl.set distances ~key:end_pos ~data:0 in
    let queue = Queue.create ~capacity:(Map.length t) () in
    let () = Queue.enqueue queue end_pos in
    let () =
      while Queue.is_empty queue |> not do
        let current_pos = Queue.dequeue_exn queue in
        let current_dist = Hashtbl.find_exn distances current_pos in
        let neighbors = neighbors t ~pos:current_pos in
        Set.iter neighbors ~f:(fun neighbor_pos ->
            match Hashtbl.find distances neighbor_pos with
            | None -> (
                let () =
                  Hashtbl.set distances ~key:neighbor_pos
                    ~data:(current_dist + 1)
                in
                match Queue.mem queue neighbor_pos ~equal:Position.equal with
                | true -> ()
                | false -> Queue.enqueue queue neighbor_pos)
            | _ -> ())
      done
    in
    Position.Map.of_hashtbl_exn distances

  let all_starting_pos (t : t) =
    Map.filter t ~f:(fun square -> Square.height square = 0) |> Map.keys
end

let lines = In_channel.read_lines "day12/input.txt"

module V1 = struct
  let _main () =
    let heights = Height_map.of_lines lines in
    let end_distances = Height_map.end_distances heights in
    Height_map.start_pos heights
    |> Option.value_exn
    |> Map.find_exn end_distances
    |> Int.to_string
    |> print_endline
end

module V2 = struct
  let main () =
    let heights = Height_map.of_lines lines in
    let end_distances = Height_map.end_distances heights in
    Height_map.all_starting_pos heights
    |> List.filter_map ~f:(Map.find end_distances)
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
    |> Int.to_string
    |> print_endline
end

module Current = V2
include Current

let () = main ()
