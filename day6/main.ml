open Core

let input = In_channel.create "day6/input.txt"

let is_marker s =
  let chars = List.init (String.length s) ~f:(String.get s) in
  List.contains_dup chars ~compare:Char.compare |> not

let map_buffer s ~f ~size =
  String.length s - size
  |> List.init ~f:(fun i -> String.sub s ~pos:i ~len:size)
  |> List.map ~f

module V1 = struct
  let _buffer_size = 4
end

module V2 = struct
  let buffer_size = 14
end

module Current = V2
include Current

let main () =
  match In_channel.input_line input with
  | None -> ()
  | Some s ->
      map_buffer s ~size:buffer_size ~f:is_marker
      |> List.findi_exn ~f:(fun _ buf -> buf)
      |> fst
      |> ( + ) buffer_size
      |> Int.to_string
      |> print_endline

let () = main ()
