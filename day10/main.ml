open Core

module Instruction = struct
  type t = Noop | Addx of int

  let create s =
    let noop_re = Re2.create_exn "noop" in
    let addx_re = Re2.create_exn "addx (-?[0-9]+)" in
    match
      List.map ~f:(fun re -> Re2.get_matches_exn re s) [ noop_re; addx_re ]
    with
    | [ [ _ ]; _ ] -> Some Noop
    | [ _; [ m ] ] ->
        let imm = Re2.Match.get ~sub:(`Index 1) m in
        Option.map imm ~f:(fun imm -> Int.of_string imm |> Addx)
    | _ -> None
end

module Cycle_map = struct
  type t = { map : int Int.Map.t; index : int }

  let empty =
    let map = Int.Map.empty |> Map.set ~key:1 ~data:1 in
    { map; index = 2 }

  let get ?index t =
    let index = Option.value index ~default:(t.index - 1) in
    Map.find_exn t.map index

  let push t x =
    { map = Map.set t.map ~key:t.index ~data:x; index = t.index + 1 }

  let size t = t.index - 1

  let _to_string t = Map.to_alist t.map |> List.map ~f:(fun (key, data) -> Printf.sprintf ("%i -> %i") key data) |> String.concat ~sep:"\n"
end

let lines = In_channel.read_lines "day10/input.txt"

let init_cycle_map () =
  let ins_list =
    List.map lines ~f:(fun line -> Instruction.create line)
    |> List.filter_map ~f:Fn.id
  in
    List.fold ins_list ~init:Cycle_map.empty ~f:(fun cycle_map ins ->
        let x = Cycle_map.get cycle_map in
        match ins with
        | Noop -> Cycle_map.push cycle_map x
        | Addx imm ->
            let cycle_map = Cycle_map.push cycle_map x in
            let cycle_map = Cycle_map.push cycle_map (x + imm) in
            cycle_map)

module V1 = struct
let _main () =
  let cycle_map = init_cycle_map () in
  Cycle_map.size cycle_map
  |> List.init ~f:Fn.id
  |> List.filter ~f:(fun cycle -> cycle >= 20 && (cycle - 20) mod 40 = 0)
  |> List.map ~f:(fun cycle -> cycle * Cycle_map.get ~index:cycle cycle_map)
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string
  |> print_endline
end

module V2 = struct
let print_pixel ~cycle_map cycle =
  let x = Cycle_map.get cycle_map ~index:cycle in
  let index = cycle - 1 in
  let pixel = match abs (x - (index mod 40)) with
    | -1 | 0 | 1 -> "#"
    | _ -> "."
  in
  print_string pixel; if index mod 40 = 39 then print_string "\n"

let main () =
  let cycle_map = init_cycle_map () in
  Cycle_map.size cycle_map
  |> List.init ~f:Fn.id
  |> Fn.flip List.drop 1
  |> List.iter ~f:(print_pixel ~cycle_map)
end

module Current = V2

let () = Current.main ()
