open Core

let read_input file = In_channel.read_lines file

module Cargo = struct
  type t = string Stack.t Int.Map.t

  let indices = List.init 9 ~f:(( + ) 1)

  let empty : t =
    List.map indices ~f:(fun i -> (i, Stack.create ())) |> Int.Map.of_alist_exn

  let push t i v = Stack.push (Map.find_exn t i) v
  let pop t i = Stack.pop (Map.find_exn t i)

  let move t ~src ~dest =
    let v = pop t src |> Option.value_exn in
    push t dest v

  let move_multiple t ~amount ~src ~dest =
    List.init amount ~f:Fn.id
    |> List.map ~f:(fun _ -> pop t src)
    |> Option.all
    |> Option.value_exn
    |> List.rev
    |> List.iter ~f:(fun v -> push t dest v)
end

let parse_stack stack =
  let stack_regex = ".([A-Z ]). ?" in
  let re = Re2.create_exn stack_regex in
  let matches = Re2.get_matches_exn re stack in
  List.map matches ~f:(Re2.Match.get ~sub:(`Index 1))
  |> List.map
       ~f:
         (Option.bind ~f:(fun x ->
              match String.equal x " " with true -> None | false -> Some x))

let parse_move move =
  let move_regex = "move ([0-9]+) from ([1-9]) to ([1-9])" in
  let re = Re2.create_exn move_regex in
  let matches = Re2.get_matches_exn re move in
  match matches with
  | [ m ] -> (
      match
        List.map [ 1; 2; 3 ] ~f:(fun i ->
            Re2.Match.get_exn ~sub:(`Index i) m |> Int.of_string)
      with
      | [ amount; src; dest ] -> Some (amount, src, dest)
      | _ -> None)
  | _ -> None

module V1 = struct
  let _move_crates moves cargo =
    List.map moves ~f:parse_move
    |> List.filter_map ~f:Fn.id
    |> List.iter ~f:(fun (amount, src, dest) ->
           List.init amount ~f:Fn.id
           |> List.iter ~f:(fun _ -> Cargo.move cargo ~src ~dest))
end

module V2 = struct
  let move_crates moves cargo =
    List.map moves ~f:parse_move
    |> List.filter_map ~f:Fn.id
    |> List.iter ~f:(fun (amount, src, dest) ->
           Cargo.move_multiple cargo ~amount ~src ~dest)
end

module Current = V2
include Current

let insert_crates stack cargo =
  List.map stack ~f:parse_stack
  |> List.rev
  |> List.iter
       ~f:
         (List.iteri ~f:(fun i -> function
            | Some crate -> Cargo.push cargo (i + 1) crate | None -> ()))

let main () =
  let input = read_input "day5/input.txt" in
  let stack = List.take input 8 in
  let moves = List.drop input 9 in
  let cargo = Cargo.empty in
  let () = insert_crates stack cargo in
  let () = move_crates moves cargo in
  let top_string =
    List.map Cargo.indices ~f:(fun i -> Cargo.pop cargo i)
    |> Option.all
    |> Option.map ~f:String.concat
    |> Option.value_exn
  in
  print_endline top_string

let () = main ()
