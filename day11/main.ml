open Core

module Monkey = struct
  type t = {
    items : int list;
    operation : int -> int;
    test : int -> int;
    inspection_count : int;
  }
  [@@deriving fields]

  module V1 = struct
    let _worry_level t ~item = t.operation item |> Fn.flip ( / ) 3
  end

  module V2 = struct
    (* product of all divisibility tests *)
    let modulo = 9699690

    let worry_level t ~item = t.operation item |> Fn.flip ( % ) modulo
  end

  module Current = V2
  include Current

  let parse_starting_items s =
    let starting_items_re = Re2.create_exn "Starting items: ([0-9, ]+)" in
    match Re2.get_matches_exn starting_items_re s with
    | [ m ] ->
        let open Option.Let_syntax in
        let%map item_string = Re2.Match.get m ~sub:(`Index 1) in
        String.substr_replace_all item_string ~pattern:" " ~with_:""
        |> String.split ~on:','
        |> List.map ~f:Int.of_string
    | _ -> None

  let parse_operation s =
    let operation_re = Re2.create_exn "Operation: new = old ([+*]) ([0-9]+)" in
    let operation_with_var_re =
      Re2.create_exn "Operation: new = old ([+*]) old"
    in
    match
      List.map
        ~f:(fun re -> Re2.get_matches_exn re s)
        [ operation_re; operation_with_var_re ]
    with
    | [ [ m ]; _ ] ->
        let open Option.Let_syntax in
        let%bind operation_string = Re2.Match.get m ~sub:(`Index 1) in
        let%bind parameter_string = Re2.Match.get m ~sub:(`Index 2) in
        let%map operation =
          match operation_string with
          | "+" -> Some ( + )
          | "*" -> Some ( * )
          | _ -> None
        in
        let parameter = Int.of_string parameter_string in
        operation parameter
    | [ _; [ m ] ] ->
        let open Option.Let_syntax in
        let%bind operation_string = Re2.Match.get m ~sub:(`Index 1) in
        let%map operation =
          match operation_string with
          | "+" -> Some ( + )
          | "*" -> Some ( * )
          | _ -> None
        in
        fun x -> operation x x
    | _ -> None

  let parse_test test if_true if_false =
    let test_re = Re2.create_exn "Test: divisible by ([0-9]+)" in
    let if_true_re = Re2.create_exn "If true: throw to monkey ([0-9]+)" in
    let if_false_re = Re2.create_exn "If false: throw to monkey ([0-9]+)" in
    let open Option.Let_syntax in
    let%bind pred =
      match Re2.get_matches_exn test_re test with
      | [ m ] ->
          let%map divisor_string = Re2.Match.get m ~sub:(`Index 1) in
          let divisor = Int.of_string divisor_string in
          fun x -> x mod divisor = 0
      | _ -> None
    in
    let%bind if_true =
      match Re2.get_matches_exn if_true_re if_true with
      | [ m ] ->
          let%map if_true_string = Re2.Match.get m ~sub:(`Index 1) in
          Int.of_string if_true_string
      | _ -> None
    in
    let%map if_false =
      match Re2.get_matches_exn if_false_re if_false with
      | [ m ] ->
          let%map if_false_string = Re2.Match.get m ~sub:(`Index 1) in
          Int.of_string if_false_string
      | _ -> None
    in
    fun x -> if pred x then if_true else if_false

  let of_input = function
    | [ _; starting_items; operation; test; if_true; if_false ] ->
        let open Option.Let_syntax in
        let%bind starting_items = parse_starting_items starting_items in
        let%bind operation = parse_operation operation in
        let%map test = parse_test test if_true if_false in
        { items = starting_items; operation; test; inspection_count = 0 }
    | _ -> None

  let move_monkey ~monkeys ~dest ~worry_level =
    let old_dest = Map.find_exn monkeys dest in
    let new_dest = { old_dest with items = old_dest.items @ [ worry_level ] } in
    Map.set monkeys ~key:dest ~data:new_dest

  let turn t ~index ~monkeys =
    let monkeys =
      List.fold t.items ~init:monkeys ~f:(fun monkeys item ->
          let worry_level = worry_level t ~item in
          let dest = t.test worry_level in
          move_monkey ~monkeys ~dest ~worry_level)
    in
    let old_src = Map.find_exn monkeys index in
    let new_src =
      {
        old_src with
        inspection_count = old_src.inspection_count + List.length old_src.items;
        items = [];
      }
    in
    Map.set monkeys ~key:index ~data:new_src

  let to_string t =
    let items = List.map ~f:Int.to_string t.items |> String.concat ~sep:", " in
    Printf.sprintf "[%s] (inspection_count = %i)" items t.inspection_count
end

let lines = In_channel.read_lines "day11/input.txt"
let num_monkeys = 8

let group_on_empty_line ~lines =
  List.fold lines ~init:([], []) ~f:(fun (curr, grouped) x ->
      if String.length x = 0 then ([], curr :: grouped) else (x :: curr, grouped))
  |> fun (fst, rest) -> fst :: rest |> List.map ~f:List.rev |> List.rev

let monkey_business ~monkeys =
  Map.data monkeys
  |> List.map ~f:Monkey.inspection_count
  |> List.sort ~compare:Int.compare
  |> List.rev
  |> Fn.flip List.take 2
  |> List.fold ~init:1 ~f:( * )

let _print_monkeys ~monkeys =
  Map.iteri monkeys ~f:(fun ~key ~data ->
      Printf.printf "Monkey %i: %s\n" key (Monkey.to_string data));
  print_endline ""

let round monkeys =
  List.init num_monkeys ~f:Fn.id
  |> List.fold ~init:monkeys ~f:(fun monkeys index ->
         Map.find_exn monkeys index |> Monkey.turn ~index ~monkeys)

module V1 = struct
  let _num_rounds = 20
end

module V2 = struct
  let num_rounds = 10000
end

module Current = V2
include Current

let () =
  let monkeys_init =
    group_on_empty_line ~lines
    |> List.filter_map ~f:Monkey.of_input
    |> List.foldi ~init:Int.Map.empty ~f:(fun i monkeys monkey ->
           Map.set monkeys ~key:i ~data:monkey)
  in
  let monkeys_after_rounds =
    List.init num_rounds ~f:(fun _ -> round)
    |> List.fold ~init:monkeys_init ~f:(fun monkeys round -> round monkeys)
  in
  monkey_business ~monkeys:monkeys_after_rounds
  |> Int.to_string
  |> print_endline
