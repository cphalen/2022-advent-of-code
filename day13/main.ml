open Core

let lines = In_channel.read_lines "day13/input.txt"

module Packet = struct
  module T = struct
    type t = Digit of int | List of t list [@@deriving sexp]

    let rec compare p1 p2 =
      match (p1, p2) with
      | Digit d1, Digit d2 ->
          Int.compare d1 d2 (* the lower integer shoudl come first *)
      | (List _ as l1), (Digit _ as d2) -> compare l1 (List [ d2 ])
      | (Digit _ as d1), (List _ as l2) -> compare (List [ d1 ]) l2
      | List (x1 :: ls1), List (x2 :: ls2) ->
          let curr = compare x1 x2 in
          if curr <> 0 then curr else compare (List ls1) (List ls2)
      | List (_ :: _), List [] -> 1
      | List [], List (_ :: _) -> -1
      | List [], List [] -> 0
  end

  include Comparable.Make (T)
  include T

  let inorder p1 p2 = Int.( < ) (compare p1 p2) 0

  let splice_out ~on s =
    String.fold s ~init:([], []) ~f:(fun (curr, grouped) x ->
        if Char.equal x on then ([], [ x ] :: curr :: grouped)
        else (x :: curr, grouped))
    |> fun (curr, grouped) ->
    curr :: grouped
    |> List.map ~f:List.rev
    |> List.rev
    |> List.map ~f:String.of_char_list
    |> List.filter ~f:(fun s -> not (String.is_empty s))

  let token_list s =
    String.split s ~on:','
    |> List.concat_map ~f:(splice_out ~on:'[')
    |> List.concat_map ~f:(splice_out ~on:']')

  let rec split backward forward n =
    match n with
    | 0 -> Some (List.drop backward 1 |> List.rev, forward)
    | _ -> (
        match forward with
        | [] -> None
        | hd :: rest ->
            let n' = match hd with "[" -> n + 1 | "]" -> n - 1 | _ -> n in
            split (hd :: backward) rest n')

  let is_numeric s =
    String.to_list_rev s
    |> List.map ~f:Char.is_digit
    |> List.fold ~init:true ~f:( && )

  let rec of_token_list = function
    | [] -> []
    | hd :: rest -> (
        match hd with
        | "]" -> []
        | "[" -> (
            match split [] rest 1 with
            | None -> failwith "should not be here"
            | Some (before, after) ->
                List (of_token_list before) :: of_token_list after)
        | s when Int.( > ) (String.length s) 0 && is_numeric s ->
            Digit (Int.of_string s) :: of_token_list rest
        | _ -> failwith "invalid input")

  let of_string s = Some (List (token_list s |> of_token_list))

  let rec to_string = function
    | Digit d -> Int.to_string d
    | List ts ->
        Printf.sprintf "[%s]"
          (List.map ~f:to_string ts |> String.concat ~sep:",")
end

module Packet_group = struct
  type t = { first : Packet.t; second : Packet.t } [@@deriving fields, sexp]

  let of_string s =
    match String.split s ~on:'\n' with
    | [ first; second ] ->
        let open Option.Let_syntax in
        let%bind first = Packet.of_string first in
        let%map second = Packet.of_string second in
        { first; second }
    | _ -> None

  let _to_string group =
    [ group.first; group.second ]
    |> List.map ~f:Packet.to_string
    |> String.concat ~sep:"\n"

  let inorder t = Packet.inorder t.first t.second
end

let split ~on ls =
  List.fold ls ~init:([], []) ~f:(fun (curr, grouped) x ->
      if String.( = ) on x then ([], curr :: grouped) else (x :: curr, grouped))
  |> fun (last, rest) -> last :: rest |> List.map ~f:List.rev |> List.rev

module Stable = struct
  module V1 = struct
    let _main () =
      let packet_groups =
        split ~on:"" lines
        |> List.map ~f:(fun group ->
               String.concat ~sep:"\n" group |> Packet_group.of_string)
        |> List.filter ~f:Option.is_some
        |> List.map ~f:(fun x -> Option.value_exn x)
      in
      List.mapi packet_groups ~f:(fun i group -> (i + 1, group))
      |> List.filter ~f:(fun (_, group) -> Packet_group.inorder group)
      |> List.map ~f:(fun (i, _) -> i)
      |> List.sum (module Int) ~f:Fn.id
      |> Int.to_string
      |> print_endline
  end

  module V2 = struct
    let main () =
      let packet_groups =
        split ~on:"" lines
        |> List.map ~f:(fun group ->
               String.concat ~sep:"\n" group |> Packet_group.of_string)
        |> List.filter ~f:Option.is_some
        |> List.map ~f:(fun x -> Option.value_exn x)
      in
      let divider_packets =
        Packet.[ List [ List [ Digit 2 ] ]; List [ List [ Digit 6 ] ] ]
      in
      let packets =
        List.concat_map packet_groups ~f:(fun group ->
            [ group.first; group.second ])
        |> List.append divider_packets
      in
      List.sort packets ~compare:Packet.compare
      |> List.mapi ~f:(fun i packet -> (i + 1, packet))
      |> List.filter ~f:(fun (_, packet) ->
             List.mem divider_packets packet ~equal:Packet.equal)
      |> List.map ~f:(fun (i, _) -> i)
      |> List.fold ~init:1 ~f:Int.( * )
      |> Int.to_string
      |> print_endline
  end
end

module Current = Stable.V2
include Current

let () = main ()
