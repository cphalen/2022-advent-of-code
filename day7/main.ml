open Core

module Cli = struct
  type t = Cd_up | Cd of string | Ls | File of int * string | Dir of string

  let cd_up_re = Re2.create_exn "\\$ cd \\.\\."
  let cd_re = Re2.create_exn "\\$ cd (.+)"
  let ls_re = Re2.create_exn "\\$ ls"
  let file_re = Re2.create_exn "([0-9]+) (.*)"
  let dir_re = Re2.create_exn "dir (.+)"

  let of_string s =
    match
      List.map
        ~f:(fun re -> Re2.get_matches_exn re s)
        [ cd_up_re; cd_re; ls_re; file_re; dir_re ]
    with
    | [ [ _ ]; _; _; _; _ ] -> Some Cd_up
    | [ _; [ m ]; _; _; _ ] ->
        let dest = Re2.Match.get ~sub:(`Index 1) m in
        Option.map ~f:(fun dest -> Cd dest) dest
    | [ _; _; [ _ ]; _; _ ] -> Some Ls
    | [ _; _; _; [ m ]; _ ] ->
        let size = Re2.Match.get ~sub:(`Index 1) m in
        let name = Re2.Match.get ~sub:(`Index 2) m in
        Option.bind
          ~f:(fun size ->
            Option.map ~f:(fun name -> File (Int.of_string size, name)) name)
          size
    | [ _; _; _; _; [ m ] ] ->
        let name = Re2.Match.get ~sub:(`Index 1) m in
        Option.map ~f:(fun name -> Dir name) name
    | _ -> None

  let _to_string = function
    | Cd_up -> "$ cd .."
    | Cd dest -> Printf.sprintf "$ cd %s" dest
    | Ls -> "$ ls"
    | File (size, name) -> Printf.sprintf "%d %s" size name
    | Dir name -> Printf.sprintf "dir %s" name
end

module Dir = struct
  type t = {
    parent : t option;
    name : string;
    mutable children : t String.Map.t;
    mutable files : int String.Map.t;
    mutable size : int;
  }
  [@@deriving fields]

  let create parent name =
    {
      parent = Some parent;
      name;
      children = String.Map.empty;
      files = String.Map.empty;
      size = 0;
    }

  let root =
    {
      parent = None;
      name = "root";
      children = String.Map.empty;
      files = String.Map.empty;
      size = 0;
    }

  let add_child t ~name =
    t.children <- Map.set t.children ~key:name ~data:(create t name)

  let add_file t ~name ~size = t.files <- Map.set t.files ~key:name ~data:size

  let update t = function
    | Cli.Cd_up -> t.parent |> Option.value_exn
    | Cd dest ->
        if Map.mem t.children dest |> not then add_child t ~name:dest;
        Map.find_exn t.children dest
    | Ls -> t
    | File (size, name) ->
        add_file t ~name ~size;
        t
    | Dir name ->
        if Map.mem t.children name |> not then add_child t ~name;
        t

  let rec size_files t =
    Map.fold t.files ~init:0 ~f:(fun ~key:_ ~data:size total_size ->
        total_size + size)

  and size_children t =
    Map.fold t.children ~init:0 ~f:(fun ~key:_ ~data:t total_size ->
        size t + total_size)

  and size t =
    let size =
      match Map.length t.children with
      | 0 -> size_files t
      | _ -> size_children t + size_files t
    in
    t.size <- size;
    size

  let rec to_sizes_list t =
    t.size :: (Map.data t.children |> List.map ~f:to_sizes_list |> List.concat)
end

let lines = In_channel.read_lines "day7/input.txt"

module V1 = struct
  let threshold = 100000

  let _main () =
    let clis = List.map lines ~f:Cli.of_string |> List.filter_map ~f:Fn.id in
    let _ = List.fold clis ~init:Dir.root ~f:Dir.update in
    let _ = Dir.size Dir.root in
    Dir.to_sizes_list Dir.root
    |> List.filter ~f:(( >= ) threshold)
    |> List.sum (module Int) ~f:Fn.id
    |> Int.to_string
    |> print_endline
end

module V2 = struct
  let total_size = 70000000
  let required_size = 30000000

  let main () =
    let clis = List.map lines ~f:Cli.of_string |> List.filter_map ~f:Fn.id in
    let _ = List.fold clis ~init:Dir.root ~f:Dir.update in
    let total_used = Dir.size Dir.root in
    let total_free = total_size - total_used in
    let need_to_free = required_size - total_free in
    Dir.to_sizes_list Dir.root
    |> List.filter ~f:(( <= ) need_to_free)
    |> List.fold ~init:total_size ~f:(fun acc dir_size ->
           if dir_size < acc then dir_size else acc)
    |> Int.to_string
    |> print_endline
end

module Current = V2

let () = Current.main ()
