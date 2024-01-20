open Base
open! Utils

type directions = char RingBuffer.t
type tunnel_map = (string, (string * string), String.comparator_witness) Map.t

let parse_directions (s: string) : directions =
    String.to_list s
    |> RingBuffer.of_list

let tunnel_regex = Re.Posix.compile_pat {|([A-Z]+) = \(([A-Z]+), ([A-Z]+)\)|}

let parse_tunnel_line (line: string) : (string * (string * string)) =
    match Re.exec_opt tunnel_regex @@ String.strip line with 
    | Some res -> (
        let current = Re.Group.get res 1 in
        let left = Re.Group.get res 2 in
        let right = Re.Group.get res 3 in
        (current, (left, right))
    )
    | None -> failwith ("parse failed with " ^ line)

let parse_tunnels (lines: string list) : tunnel_map =
    List.map ~f:parse_tunnel_line lines
    |> Map.of_alist_exn (module String)

let parse_input (s:string) : (directions * tunnel_map) =
    let lines = 
        String.split_lines s 
        |> List.filter ~f:(fun x -> String.length x > 0)
    in
    match lines with
    | x::xs -> (parse_directions x, parse_tunnels xs)
    | _ -> failwith "shouldn't happen"

let get_tunnel (m: tunnel_map) (x: string) : (string * string) =
    Map.find_exn m x

let get_next_no_inc (m: tunnel_map) (d: char) (k: string) : string =
    let (left,right) = get_tunnel m k in
    match d with
    | 'L' -> left
    | 'R' -> right
    | _ -> failwith "Ringbuffer returned a weird character"


let get_next (m: tunnel_map) (dir: directions) (k: string) : string =
    let d = RingBuffer.read_and_shift dir in
    get_next_no_inc m d k

let find_exit (m: tunnel_map) (dir: directions) : int =
    let rec helper curr round =
        match curr with
        | "ZZZ" -> round
        | _ -> helper (get_next m dir curr) (round+1)
    in
    helper "AAA" 0


let part1 (s:string) =
    let (d,m) = parse_input s in
    find_exit m d

let is_starting_node (s: string) : bool =
    String.suffix s 1
    |> String.equal "A"

let is_ending_node s =
    String.suffix s 1
    |> String.equal "Z"

let get_starting_nodes (m: tunnel_map) : string list =
    Map.keys m 
    |> List.filter ~f:is_starting_node

let find_exits (m: tunnel_map) (dir: directions) : int =
    let rec helper currs round =
        if ListUtils.all ~f:is_ending_node currs
        then round
        else (
            let d = RingBuffer.read_and_shift dir in
            let nexts = List.map ~f:(get_next_no_inc m d) currs in
            helper nexts (round+1)
        )
    in
    let starting_nodes = get_starting_nodes m in
    helper starting_nodes 0


let part2 (s:string) =
    let (d,m) = parse_input s in
    find_exits m d
