open Base
open Utils

let find_distance (race_length: int) (x: int) =
    x*(race_length-x)

let beats_record (record: int) (race_length: int) (x: int) : bool =
    let dist = find_distance race_length x in
    dist > record

let how_many_ways_of_winning (l: int) (r: int) : int =
    let max_distance = l / 2 in
    let remainder = l % 2 in
    let winning_numbers =
        0 @.. max_distance
        |> List.filter ~f:(beats_record r l)
    in
    (* Stdio.printf "%s\n" @@ [%show: int list] winning_numbers; *)
    winning_numbers
    |> List.length
    |> (fun x -> if remainder = 0 then 2*x-1 else 2*x)

let parse_int_list s : int list =
    String.strip s
    |> String.split ~on:' '
    |> List.tl_exn (* remove label *)
    |> List.filter ~f:(fun x -> String.length x > 0)
    |> List.map ~f:String.strip
    |> List.map ~f:Int.of_string

let parse_input (s: string) : (int list * int list) =
    match String.split_lines s with
    | [time_line; record_line] ->
            (parse_int_list time_line, parse_int_list record_line)
    | _ -> failwith "shouldn't happen"

let part1 (s:string) : int =
    let (times, records) = parse_input s in
    match List.map2 times records ~f:how_many_ways_of_winning with
    | Ok x -> List.fold ~init:1 ~f:( * ) x
    | Unequal_lengths -> failwith "shouldn't happen"

let parse_single_int s : int =
    String.filter ~f:Char.is_digit s
    |> Int.of_string

let parse_input2 (s: string) : (int * int) =
    match String.split_lines s with
    | [time_line; record_line] ->
            (parse_single_int time_line, parse_single_int record_line)
    | _ -> failwith "shouldn't happen"

let part2 (s:string) =
    let (l,r) = parse_input2 s in
    how_many_ways_of_winning l r
