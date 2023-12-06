open Base
open Utils

let parse_int_list s : int list =
    s |> String.strip
      |> String.split ~on:' '
      |> List.filter ~f:(fun x -> (not (0 = (String.length x))))
      |> List.map ~f:Int.of_string

let remove_card_title s : string =
    match String.split ~on:':' s with
    | [_; number_list] -> number_list
    | _ -> failwith "string did not contain 1 colon"

type card_pair = (int list * int list)

let parse_line (s: string) : (int list * int list) =
    match String.split ~on:'|' s with
    | [s1;s2] ->
            (parse_int_list (remove_card_title s1), parse_int_list s2)
    | _ -> failwith "line did not contain 1 pipe"

let parse_input (s: string) : card_pair list =
    String.split_lines s
    |> List.map ~f:parse_line

let score_matches (n: int) : int =
    match n with
    | 0 -> 0
    | _ -> Int.pow 2 (n - 1)

let number_of_matches (winning_numbers: int list) (held_numbers: int list) : int =
    List.filter ~f:(fun n -> List.exists ~f:((=) n) winning_numbers) held_numbers
    |> List.length

let score_hand (winning_numbers: int list) (held_numbers: int list) : int =
    number_of_matches winning_numbers held_numbers
    |> score_matches

let score_card (x,y) =
    score_hand x y

let part1 (s:string) =
    let rounds = parse_input s in
    List.map ~f:score_card rounds
    |> ListUtils.sum

let inc (a: int array) (idx: int) (n: int) : unit =
    a.(idx-1) <- a.(idx-1) + n

let inc_many (a: int array) (start: int) (n: int) (multiplier: int) : unit =
    let rec helper n =
        match n with
        | 0 -> ()
        | _ -> inc a (start+n) multiplier;
               helper (n-1)
    in
    helper n

let process_cards rounds =
    let n = List.length rounds in
    let card_counts = Array.init n ~f:(fun _ -> 1) in
    let rec helper (i: int) (rounds: card_pair list) =
       match rounds with
       | (wn,hn)::rest -> (
           let matches = number_of_matches wn hn in
           let number_of_this_card = card_counts.(i-1) in
           inc_many card_counts i matches number_of_this_card;
           helper (i+1) rest
       )
       | [] -> Array.fold ~init:0 ~f:(+) card_counts
    in
    helper 1 rounds


let part2 (s:string) =
    let rounds = parse_input s in
    process_cards rounds
