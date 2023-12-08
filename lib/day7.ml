open Base
open Utils

type hand_type = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | OnePair | HighCard
[@@deriving show,ord]

let largest_group (x: 'a list list) : int =
    List.map ~f:List.length x
    |> ListUtils.max_exn

let type_of_group groups =
    match List.length groups with
    | 1 -> FiveOfAKind
    | 2 -> (
        match largest_group groups with
        | 4 -> FourOfAKind
        | 3 -> FullHouse
        | _ -> failwith "shouldn't happen"
      )
    | 3 -> (
        match largest_group groups with
        | 3 -> ThreeOfAKind
        | 2 -> TwoPair
        | _ -> failwith "Shouldn't happen"
    )
    | 4 -> OnePair
    | 5 -> HighCard
    | _ -> failwith "shouldn't happen"

let calc_hand_type (hand: char list) : hand_type =
    let groups = List.sort_and_group ~compare:Char.compare hand in
    type_of_group groups

let card_points = function
| 'A' -> 14
| 'K' -> 13
| 'Q' -> 12
| 'J' -> 11
| 'T' -> 10
| x -> IntUtils.of_char x

let card_compare x y = Int.neg @@ Int.compare (card_points x) (card_points y)

let rec cardwise_compare x y =
    match (x,y) with
    | (c1::rest1,c2::rest2) -> (
        match card_compare c1 c2 with
        | 0 -> cardwise_compare rest1 rest2
        | v -> v
    )
    | _ -> failwith "Ran out of cards to compare?"

let hand_compare (a: char list) (b: char list) : int =
    let a_type = calc_hand_type a in
    let b_type = calc_hand_type b in
    match compare_hand_type a_type b_type with
    | 0 -> cardwise_compare a b
    | x -> x

let parse_hand s : (char list * int) =
    match String.split ~on:' ' s with
    | [hand;bid] -> (String.to_list hand, Int.of_string bid)
    | _ -> failwith "Failed to parse"

let parse_hands s : (char list * int) list =
    String.split_lines s |> List.map ~f:parse_hand

let sort_hands hands : (char list * int) list =
    List.sort ~compare:(fun (h1,_) (h2,_) -> hand_compare h1 h2) hands

let score (hands : (char list * int) list) : int =
    let hands' = sort_hands hands in
    let l = List.length hands' in
    let f i (_,bid) =
        let rank = l - i in
        (* Stdio.printf "rank:%d i:%d bid:%d\n" rank i bid; *)
        rank * bid
    in
    List.mapi ~f:f hands' |> ListUtils.sum

let part1 (s:string) =
    let hands = parse_hands s in
    score hands

let is_joker_group l = phys_equal 'J' (List.hd_exn l)

(* if joker is found *)
(* remove J group from list *)
(* find largest grouping *)
(* increase size by number of Js *)
(* use that to determine type *)

let calc_hand_type2 (hand: char list) : hand_type =
    let groups = List.sort_and_group ~compare:Char.compare hand in
    let (jokers, rest) = List.partition_tf ~f:is_joker_group groups in
    match jokers with
    | x::_ -> (
            match ListUtils.max_by ~key_fn:List.length rest with
            | None -> type_of_group groups (* hand full of jokers *)
            | Some largest -> (
                let l = List.length x in
                let a = List.hd_exn largest in
                let (_,others) = List.partition_tf ~f:(fun x -> phys_equal a (List.hd_exn x)) rest in
                let largest_with_jokers : char list = List.init (List.length largest + l) ~f:(fun _ -> a) in
                type_of_group (largest_with_jokers::others)
            )
    )
    | _ -> type_of_group groups

let card_points2 = function
    | 'J' -> 0
    | x -> card_points x

let card_compare2 x y = Int.neg @@ Int.compare (card_points2 x) (card_points2 y)

let rec cardwise_compare2 x y =
    match (x,y) with
    | (c1::rest1,c2::rest2) -> (
        match card_compare2 c1 c2 with
        | 0 -> cardwise_compare2 rest1 rest2
        | v -> v
    )
    | _ -> failwith "Ran out of cards to compare?"

let hand_compare2 (a: char list) (b: char list) : int =
    let a_type = calc_hand_type2 a in
    let b_type = calc_hand_type2 b in
    match compare_hand_type a_type b_type with
    | 0 -> cardwise_compare2 a b
    | x -> x

let sort_hands2 hands : (char list * int) list =
    List.sort ~compare:(fun (h1,_) (h2,_) -> hand_compare2 h1 h2) hands


let score2 (hands : (char list * int) list) : int =
    let hands' = sort_hands2 hands in
    let l = List.length hands' in
    let f i (_,bid) =
        let rank = l - i in
        (* Stdio.printf "rank:%d i:%d bid:%d\n" rank i bid; *)
        rank * bid
    in
    List.mapi ~f:f hands' |> ListUtils.sum

let part2 (s:string) =
    let hands = parse_hands s in
    score2 hands
