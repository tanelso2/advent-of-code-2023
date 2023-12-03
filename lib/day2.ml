open Base
open Utils

type color = Red | Green | Blue
[@@deriving show]

let string_to_color (s: string): color =
    match s with
    | "red" -> Red
    | "green" -> Green
    | "blue" -> Blue
    | _ -> failwith ("Malformed color " ^ s)

type draw = color * int
[@@deriving show]
type game = {
    number: int;
    draws: draw list list
}[@@deriving show]

let parse_game_num s : int =
    String.substr_replace_first s
        ~pattern:"Game " ~with_:""
    |> Int.of_string

let parse_draw s : draw =
    let s = String.strip s in
    match String.split ~on:' ' s with
    | [num_str;color_str] ->
            (string_to_color color_str, Int.of_string num_str)
    | _ -> failwith @@ "failed to parse draw for \"" ^ s ^ "\""

let parse_draw_list s : draw list =
    let draws = String.split ~on:',' s in
    List.map ~f:parse_draw draws

let parse_draws s : draw list list =
    let draws = String.split ~on:';' s in
    List.map ~f:parse_draw_list draws

let parse_line (s: string): game =
    match String.split ~on:':' s with
    | [game_str;draws_str] ->
            let game_num = parse_game_num game_str in
            let draws = parse_draws draws_str in
            {number=game_num; draws=draws}
    | _ -> failwith "failed to split line properly"

let parse_input (s: string): game list =
    let lines = String.split_lines s in
    List.map ~f:parse_line lines

let color_count (draws: draw list) : (int * int * int) =
    let red = ref 0 in
    let green = ref 0 in
    let blue = ref 0 in
    let f (c,n) =
        match c with
        | Red -> red := !red + n
        | Green -> green := !green + n
        | Blue -> blue := !blue + n
    in
    List.iter ~f:f draws;
    (!red, !green, !blue)

let get_red (r,_,_) = r
let get_green (_,g,_) = g
let get_blue (_,_,b) = b

let max_colors (draws: draw list list) : (int * int * int) =
    let counts = List.map ~f:color_count draws in
    let get_color f =
        ListUtils.max_by_exn ~key_fn:f counts |> f
    in
    let red = get_color get_red in
    let green = get_color get_green in
    let blue = get_color get_blue  in
    (red,green,blue)

let is_valid_game (g: game) : bool =
    let (r,g,b) = max_colors g.draws in
    (* Stdio.printf "r:%d, g:%d, b:%d\n" r g b; *)
    r <= 12 && g <= 13 && b <= 14

let game_power (g: game): int =
    let (r,g,b) = max_colors g.draws in
    r*g*b

let part1 (s:string) =
    let games =  parse_input s in
    let valid_games = List.filter ~f:is_valid_game games in
    let valid_numbers = List.map ~f:(fun {number;_} -> number) valid_games in
    let final_sum = ListUtils.sum valid_numbers in
    (* Stdio.printf "Valid numbers: %s\n" @@ [%show: int list] valid_numbers; *)
    final_sum

let part2 (s:string) =
    let games = parse_input s in
    List.map ~f:game_power games
    |> ListUtils.sum
