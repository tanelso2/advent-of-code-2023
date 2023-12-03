open Base
open Utils

type loc = int * int
[@@deriving show]
type part_number = loc * loc * int
[@@deriving show]
type symbol = loc * char
[@@deriving show]

let touches_symbol ((x1,y),(x2,_),_) (((sx,sy),_): symbol)  : bool =
    let lb = x1-1 in
    let rb = x2+1 in
    let ub = y-1 in
    let db = y+1 in
    sx >= lb && sx <= rb && sy >= ub && sy <= db

let touches_symbols (syms: symbol list) l : bool =
    ListUtils.some ~f:(touches_symbol l) syms

let parse_line y (s: string) : (symbol list * part_number list) =
    let syms = ref [] in
    let nums = ref [] in
    let current_first: loc option ref = ref None in
    let finish_number x =
        match !current_first with
        | None -> ()
        | Some (ox,oy) ->
                let n =
                    String.sub s ~pos:ox ~len:(x-ox+1)
                    |> Int.of_string
                in
                let new_num = ((ox,oy),(x,y),n) in
                nums := new_num::!nums;
                current_first := None
    in
    let f x c =
        if Char.is_digit c
        then begin
            match !current_first with
            | None -> current_first := Some (x,y)
            | Some _ -> () (* Do nothing, progress on to next char *)
        end
        else begin
            finish_number (x-1);
            match c with
            | '.' -> ()
            (* Add new symbol if not a period *)
            | _ -> syms := ((x,y),c)::!syms
        end
    in
    String.iteri ~f:f s;
    finish_number (String.length s - 1);
    (!syms, !nums)

let parse_input (s: string) : (symbol list * part_number list) =
    let lines = String.split_lines s in
    List.mapi ~f:parse_line lines
    |> List.fold_left
        ~init:([], [])
        ~f:(fun (acc_sym, acc_num) (sym, num) ->
            (List.append acc_sym sym, List.append acc_num num))

let get_number (_,_,n) = n

let get_valid_numbers syms nums =
    List.filter ~f:(touches_symbols syms) nums
        |> List.map ~f:get_number

let part1 (s:string) =
    let (syms, nums) = parse_input s in
    let valid_nums =
        List.filter ~f:(touches_symbols syms) nums
            |> List.map ~f:get_number
    in
    ListUtils.sum valid_nums

let gear_ratio nums sym =
    match sym with
    | (_, '*') -> (
        let f num =
            touches_symbol num sym
        in
        match List.filter ~f:f nums with
        | [n1;n2] -> get_number n1 * get_number n2
        | _ -> 0
    )
    | _ -> 0

let total_gear_ratio nums syms =
    let ratios = List.map ~f:(gear_ratio nums) syms in
    ListUtils.sum ratios

let part2 (s:string) =
    let (syms, nums) = parse_input s in
    total_gear_ratio nums syms
