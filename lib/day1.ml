open Utils

let get_digits s =
    let s' = String.to_seq s |> List.of_seq in
    List.filter Base.Char.is_digit s'

let first_digit s =
    List.hd s |> Base.Char.get_digit_exn

let last_digit s =
    s |> List.rev |> List.hd |> Base.Char.get_digit_exn

let get_score s =
    let d = get_digits s in
    let first = first_digit d in
    let last = last_digit d in
    (first * 10) + last

let calc_scores s =
    let lines = Base.String.split_lines s in
    let scores = List.map get_score lines in
    ListUtils.sum scores

let translate (s:char list) : char list =
  let rec helper s acc =
      let continue x =
          helper (List.tl s) (x::acc)
      in
      match s with
      | 'o'::'n'::'e'::_ -> continue '1'
      | 't'::'w'::'o'::_ -> continue '2'
      | 't'::'h'::'r'::'e'::'e'::_ -> continue '3'
      | 'f'::'o'::'u'::'r'::_ -> continue '4'
      | 'f'::'i'::'v'::'e'::_ -> continue '5'
      | 's'::'i'::'x'::_ -> continue '6'
      | 's'::'e'::'v'::'e'::'n'::_ -> continue '7'
      | 'e'::'i'::'g'::'h'::'t'::_ -> continue '8'
      | 'n'::'i'::'n'::'e'::_ -> continue '9'
      | x::_ -> continue x
      | [] -> List.rev acc
  in
  helper s []

let get_digits2 s =
    let s' = String.to_seq s |> List.of_seq in
    let s'' = translate s' in
    List.filter Base.Char.is_digit s''

let get_score2 s =
    let d = get_digits2 s in
    let first = first_digit d in
    let last = last_digit d in
    (first * 10) + last

let calc_scores2 s =
    let lines = Base.String.split_lines s in
    let scores = List.map get_score2 lines in
    ListUtils.sum scores

let part1 (s:string) =
    calc_scores s

let part2 (s:string) =
    calc_scores2 s
