open! Base
open! Lib.Day4
open Utils

let test_input = Stdio.In_channel.read_all "../inputs/day4.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example_input = StringUtils.box_trim {| 
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
|}

let%expect_test "parsing" =
    let cards = parse_input example_input in
    let card1 = List.hd_exn cards in
    Stdio.printf "%s\n" @@ [%show: (int list * int list)] card1;
    [%expect {| ([41; 48; 83; 86; 17], [83; 86; 6; 31; 17; 9; 48; 53]) |}];
    let score = score_card card1 in
    Stdio.printf "%d\n" score;
    [%expect {| 8 |}]

let%expect_test "part1" =
    let _ = part1 test_input in
    Stdio.printf "%d\n" (part1 example_input);
    [%expect {| 13 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 33950 |}]

let%expect_test "part2" =
    Stdio.printf "%d\n" (part2 example_input);
    [%expect {| 30 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 14814534 |}]
