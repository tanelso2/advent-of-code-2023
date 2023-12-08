open! Base
open! Lib.Day7

let test_input = Stdio.In_channel.read_all "../inputs/day7.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let%expect_test "comparisons" =
    Stdio.printf "%d\n" @@ compare_hand_type FiveOfAKind FourOfAKind;
    [%expect {| -1 |}];
    Stdio.printf "%s\n" @@ [%show: hand_type] @@ calc_hand_type ['J';'J';'J';'K';'K'];
    [%expect {| Day7.FullHouse |}];
    Stdio.printf "%s\n" @@ [%show: hand_type] @@ calc_hand_type ['J';'J';'J';'A';'K'];
    [%expect {| Day7.ThreeOfAKind |}];
    Stdio.printf "%s\n" @@ [%show: hand_type] @@ calc_hand_type ['J';'J';'J';'J';'K'];
    [%expect {| Day7.FourOfAKind |}];
    Stdio.printf "%s\n" @@ [%show: hand_type] @@ calc_hand_type ['J';'J';'K';'4';'K'];
    [%expect {| Day7.TwoPair |}];
    ()

let example_input = {|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|}

let%expect_test "sorting" =
    Stdio.printf "%s\n" @@ [%show: (char list * int) list] @@ sort_hands @@ parse_hands example_input;
    [%expect {|
      [(['Q'; 'Q'; 'Q'; 'J'; 'A'], 483); (['T'; '5'; '5'; 'J'; '5'], 684);
        (['K'; 'K'; '6'; '7'; '7'], 28); (['K'; 'T'; 'J'; 'J'; 'T'], 220);
        (['3'; '2'; 'T'; '3'; 'K'], 765)] |}];
    Stdio.printf "%d\n" @@ score @@ parse_hands example_input;
    [%expect {| 6440 |}]

let%expect_test "part1" =
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 251287184 |}]

let%expect_test "calc_hand_type2" =
    Stdio.printf "%s\n" @@ [%show: hand_type] @@ calc_hand_type2 ['J';'J';'J';'K';'K'];
    [%expect {| Day7.FiveOfAKind |}];
    Stdio.printf "%s\n" @@ [%show: hand_type] @@ calc_hand_type2 ['K';'T';'J';'J';'T'];
    [%expect {| Day7.FourOfAKind |}];
    ()

let%expect_test "part2" =
    Stdio.printf "%d\n" @@ part2 example_input;
    [%expect {| 5905 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 250757288 |}]
