open! Base
open! Lib.Day1

let test_input = Stdio.In_channel.read_all "../inputs/day1.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let%expect_test "get_score" =
    let show_score s =
        Stdio.printf "%d\n" (get_score s)
    in
    show_score "1abc2";
    [%expect {| 12 |}];
    show_score "pqr3stu8vwx";
    [%expect {| 38 |}];
    show_score "a1b2c3d4e5f";
    [%expect {| 15 |}]

let%expect_test "get_score2" =
    let show_score s =
        Stdio.printf "%d\n" (get_score2 s)
    in
    show_score "two1nine";
    [%expect {| 29 |}];
    show_score "eightwothree";
    [%expect {| 83 |}];
    show_score "7pqrstsixteen";
    [%expect {| 76 |}];
    show_score "zoneight234";
    [%expect {| 14 |}];
    show_score "abcone2threexyz";
    [%expect {| 13 |}];
    show_score "xtwoone3four";
    [%expect {| 24 |}];
    show_score "eighthree";
    [%expect {| 83 |}];
    show_score "sevenine";
    [%expect {| 79 |}];
    let sample_string = {|two1nine
      eightwothree
      abcone2threexyz
      xtwone3four
      4nineeightseven2
      zoneight234
      7pqrstsixteen|} in
    Stdio.printf "%d\n" (calc_scores2 sample_string);
    [%expect {| 281 |}]




let%expect_test "part1" =
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 54601 |}]

let%expect_test "part2" =
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 54078 |}]
