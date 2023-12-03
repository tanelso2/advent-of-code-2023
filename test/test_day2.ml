open! Base
open! Lib.Day2

let test_input = Stdio.In_channel.read_all "../inputs/day2.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let%expect_test "parsing" =
    let print_game g =
        Stdio.printf "%s\n" (show_game g);
    in
    let parse_and_show s =
        s |> parse_line |> print_game
    in
    let example = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" in
    parse_and_show example;
    [%expect {|
      { Day2.number = 1;
        draws =
        [[(Day2.Blue, 3); (Day2.Red, 4)];
          [(Day2.Red, 1); (Day2.Green, 2); (Day2.Blue, 6)]; [(Day2.Green, 2)]]
        } |}]

let example_input = {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}

let%expect_test "part1" =
    Stdio.printf "%d\n" (part1 example_input);
    [%expect {| 8 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 2486 |}]

let%expect_test "part2" =
    Stdio.printf "%d\n" (part2 example_input);
    [%expect {| 2286 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 87984 |}]
