open! Base
open! Lib.Day6

let test_input = Stdio.In_channel.read_all "../inputs/day6.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let%expect_test "ways_of_winning" =
    Stdio.printf "%d\n" @@ how_many_ways_of_winning 7 9;
    [%expect {| 4 |}];
    Stdio.printf "%d\n" @@ how_many_ways_of_winning 15 40;
    [%expect {| 8 |}];
    Stdio.printf "%d\n" @@ how_many_ways_of_winning 30 200;
    [%expect {| 9 |}];
    ()

let example_input = {|Time:      7  15   30
Distance:  9  40  200|}


let%expect_test "part1" =
    Stdio.printf "%d\n" @@ part1 example_input;
    [%expect {| 288 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 114400 |}]

let%expect_test "part2" =
    Stdio.printf "%d\n" @@ part2 example_input;
    [%expect {| 71503 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 21039729 |}]
