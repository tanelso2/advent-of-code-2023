open! Base
open! Lib.Day3
open Utils

let test_input = Stdio.In_channel.read_all "../inputs/day3.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example_input = StringUtils.box_trim {|
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
|}

let show_valid_numbers s =
    let (syms, nums) = parse_input s in
    let valid_nums = get_valid_numbers syms nums in
    Stdio.printf "%s\n" @@ [%show: int list] valid_nums

let show_parse_output s =
    let (syms, nums) = parse_input s in
    Stdio.printf "syms: %s\n" @@ [%show: symbol list] syms;
    Stdio.printf "nums: %s\n" @@ [%show: part_number list] nums

let%expect_test "parsing" =
    let example = StringUtils.box_trim {|
        .....
        7.#..
    |} in
    show_parse_output example;
    show_valid_numbers example;
    [%expect {|
      syms: [((2, 1), '#')]
      nums: [((0, 1), (0, 1), 7)]
      [] |}];
    let example = StringUtils.box_trim {|
        84.85
        ..#..
    |} in
    show_valid_numbers example;
    [%expect {| [85; 84] |}];
    let example = StringUtils.box_trim {|
        84.85
        7.#77
    |} in
    show_valid_numbers example;
    [%expect {| [85; 84; 77] |}];
    let example = StringUtils.box_trim {|
        84.85
        7.#77
        64..8
    |} in
    show_valid_numbers example;
    [%expect {| [85; 84; 77; 64] |}]

let%expect_test "part1" =
    Stdio.printf "%d\n" @@ part1 example_input;
    [%expect {| 4361 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 525119 |}]

let%expect_test "part2" =
    Stdio.printf "%d\n" @@ part2 example_input;
    [%expect {| 467835 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 76504829 |}]
