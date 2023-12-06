open! Base
open! Lib.Day5

let test_input = Stdio.In_channel.read_all "../inputs/day5.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example_input = {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4|}

let%expect_test "parsing" =
    let (seeds,maps) = parse_input example_input in
    Stdio.printf "%s\n" @@ [%show: int list] seeds;
    [%expect {| [79; 14; 55; 13] |}];
    let seed_to_soil = maps.(0) in
    let v = IntRangeMap.get_val seed_to_soil 98 in
    Stdio.printf "%d\n" v;
    let v = IntRangeMap.get_val seed_to_soil 99 in
    Stdio.printf "%d\n" v;
    let v = IntRangeMap.get_val seed_to_soil 52 in
    Stdio.printf "%d\n" v;
    [%expect {|
      50
      51
      54 |}];
    let v = MapsArray.seed_to_location maps 79 in
    Stdio.printf "%d\n" v;
    [%expect {| 82 |}];
    let v = MapsArray.seed_to_location maps 14 in
    Stdio.printf "%d\n" v;
    [%expect {| 43 |}];
    let v = MapsArray.seed_to_location maps 55 in
    Stdio.printf "%d\n" v;
    [%expect {| 86 |}];
    let v = MapsArray.seed_to_location maps 13 in
    Stdio.printf "%d\n" v;
    [%expect {| 35 |}];
    ()


let%expect_test "part1" =
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 346433842 |}]

(* let%expect_test "part2" = *)
(*     (match part2' () with *)
(*      | None -> () *)
(*      | Some x -> Stdio.printf "%d\n" x); *)
(*     [%expect {| 60294664 |}] *)
