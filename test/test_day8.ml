open! Base
open! Lib.Day8
open Utils

let test_input = Stdio.In_channel.read_all "../inputs/day8.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example_input = {|RL

    AAA = (BBB, CCC)
    BBB = (DDD, EEE)
    CCC = (ZZZ, GGG)
    DDD = (DDD, DDD)
    EEE = (EEE, EEE)
    GGG = (GGG, GGG)
    ZZZ = (ZZZ, ZZZ)
|}

let%expect_test "parsing" =
    Stdio.printf "%s\n" @@ [%show: (string * (string * string))] @@ parse_tunnel_line "ZZZ = (AAA, BBB)";
    [%expect {| ("ZZZ", ("AAA", "BBB")) |}];
    let (dirs,tunnels) = parse_input example_input in
    let (x,y) = get_tunnel tunnels "BBB" in
    Stdio.printf "%s\n" @@ [%show: (string * string)] (x,y);
    [%expect {| ("DDD", "EEE") |}];
    let (x,y) = get_tunnel tunnels "ZZZ" in
    Stdio.printf "%s\n" @@ [%show: (string * string)] (x,y);
    [%expect {| ("ZZZ", "ZZZ") |}];
    Stdio.printf "%s\n" @@ [%show: char] @@ RingBuffer.read_and_shift dirs;
    Stdio.printf "%s\n" @@ [%show: char] @@ RingBuffer.read_and_shift dirs;
    Stdio.printf "%s\n" @@ [%show: char] @@ RingBuffer.read_and_shift dirs;
    Stdio.printf "%s\n" @@ [%show: char] @@ RingBuffer.read_and_shift dirs;
    [%expect {|
      'R'
      'L'
      'R'
      'L' |}];
    ()

let example_input2 = {|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)|}

let%expect_test "part1" =
    Stdio.printf "%d\n" @@ part1 example_input;
    [%expect {| 2 |}];
    Stdio.printf "%d\n" @@ part1 example_input2;
    [%expect {| 6 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 17263 |}]

let%test_unit "part2_nodes" =
    [%test_eq: bool] true (is_starting_node "BXA");
    [%test_eq: bool] false (is_starting_node "BXB");
    [%test_eq: bool] false (is_starting_node "BXZ");
    [%test_eq: bool] false (is_starting_node "KBZ");
    [%test_eq: bool] true (is_ending_node "BXZ");
    [%test_eq: bool] true (is_ending_node "KBZ");
    ()

let example_input3 = {|LR

AAA = (AAB, XXX)
AAB = (XXX, AAZ)
AAZ = (AAB, XXX)
BBA = (BBB, XXX)
BBB = (BBC, BBC)
BBC = (BBZ, BBZ)
BBZ = (BBB, BBB)
XXX = (XXX, XXX)|}

let%expect_test "part2" =
    let (_d,m) = parse_input example_input3 in
    Stdio.printf "%s\n" @@ [%show: string list] @@ get_starting_nodes m;
    [%expect {| |}];
    Stdio.printf "%d\n" @@ part2 example_input3;
    [%expect {| |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| |}];
    ()
