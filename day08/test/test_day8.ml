let raw_input_1 =
  {|RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)|}

let test_part1_example1 () =
  let expected = 2L
  and actual = Day8.Part1.run (Day8.Input.of_string raw_input_1) in

  Alcotest.(check int64) "steps" expected actual

let raw_input_2 = {|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)|}

let test_part1_example2 () =
  let expected = 6L
  and actual = Day8.Part1.run (Day8.Input.of_string raw_input_2) in

  Alcotest.(check int64) "steps" expected actual

let raw_input_3 =
  {|LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)|}

let test_part2 () =
  let expected = 6L
  and actual = Day8.Part2.run (Day8.Input.of_string raw_input_3) in

  Alcotest.(check int64) "steps" expected actual

let () =
  let open Alcotest in
  run "Day8"
    [
      ( "part1",
        [
          test_case "Part1 Example1" `Quick test_part1_example1;
          test_case "Part1 Example2" `Quick test_part1_example2;
        ] );
      ("part2", [ test_case "Part2" `Quick test_part2 ]);
    ]
