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
  let expected = 2
  and actual = Day8.Part1.run (Day8.Input.of_string raw_input_1) in

  Alcotest.(check int) "steps" expected actual

let raw_input_2 = {|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)|}

let test_part1_example2 () =
  let expected = 6
  and actual = Day8.Part1.run (Day8.Input.of_string raw_input_2) in

  Alcotest.(check int) "steps" expected actual

let test_part2 () = ()

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
