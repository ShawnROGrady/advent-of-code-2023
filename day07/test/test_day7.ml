let raw_input = {|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|}

let test_part1 () =
  let expected = 6440
  and actual = Day7.Part1.run (Day7.Input.of_string raw_input) in

  Alcotest.(check int) "sum of winnings" expected actual

let test_part2 () =
  let expected = 5905
  and actual = Day7.Part2.run (Day7.Input.of_string raw_input) in

  Alcotest.(check int) "sum of winnings" expected actual

let () =
  let open Alcotest in
  run "Day7"
    [
      ("part1", [ test_case "Part1" `Quick test_part1 ]);
      ("part2", [ test_case "Part2" `Quick test_part2 ]);
    ]
