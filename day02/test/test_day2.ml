let raw_input =
  {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}

let test_part1 () =
  let expected = 8
  and actual = Day2.Part1.run (Day2.Input.of_string raw_input) in

  Alcotest.(check int) "sum of possible games" expected actual

let test_part2 () =
  let expected = 2286
  and actual = Day2.Part2.run (Day2.Input.of_string raw_input) in

  Alcotest.(check int) "sum of powers" expected actual

let () =
  let open Alcotest in
  run "Day2"
    [
      ("part1", [ test_case "Part1" `Quick test_part1 ]);
      ("part2", [ test_case "Part2" `Quick test_part2 ]);
    ]
