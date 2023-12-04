let raw_input =
  {|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|}

let test_part1 () =
  let expected = 4361
  and actual = Day3.Part1.run (Day3.Input.of_string raw_input) in

  Alcotest.(check int) "sum of parts" expected actual

let test_part2 () = ()

let () =
  let open Alcotest in
  run "Day3"
    [
      ("part1", [ test_case "Part1" `Quick test_part1 ]);
      ("part2", [ test_case "Part2" `Quick test_part2 ]);
    ]
