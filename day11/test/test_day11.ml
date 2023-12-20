let raw_input =
  {|...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....|}

let test_part1 () =
  let expected = 374
  and actual = Day11.Part1.run (Day11.Input.of_string raw_input) in

  Alcotest.(check int) "sum of lengths" expected actual

let test_part2 () = Alcotest.skip ()

let () =
  let open Alcotest in
  run "Day11"
    [
      ("part1", [ test_case "Part1" `Quick test_part1 ]);
      ("part2", [ test_case "Part2" `Quick test_part2 ]);
    ]
