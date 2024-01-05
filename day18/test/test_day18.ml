let raw_input =
  {|R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
|}

let test_part1 () =
  let expected = 62
  and actual = Day18.Part1.run (Day18.Input.of_string raw_input) in

  Alcotest.(check int) "cubic meters" expected actual

let test_part2 () = Alcotest.skip ()

let () =
  let open Alcotest in
  run "Day18"
    [
      ("part1", [ test_case "Part1" `Quick test_part1 ]);
      ("part2", [ test_case "Part2" `Quick test_part2 ]);
    ]
