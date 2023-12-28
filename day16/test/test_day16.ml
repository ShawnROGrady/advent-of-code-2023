let raw_input =
  {|.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....|}

let test_part1 () =
  let expected = 46
  and actual = Day16.Part1.run (Day16.Input.of_string raw_input) in

  Alcotest.(check int) "energized tiles" expected actual

let test_part2 () = Alcotest.skip ()

let () =
  let open Alcotest in
  run "Day16"
    [
      ("part1", [ test_case "Part1" `Quick test_part1 ]);
      ("part2", [ test_case "Part2" `Quick test_part2 ]);
    ]
