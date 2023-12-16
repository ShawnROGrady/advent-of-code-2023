let part1_example1_input = {|.....
.S-7.
.|.|.
.L-J.
.....|}

let test_part1_example1 () =
  let expected = 4
  and actual = Day10.Part1.run (Day10.Input.of_string part1_example1_input) in

  Alcotest.(check int) "steps" expected actual

let part1_example2_input = {|..F7.
.FJ|.
SJ.L7
|F--J
LJ...|}

let test_part1_example2 () =
  let expected = 8
  and actual = Day10.Part1.run (Day10.Input.of_string part1_example2_input) in

  Alcotest.(check int) "steps" expected actual

let test_part2 () = Alcotest.skip ()

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
