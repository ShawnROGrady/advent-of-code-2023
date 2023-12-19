let part1_example1_input = {|.....
.S-7.
.|.|.
.L-J.
.....|}

let part1_example2_input = {|..F7.
.FJ|.
SJ.L7
|F--J
LJ...|}

let test_part (run : Day10.Input.t -> int) (msg : string) (example : string)
    (expected : int) () =
  let actual = run (Day10.Input.of_string example) in

  Alcotest.(check int) msg expected actual

let make_part_test (run : Day10.Input.t -> int) (msg : string)
    (name, example, expected) =
  let run_test = test_part run msg example expected in
  Alcotest.test_case name `Quick run_test

let part1_test = make_part_test Day10.Part1.run "steps"

let part1_tests =
  [
    ("Part1 Example1", part1_example1_input, 4);
    ("Part1 Example2", part1_example2_input, 8);
  ]
  |> List.map part1_test

let part2_example1_input =
  {|...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........|}

let part2_example2_input =
  {|..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
..........|}

let part2_example3_input =
  {|.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...|}

let part2_example4_input =
  {|FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L|}

let part2_test = make_part_test Day10.Part2.run "enclosed area"

let part2_tests =
  [
    ("Part2 Example1", part2_example1_input, 4);
    ("Part2 Example2", part2_example2_input, 4);
    ("Part2 Example3", part2_example3_input, 8);
    ("Part2 Example4", part2_example4_input, 10);
  ]
  |> List.map part2_test

let () =
  let open Alcotest in
  run "Day10" [ ("part1", part1_tests); ("part2", part2_tests) ]
