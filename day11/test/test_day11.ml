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

let pp_z : Z.t Fmt.t = Fmt.using Z.to_string Fmt.string
let z : Z.t Alcotest.testable = Alcotest.testable pp_z Z.equal

let test_part1 () =
  let expected = Z.of_int 374
  and actual = Day11.Part1.run (Day11.Input.of_string raw_input) in

  Alcotest.(check z) "sum of lengths" expected actual

let part2_test empty_scale expected () =
  let actual = Day11.Part2.run ~empty_scale (Day11.Input.of_string raw_input) in

  Alcotest.(check z) "sum of lengths" (Z.of_int expected) actual

let part2_test_case empty_scale expected =
  Alcotest.test_case
    (Printf.sprintf "Part2 scale=%d" empty_scale)
    `Quick
    (part2_test empty_scale expected)

let part2_tests =
  List.map
    (fun (empty_scale, expected) -> part2_test_case empty_scale expected)
    [ (10, 1030); (100, 8410) ]

let () =
  let open Alcotest in
  run "Day11"
    [
      ("part1", [ test_case "Part1" `Quick test_part1 ]); ("part2", part2_tests);
    ]
