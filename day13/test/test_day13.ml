let raw_input =
  {|#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#|}

let pp_z : Z.t Fmt.t = Fmt.using Z.to_string Fmt.string
let z : Z.t Alcotest.testable = Alcotest.testable pp_z Z.equal

let test_part1 () =
  let expected = Z.of_int 405
  and actual = Day13.Part1.run (Day13.Input.of_string raw_input) in

  Alcotest.(check z) "summary" expected actual

let test_part2 () = Alcotest.skip ()

let () =
  let open Alcotest in
  run "Day13"
    [
      ("part1", [ test_case "Part1" `Quick test_part1 ]);
      ("part2", [ test_case "Part2" `Quick test_part2 ]);
    ]
