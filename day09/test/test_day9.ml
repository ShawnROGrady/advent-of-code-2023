let raw_input = {|0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45|}

let pp_z : Z.t Fmt.t = Fmt.using Z.to_string Fmt.string
let z : Z.t Alcotest.testable = Alcotest.testable pp_z Z.equal

let test_part1 () =
  let expected = Z.of_int 114
  and actual = Day9.Part1.run (Day9.Input.of_string raw_input) in

  Alcotest.(check z) "sum of extrapolated" expected actual

let test_part2 () =
  let expected = Z.of_int 2
  and actual = Day9.Part2.run (Day9.Input.of_string raw_input) in

  Alcotest.(check z) "sum of extrapolated" expected actual

let () =
  let open Alcotest in
  run "Day9"
    [
      ("part1", [ test_case "Part1" `Quick test_part1 ]);
      ("part2", [ test_case "Part2" `Quick test_part2 ]);
    ]
