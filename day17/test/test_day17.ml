let raw_input =
  {|2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533|}

let pp_z : Z.t Fmt.t = Fmt.using Z.to_string Fmt.string
let z : Z.t Alcotest.testable = Alcotest.testable pp_z Z.equal

let test_part1 () =
  let expected = Z.of_int 102
  and actual = Day17.Part1.run (Day17.Input.of_string raw_input) in

  Alcotest.(check z) "minimal heat loss" expected actual

let part2_example2 =
  {|111111111111
999999999991
999999999991
999999999991
999999999991|}

let part2_test input expected () =
  let actual = Day17.Part2.run input in
  Alcotest.(check z) "minimal heat loss" (Z.of_int expected) actual

let part2_test_case name input expected =
  Alcotest.test_case
    (Printf.sprintf "Part2 %s" name)
    `Quick
    (part2_test input expected)

let part2_tests =
  [ ("Example1", raw_input, 94); ("Example2", part2_example2, 71) ]

let part2_test_cases =
  part2_tests
  |> List.map (fun (name, input_str, expected) ->
         part2_test_case name (Day17.Input.of_string input_str) expected)

let () =
  let open Alcotest in
  run "Day17"
    [
      ("part1", [ test_case "Part1" `Quick test_part1 ]);
      ("part2", part2_test_cases);
    ]
