let raw_input =
  {|???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1|}

let pp_z : Z.t Fmt.t = Fmt.using Z.to_string Fmt.string
let z : Z.t Alcotest.testable = Alcotest.testable pp_z Z.equal

let test_part1 () =
  let expected = Z.of_int 21
  and actual = Day12.Part1.run (Day12.Input.of_string raw_input) in

  Alcotest.(check z) "sum of arrangements" expected actual

let test_part2 () =
  let expected = Z.of_int 525152
  and actual = Day12.Part2.run (Day12.Input.of_string raw_input) in

  Alcotest.(check z) "sum of arrangements" expected actual

let () =
  let open Alcotest in
  run "Day12"
    [
      ("part1", [ test_case "Part1" `Quick test_part1 ]);
      ("part2", [ test_case "Part2" `Quick test_part2 ]);
    ]
