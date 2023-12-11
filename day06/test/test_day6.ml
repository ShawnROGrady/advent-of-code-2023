let raw_input = {|Time:      7  15   30
Distance:  9  40  200|}

let test_part1 () =
  let expected = 288
  and actual = Day6.Part1.run (Day6.Input.of_string raw_input) in

  Alcotest.(check int) "product of ways" expected actual

let test_part2 () = ()

let () =
  let open Alcotest in
  run "Day6"
    [
      ("part1", [ test_case "Part1" `Quick test_part1 ]);
      ("part2", [ test_case "Part2" `Quick test_part2 ]);
    ]
