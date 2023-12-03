let raw_input = {|1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet|}

let test_part1 () =
  let expected = 142
  and actual = Day1.Part1.run (Day1.Input.of_string raw_input) in

  Alcotest.(check int) "sum of calibration values" expected actual

let () =
  let open Alcotest in
  run "Day1" [ ("part1", [ test_case "Part1" `Quick test_part1 ]) ]
