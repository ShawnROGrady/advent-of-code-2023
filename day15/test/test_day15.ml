let raw_input = {|rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7|}

let test_part1 () =
  let expected = 1320
  and actual = Day15.Part1.run (Day15.Input.of_string raw_input) in

  Alcotest.(check int) "sum of codes" expected actual

let test_part2 () =
  let expected = 145
  and actual = Day15.Part2.run (Day15.Input.of_string raw_input) in

  Alcotest.(check int) "focusing power" expected actual

let () =
  let open Alcotest in
  run "Day15"
    [
      ("part1", [ test_case "Part1" `Quick test_part1 ]);
      ("part2", [ test_case "Part2" `Quick test_part2 ]);
    ]
