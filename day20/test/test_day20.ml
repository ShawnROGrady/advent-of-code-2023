let pp_z : Z.t Fmt.t = Fmt.using Z.to_string Fmt.string
let z : Z.t Alcotest.testable = Alcotest.testable pp_z Z.equal

module TestPart1 = struct
  let example1 = {|broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a|}

  let example2 =
    {|broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output|}

  let run_case raw_input expected () =
    let actual = Day20.Part1.run (Day20.Input.of_string raw_input) in

    Alcotest.(check z) "total pulses" (Z.of_int expected) actual

  let make_case (name, (raw_input, expected)) =
    Alcotest.test_case
      (Printf.sprintf "Part1 %s" name)
      `Quick
      (run_case raw_input expected)

  let cases =
    [ ("Example1", (example1, 32000000)); ("Example2", (example2, 11687500)) ]
    |> List.map make_case
end

let () =
  let open Alcotest in
  run "Day20" [ ("part1", TestPart1.cases) ]
