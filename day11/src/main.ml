module Main = struct
  let day = 11

  module Answer = Z
  module Solution = Day11

  let run_part1 chan =
    chan
    |> Solution.Input.of_in_channel
    |> Solution.Part1.run
    |> Answer.to_string
    |> print_endline

  let run_part2 ?empty_scale chan =
    chan
    |> Solution.Input.of_in_channel
    |> Solution.Part2.run ?empty_scale
    |> Answer.to_string
    |> print_endline

  let run_both ?empty_scale chan =
    let input = Solution.Input.of_in_channel chan in
    let a1 = Solution.Part1.run input
    and a2 = Solution.Part2.run ?empty_scale input in

    Printf.printf "part1: %s\npart2: %s\n" (Answer.to_string a1)
      (Answer.to_string a2)

  type part_to_run = One | Two | Both

  let run () =
    let with_input file fn =
      match file with
      | "-" -> fn stdin
      | file -> In_channel.with_open_bin file fn
    in

    let to_run = ref Both and empty_scale : int ref = ref 1000000 in
    let usage = Printf.sprintf "day%d [--part PART] [--scale SCALE] FILE" day in

    let spec_list =
      [
        ( "--part",
          Arg.Symbol
            ( [ "both"; "1"; "2" ],
              function
              | "both" -> to_run := Both
              | "1" -> to_run := One
              | "2" -> to_run := Two
              | other ->
                  raise @@ Arg.Bad (Printf.sprintf "invalid part %s" other) ),
          "The part to run" );
        ( "--scale",
          Arg.Set_int empty_scale,
          "For part2, the scale of empty rows/cols" );
      ]
    in

    let file = ref "-" in
    Arg.parse spec_list (fun anon -> file := anon) usage;

    with_input !file
      (match !to_run with
      | One -> run_part1
      | Two -> run_part2 ~empty_scale:!empty_scale
      | Both -> run_both ~empty_scale:!empty_scale)
end

let () = Main.run ()
