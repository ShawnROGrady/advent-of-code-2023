module type SOLUTION = sig
  val day : int

  module Input : sig
    type t

    val of_in_channel : in_channel -> t
    val of_string : string -> t
  end

  module Answer : sig
    type t

    val to_string : t -> string
  end

  module Part1 : sig
    val run : Input.t -> Answer.t
  end

  module Part2 : sig
    val run : Input.t -> Answer.t
  end
end

module String = struct
  type t = string

  let to_string : t -> string = Fun.id
  let of_string : string -> t = Fun.id
  let of_in_channel : in_channel -> t = In_channel.input_all
end

module Make (Solution : SOLUTION) = struct
  let run_part1 chan =
    chan
    |> Solution.Input.of_in_channel
    |> Solution.Part1.run
    |> Solution.Answer.to_string
    |> print_endline

  let run_part2 chan =
    chan
    |> Solution.Input.of_in_channel
    |> Solution.Part2.run
    |> Solution.Answer.to_string
    |> print_endline

  let run_both chan =
    let input = Solution.Input.of_in_channel chan in
    let a1 = Solution.Part1.run input and a2 = Solution.Part2.run input in

    Printf.printf "part1: %s\npart2: %s\n"
      (Solution.Answer.to_string a1)
      (Solution.Answer.to_string a2)

  let run () =
    let with_input file fn =
      match file with
      | "-" -> fn stdin
      | file -> In_channel.with_open_bin file fn
    in

    let run_fn = ref run_both in
    let usage = Printf.sprintf "day%d [--part PART] FILE" Solution.day in

    let spec_list =
      [
        ( "--part",
          Arg.Symbol
            ( [ "both"; "1"; "2" ],
              function
              | "both" -> run_fn := run_both
              | "1" -> run_fn := run_part1
              | "2" -> run_fn := run_part2
              | other ->
                  raise @@ Arg.Bad (Printf.sprintf "invalid part %s" other) ),
          "The part to run" );
      ]
    in

    let file = ref "-" in
    Arg.parse spec_list (fun anon -> file := anon) usage;

    with_input !file !run_fn
end
