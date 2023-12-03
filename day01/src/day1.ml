module Input = struct
  type t = string Seq.t

  let of_in_channel chan =
    (fun () -> try Some (input_line chan) with End_of_file -> None)
    |> Seq.of_dispenser
    |> Seq.memoize

  let of_string s = s |> String.split_on_char '\n' |> List.to_seq
end

let int_of_char_opt c =
  if c >= '0' && c <= '9' then Some (Char.code c - 48) else None

let two_digit x y = (10 * x) + y

module Part1 = struct
  let value_of_line line =
    let nums =
      line |> String.to_seq |> Seq.filter_map int_of_char_opt |> Array.of_seq
    in
    two_digit nums.(0) nums.(Array.length nums - 1)

  let run : Input.t -> int =
   fun lines -> lines |> Seq.map value_of_line |> Seq.fold_left ( + ) 0
end

module Part2 = struct
  let run : Input.t -> int = fun _ -> failwith "unimplemented"
end
