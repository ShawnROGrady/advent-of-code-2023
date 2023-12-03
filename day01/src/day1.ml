module Input = struct
  type t = string Seq.t

  let of_in_channel chan =
    (fun () -> try Some (input_line chan) with End_of_file -> None)
    |> Seq.of_dispenser
    |> Seq.memoize

  let of_string s = s |> String.split_on_char '\n' |> List.to_seq
end

let two_digit x y = (10 * x) + y

module Part1 = struct
  let int_of_char_opt c =
    match c with '0' .. '9' -> Some (Char.code c - 48) | _ -> None

  let nums_of_line line =
    line |> String.to_seq |> Seq.filter_map int_of_char_opt

  let value_of_line line =
    let nums = line |> nums_of_line |> Array.of_seq in
    two_digit nums.(0) nums.(Array.length nums - 1)

  let run : Input.t -> int =
   fun lines -> lines |> Seq.map value_of_line |> Seq.fold_left ( + ) 0
end

module Part2 = struct
  let nums_of_line line =
    let rec loop nums = function
      | [] -> nums
      | ('0' .. '9' as c) :: rest -> loop ((Char.code c - 48) :: nums) rest
      | 'o' :: 'n' :: 'e' :: rest -> loop (1 :: nums) ('e' :: rest)
      | 't' :: 'w' :: 'o' :: rest -> loop (2 :: nums) ('o' :: rest)
      | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: rest -> loop (3 :: nums) ('e' :: rest)
      | 'f' :: 'o' :: 'u' :: 'r' :: rest -> loop (4 :: nums) rest
      | 'f' :: 'i' :: 'v' :: 'e' :: rest -> loop (5 :: nums) ('e' :: rest)
      | 's' :: 'i' :: 'x' :: rest -> loop (6 :: nums) rest
      | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: rest -> loop (7 :: nums) ('n' :: rest)
      | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: rest -> loop (8 :: nums) ('t' :: rest)
      | 'n' :: 'i' :: 'n' :: 'e' :: rest -> loop (9 :: nums) ('e' :: rest)
      | _ :: rest -> loop nums rest
    in

    loop [] (List.of_seq @@ String.to_seq line)

  let value_of_line line =
    let nums = line |> nums_of_line |> Array.of_list in
    two_digit nums.(Array.length nums - 1) nums.(0)

  let run : Input.t -> int =
   fun lines -> lines |> Seq.map value_of_line |> Seq.fold_left ( + ) 0
end
