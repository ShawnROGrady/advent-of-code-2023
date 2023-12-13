module Input = struct
  type t = Z.t Seq.t Seq.t

  let parse_line (line : string) : Z.t Seq.t =
    line |> String.split_on_char ' ' |> List.to_seq |> Seq.map Z.of_string

  let of_in_channel (chan : in_channel) : t =
    (fun () ->
      try Some (parse_line @@ input_line chan) with End_of_file -> None)
    |> Seq.of_dispenser
    |> Seq.memoize

  let of_string (s : string) : t =
    s |> String.split_on_char '\n' |> List.to_seq |> Seq.map parse_line
end

let all_zero : Z.t list -> bool = function
  | [] -> failwith "all_zero with empty list"
  | xs -> List.for_all (Z.equal Z.zero) xs

let differences ~(reversed : bool) : Z.t list -> Z.t list =
  let rec loop cur = function
    | [] | [ _ ] -> cur
    | x :: y :: rest ->
        let d = if reversed then Z.sub x y else Z.sub y x in
        let cur' = d :: cur in
        loop cur' (y :: rest)
  in

  loop []

let actual_last ~(reversed : bool) xs =
  if reversed then List.hd xs else List.hd (List.rev xs)

let implied_last : Z.t list -> Z.t =
  let rec loop reversed cur xs =
    if all_zero xs then cur
    else
      let cur' = Z.(cur + actual_last ~reversed xs)
      and xs' = differences ~reversed xs in
      loop (not reversed) cur' xs'
  in

  loop false Z.zero

module Part1 = struct
  let run : Input.t -> Z.t =
   fun lines ->
    lines
    |> Seq.map (fun nums -> implied_last (List.of_seq nums))
    |> Seq.fold_left Z.( + ) Z.zero
end

module Part2 = struct
  let run _ = failwith "unimplemented"
end
