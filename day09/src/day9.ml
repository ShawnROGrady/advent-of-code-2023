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

let differences : Z.t list -> Z.t list =
  let rec loop cur = function
    | [] | [ _ ] -> List.rev cur
    | x :: y :: rest ->
        let d = Z.sub y x in
        let cur' = d :: cur in
        loop cur' (y :: rest)
  in

  loop []

let rec last_of_list = function
  | [] -> failwith "last_of_list"
  | [ x ] -> x
  | _ :: xs -> last_of_list xs

let actual_last : 'a list -> 'a = last_of_list

let implied_last : Z.t list -> Z.t =
  let rec loop cur xs =
    if all_zero xs then cur
    else
      let cur' = Z.(cur + actual_last xs) and xs' = differences xs in
      loop cur' xs'
  in

  loop Z.zero

module Part1 = struct
  let run : Input.t -> Z.t =
   fun lines ->
    lines
    |> Seq.map (fun nums -> implied_last (List.of_seq nums))
    |> Seq.fold_left Z.( + ) Z.zero
end

let actual_first : 'a list -> 'a = List.hd

let rec implied_first (xs : Z.t list) : Z.t =
  if all_zero xs then Z.zero
  else
    let xs' = differences xs in
    Z.sub (actual_first xs) (implied_first xs')

module Part2 = struct
  let run : Input.t -> Z.t =
   fun lines ->
    lines
    |> Seq.map (fun nums -> implied_first (List.of_seq nums))
    |> Seq.fold_left Z.( + ) Z.zero
end
