let pp_z : Z.t Fmt.t = Fmt.using Z.to_string Fmt.string

module Input = struct
  type pattern = char Seq.t Seq.t
  type t = pattern Seq.t

  let sep = Str.regexp "\n\n"

  let of_string (s : string) : t =
    s
    |> Str.split sep
    |> List.to_seq
    |> Seq.map (fun p ->
           p
           |> String.split_on_char '\n'
           |> List.to_seq
           |> Seq.filter (Fun.negate @@ String.equal "")
           |> Seq.map String.to_seq)

  let of_in_channel (ic : in_channel) : t =
    ic |> In_channel.input_all |> of_string
end

(* Helper to before_reflection_point to check whether or not we are at the
   reflection point given the (reversed) previous lines and the remaining
   lines. *)
let rec at_reflection_point = function
  | [], [] | _, [] -> true
  | [], _ -> true
  | prev :: prevs, remaining :: remainings ->
      if not (String.equal prev remaining) then false
      else at_reflection_point (prevs, remainings)

let before_reflection_point (items : string list) : int option =
  let rec loop = function
    | [], [] -> failwith "before_reflection_point with no items"
    | [], item :: items -> loop ([ item ], items)
    | _, [] -> None
    | prev :: prevs, remaining :: remainings ->
        if not (String.equal prev remaining) then
          loop (remaining :: prev :: prevs, remainings)
        else if at_reflection_point (prevs, remainings) then
          Some (Int.succ @@ List.length prevs)
        else loop (remaining :: prev :: prevs, remainings)
  in

  loop ([], items)

let pattern_as_string (pattern : Input.pattern) : string =
  pattern |> Seq.map String.of_seq |> List.of_seq |> String.concat "\n"

module Part1 = struct
  let hundred = Z.of_int 100

  let score_of_pattern (pattern : Input.pattern) : Z.t =
    let rows_above =
      pattern
      |> Seq.map String.of_seq
      |> List.of_seq
      |> before_reflection_point
      |> Option.map Z.of_int
    and cols_to_left =
      pattern
      |> Seq.transpose
      |> Seq.map String.of_seq
      |> List.of_seq
      |> before_reflection_point
      |> Option.map Z.of_int
    in

    match (rows_above, cols_to_left) with
    | None, None ->
        failwith
        @@ Format.asprintf "no reflection point found for:%s"
             (pattern_as_string pattern)
    | Some row, Some col ->
        failwith
        @@ Format.asprintf "found reflection at both row=%a and col=%a for:%s"
             pp_z row pp_z col
             (pattern_as_string pattern)
    | Some rows, None -> Z.mul hundred rows
    | None, Some cols -> cols

  let run : Input.t -> Z.t =
   fun patterns ->
    patterns |> Seq.map score_of_pattern |> Seq.fold_left Z.add Z.zero
end

module Part2 = struct
  let run _ = failwith "unimplemented"
end
