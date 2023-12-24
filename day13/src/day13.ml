let pp_z : Z.t Fmt.t = Fmt.using Z.to_string Fmt.string

module Pattern = struct
  type t = char Seq.t Seq.t

  let as_string (pattern : t) : string =
    pattern |> Seq.map String.of_seq |> List.of_seq |> String.concat "\n"

  let of_string (s : string) : t =
    s
    |> String.split_on_char '\n'
    |> List.to_seq
    |> Seq.filter (Fun.negate @@ String.equal "")
    |> Seq.map String.to_seq

  let hundred = Z.of_int 100

  let summary (before_reflection_point : string list -> int option)
      (pattern : t) : Z.t =
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
             (as_string pattern)
    | Some row, Some col ->
        failwith
        @@ Format.asprintf "found reflection at both row=%a and col=%a for:%s"
             pp_z row pp_z col (as_string pattern)
    | Some rows, None -> Z.mul hundred rows
    | None, Some cols -> cols
end

module Input = struct
  type t = Pattern.t Seq.t

  let sep = Str.regexp "\n\n"

  let of_string (s : string) : t =
    s |> Str.split sep |> List.to_seq |> Seq.map Pattern.of_string

  let of_in_channel (ic : in_channel) : t =
    ic |> In_channel.input_all |> of_string
end

module Part1 = struct
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

  let summary_of_pattern : Pattern.t -> Z.t =
    Pattern.summary before_reflection_point

  let run : Input.t -> Z.t =
   fun patterns ->
    patterns |> Seq.map summary_of_pattern |> Seq.fold_left Z.add Z.zero
end

let one_element_off (eq : 'a -> 'a -> bool) (xs : 'a Seq.t) (ys : 'a Seq.t) :
    bool =
  let rec loop count (xs, ys) =
    match (xs (), ys ()) with
    | Seq.Nil, Seq.Nil -> Int.equal 1 count
    | Seq.Nil, _ | _, Seq.Nil -> false
    | Seq.Cons (x, xs'), Seq.Cons (y, ys') ->
        let count' = count + if eq x y then 0 else 1 in
        if count' > 1 then false else loop count' (xs', ys')
  in

  loop 0 (xs, ys)

module Part2 = struct
  let one_element_off (a : string) (b : string) : bool =
    one_element_off Char.equal (String.to_seq a) (String.to_seq b)

  (* Helper to before_reflection_point to check whether or not we are at the
     reflection point given the (reversed) previous lines, the remaining
     lines, and the current count of differing elements. *)
  let rec at_reflection_point (diff_count : int) = function
    | [], [] | _, [] | [], _ -> Int.equal diff_count 1
    | prev :: prevs, remaining :: remainings ->
        if String.equal prev remaining then
          at_reflection_point diff_count (prevs, remainings)
        else if Int.equal 0 diff_count && one_element_off prev remaining then
          at_reflection_point 1 (prevs, remainings)
        else false

  let before_reflection_point (items : string list) : int option =
    let rec loop = function
      | [], [] -> failwith "before_reflection_point with no items"
      | [], item :: items -> loop ([ item ], items)
      | _, [] -> None
      | prev :: prevs, remaining :: remainings ->
          if
            String.equal prev remaining
            && at_reflection_point 0 (prevs, remainings)
            || one_element_off prev remaining
               && at_reflection_point 1 (prevs, remainings)
          then Some (Int.succ @@ List.length prevs)
          else loop (remaining :: prev :: prevs, remainings)
    in

    loop ([], items)

  let summary_of_pattern : Pattern.t -> Z.t =
    Pattern.summary before_reflection_point

  let run : Input.t -> Z.t =
   fun patterns ->
    patterns |> Seq.map summary_of_pattern |> Seq.fold_left Z.add Z.zero
end
