[@@@ocaml.warning "-32"]

module SpringCond = struct
  type t = Working | Broken | Unknown

  let of_char = function
    | '.' -> Working
    | '#' -> Broken
    | '?' -> Unknown
    | other -> failwith @@ Printf.sprintf "invalid spring condition %c" other

  let as_char = function Working -> '.' | Broken -> '#' | Unknown -> '?'

  let as_bool_opt = function
    | Working -> Some true
    | Broken -> Some false
    | Unknown -> None

  let as_bool cond =
    match as_bool_opt cond with
    | Some b -> b
    | None -> failwith "SpringCond.as_bool"
end

let conds_of_string (s : string) : SpringCond.t Seq.t =
  s |> String.to_seq |> Seq.map SpringCond.of_char

let string_of_conds (conds : SpringCond.t Seq.t) : string =
  conds |> Seq.map SpringCond.as_char |> String.of_seq

module Input = struct
  type line = SpringCond.t Seq.t * int Seq.t
  type t = line Seq.t

  let parse_line (s : string) : line =
    let conds_part, groups_part =
      match String.split_on_char ' ' s with
      | [ a; b ] -> (a, b)
      | other ->
          failwith
          @@ Fmt.str "cannot parse parts: %a" Fmt.Dump.(list string) other
    in

    let conds = conds_part |> conds_of_string
    and groups =
      groups_part
      |> String.split_on_char ','
      |> List.to_seq
      |> Seq.map int_of_string
    in

    (conds, groups)

  let of_in_channel (chan : in_channel) : t =
    (fun () ->
      try Some (parse_line @@ input_line chan) with End_of_file -> None)
    |> Seq.of_dispenser
    |> Seq.memoize

  let of_string (s : string) : t =
    s |> String.split_on_char '\n' |> List.to_seq |> Seq.map parse_line
end

module IntSet = Set.Make (Int)

module Part1 = struct
  let indices_of_unknowns (conds : SpringCond.t Seq.t) : int Seq.t =
    conds
    |> Seq.fold_lefti
         (fun idxs i cond ->
           match cond with SpringCond.Unknown -> Seq.cons i idxs | _ -> idxs)
         Seq.empty

  let all_potential_solutions (conds : SpringCond.t Seq.t) :
      SpringCond.t Seq.t Seq.t =
    let rec loop prevs seq =
      match seq () with
      | Seq.Nil -> prevs |> Seq.map (fun prev -> List.to_seq @@ List.rev prev)
      | Seq.Cons (SpringCond.Unknown, rest) ->
          let prevs' =
            if Seq.is_empty prevs then
              Seq.cons [ SpringCond.Working ]
              @@ Seq.cons [ SpringCond.Broken ]
              @@ Seq.empty
            else
              prevs
              |> Seq.fold_left
                   (fun prevs prev ->
                     Seq.cons (SpringCond.Working :: prev)
                     @@ Seq.cons (SpringCond.Broken :: prev) prevs)
                   Seq.empty
          in
          loop prevs' rest
      | Seq.Cons (cond, rest) ->
          let prevs' =
            if Seq.is_empty prevs then Seq.return [ cond ]
            else prevs |> Seq.map (List.cons cond)
          in
          loop prevs' rest
    in

    loop Seq.empty conds

  let groups_of_broken (conds : SpringCond.t Seq.t) : int list =
    let groups, last_group =
      conds
      |> Seq.map SpringCond.as_bool
      |> Seq.fold_left
           (fun (groups, cur_group) is_working ->
             let is_broken = not is_working in

             if is_broken then (groups, cur_group + 1)
             else if not (Int.equal cur_group 0) then (cur_group :: groups, 0)
             else (groups, 0))
           ([], 0)
    in

    if not (Int.equal 0 last_group) then List.rev (last_group :: groups)
    else List.rev groups

  let is_valid_solution
      ((conds, expected_groups) : SpringCond.t Seq.t * int list) : bool =
    let actual_groups = groups_of_broken conds in
    List.equal Int.equal expected_groups actual_groups

  let valid_solution_count ((conds, groups) : SpringCond.t Seq.t * int Seq.t) :
      int =
    let expected_groups = List.of_seq groups in
    conds
    |> all_potential_solutions
    |> Seq.filter (fun c ->
           let is_valid = is_valid_solution (c, expected_groups) in

           is_valid)
    |> Seq.fold_left (fun count _ -> Int.succ count) 0

  let run : Input.t -> int =
   fun lines -> lines |> Seq.map valid_solution_count |> Seq.fold_left ( + ) 0
end

module Part2 = struct
  let run _ = failwith "unimplemented"
end
