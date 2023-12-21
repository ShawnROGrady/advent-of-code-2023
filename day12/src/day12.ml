[@@@ocaml.warning "-32"]

let compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c = fun f g x -> f (g x)
let ( << ) = compose

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

  let is_unknown = function Unknown -> true | _ -> false
  let pp : t Fmt.t = Fmt.using as_char Fmt.char
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

module SolutionFinder = struct
  type state = {
    cur_group_size : int;
    remaining_groups : int list;
    remaining_conds : SpringCond.t list;
    resolved : SpringCond.t list;
  }

  let cur_group_size s = s.cur_group_size
  let remaining_groups s = s.remaining_groups
  let remaining_conds s = s.remaining_conds
  let resolved s = s.resolved

  let dump_state : state Fmt.t =
    let open Fmt in
    Dump.record
      [
        Dump.field "cur_group_size" cur_group_size int;
        Dump.field "remaining_conds" remaining_conds (Dump.list SpringCond.pp);
        Dump.field "remaining_groups" remaining_groups (Dump.list int);
        Dump.field "resolved" resolved
          (using (string_of_conds << List.to_seq << List.rev) Dump.string);
      ]

  let add_broken state =
    {
      state with
      resolved = SpringCond.Broken :: state.resolved;
      cur_group_size = Int.succ state.cur_group_size;
    }

  let add_working state =
    {
      state with
      resolved = SpringCond.Working :: state.resolved;
      cur_group_size = 0;
    }

  let rec solutions_of_state = function
    | { remaining_groups = []; remaining_conds = []; cur_group_size = 0; _ }
    | {
        remaining_groups = [];
        remaining_conds = [ SpringCond.Working ];
        cur_group_size = 0;
        _;
      }
    | {
        remaining_groups = [];
        remaining_conds = [ SpringCond.Unknown ];
        cur_group_size = 0;
        _;
      } ->
        1
    | { remaining_groups = []; remaining_conds = SpringCond.Broken :: _; _ } ->
        0
    | { remaining_conds = []; remaining_groups = _ :: _; _ } -> 0
    | {
        remaining_groups = [];
        remaining_conds = SpringCond.Working :: conds;
        cur_group_size;
        _;
      } as state ->
        if cur_group_size = 0 then
          let state' = add_working state in
          solutions_of_state { state' with remaining_conds = conds }
        else 0
    | {
        remaining_groups = [ group ];
        remaining_conds = [ SpringCond.Working ];
        cur_group_size;
        _;
      } ->
        if cur_group_size = group then 1 else 0
    | {
        remaining_groups = [ group ];
        remaining_conds = [ SpringCond.Broken ];
        cur_group_size;
        _;
      } ->
        if group = cur_group_size + 1 then 1 else 0
    | {
        remaining_groups = [ group ];
        remaining_conds = [ SpringCond.Unknown ];
        cur_group_size;
        _;
      } ->
        if cur_group_size = group || group = cur_group_size + 1 then 1 else 0
    | {
        remaining_groups = group :: groups;
        remaining_conds = SpringCond.Working :: conds;
        cur_group_size;
        _;
      } as state ->
        if cur_group_size = 0 then
          let state' = add_working state in
          solutions_of_state { state' with remaining_conds = conds }
        else if cur_group_size <> group then 0
        else
          let state' = add_working state in
          solutions_of_state
            { state' with remaining_groups = groups; remaining_conds = conds }
    | {
        remaining_groups = group :: _;
        remaining_conds = SpringCond.Broken :: conds;
        _;
      } as state ->
        let state' = add_broken state in
        if state'.cur_group_size > group then 0
        else solutions_of_state { state' with remaining_conds = conds }
    | { remaining_conds = SpringCond.Unknown :: conds; _ } as state ->
        let if_broken =
          solutions_of_state
            { state with remaining_conds = SpringCond.Broken :: conds }
        in
        let if_working =
          solutions_of_state
            { state with remaining_conds = SpringCond.Working :: conds }
        in
        if_broken + if_working
    | _ as state ->
        failwith @@ Format.asprintf "unreachable - %a" dump_state state

  let init_state : SpringCond.t list * int list -> state = function
    | [], _ -> failwith "spring conds empty"
    | _, [] -> failwith "expected groups empty"
    | remaining_conds, remaining_groups ->
        { cur_group_size = 0; remaining_conds; remaining_groups; resolved = [] }

  let num_solutions (conds, groups) : int =
    solutions_of_state @@ init_state (conds, groups)
end

module Part1 = struct
  let run : Input.t -> int =
   fun lines ->
    lines
    |> Seq.map (fun (conds, groups) -> (List.of_seq conds, List.of_seq groups))
    |> Seq.map SolutionFinder.num_solutions
    |> Seq.fold_left ( + ) 0
end

module Part2 = struct
  let run _ = failwith "unimplemented"
end
