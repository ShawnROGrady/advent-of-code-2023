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
    full_conds : string;
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

  let memo_rec (fn : (state -> 'b) -> state -> 'b) : state -> 'b =
    (* Inner - accepts state + per-input cache *)
    let rec g input_cache (state : state) =
      let key =
        (state.cur_group_size, state.remaining_groups, state.remaining_conds)
      in

      try Hashtbl.find input_cache key
      with Not_found ->
        let y = fn (g input_cache) state in
        Hashtbl.add input_cache key y;
        y
    in

    (* Outer - accepts state, calls inner w/ state + per-input cache *)
    let tbl = Hashtbl.create 1024 in
    let h (state : state) =
      let key = state.full_conds in
      let input_cache =
        try Hashtbl.find tbl key
        with Not_found ->
          let cache = Hashtbl.create 1024 in
          Hashtbl.add tbl key cache;
          cache
      in

      g input_cache state
    in

    h

  let solutions_of_state =
    memo_rec @@ fun solutions_of_state -> function
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
        Z.one
    | { remaining_groups = []; remaining_conds = SpringCond.Broken :: _; _ } ->
        Z.zero
    | { remaining_conds = []; remaining_groups = _ :: _; _ } -> Z.zero
    | {
        remaining_groups = [];
        remaining_conds = SpringCond.Working :: conds;
        cur_group_size;
        _;
      } as state ->
        if cur_group_size = 0 then
          let state' = add_working state in
          solutions_of_state { state' with remaining_conds = conds }
        else Z.zero
    | {
        remaining_groups = [ group ];
        remaining_conds = [ SpringCond.Working ];
        cur_group_size;
        _;
      } ->
        if cur_group_size = group then Z.one else Z.zero
    | {
        remaining_groups = [ group ];
        remaining_conds = [ SpringCond.Broken ];
        cur_group_size;
        _;
      } ->
        if group = cur_group_size + 1 then Z.one else Z.zero
    | {
        remaining_groups = [ group ];
        remaining_conds = [ SpringCond.Unknown ];
        cur_group_size;
        _;
      } ->
        if cur_group_size = group || group = cur_group_size + 1 then Z.one
        else Z.zero
    | {
        remaining_groups = group :: groups;
        remaining_conds = SpringCond.Working :: conds;
        cur_group_size;
        _;
      } as state ->
        if cur_group_size = 0 then
          let state' = add_working state in
          solutions_of_state { state' with remaining_conds = conds }
        else if cur_group_size <> group then Z.zero
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
        if state'.cur_group_size > group then Z.zero
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
        Z.add if_working if_broken
    | _ as state ->
        (* I truly do believe this is unreachable... *)
        failwith @@ Format.asprintf "unreachable - %a" dump_state state

  let init_state : SpringCond.t list * int list -> state = function
    | [], _ -> failwith "spring conds empty"
    | _, [] -> failwith "expected groups empty"
    | remaining_conds, remaining_groups ->
        {
          cur_group_size = 0;
          remaining_conds;
          remaining_groups;
          resolved = [];
          full_conds = (string_of_conds << List.to_seq) remaining_conds;
        }

  let num_solutions (conds, groups) : Z.t =
    solutions_of_state @@ init_state (conds, groups)
end

module Part1 = struct
  let prepare_conds : SpringCond.t Seq.t -> SpringCond.t list = List.of_seq
  let prepare_counts : int Seq.t -> int list = List.of_seq

  let prepare_input_line ((conds, counts) : Input.line) :
      SpringCond.t list * int list =
    (prepare_conds conds, prepare_counts counts)

  let run : Input.t -> Z.t =
   fun lines ->
    lines
    |> Seq.map prepare_input_line
    |> Seq.map SolutionFinder.num_solutions
    |> Seq.fold_left Z.add Z.zero
end

module Part2 = struct
  let rec ( +: ) a b =
    match a with
    | [] -> b
    | h1 :: [] -> h1 :: b
    | [ h1; h2 ] -> h1 :: h2 :: b
    | h1 :: h2 :: h3 :: tl -> h1 :: h2 :: h3 :: (tl +: b)

  let prepare_conds (conds_seq : SpringCond.t Seq.t) : SpringCond.t list =
    let conds = List.of_seq conds_seq in
    conds
    +: (SpringCond.Unknown :: conds)
    +: (SpringCond.Unknown :: conds)
    +: (SpringCond.Unknown :: conds)
    +: (SpringCond.Unknown :: conds)

  let prepare_counts (counts_seq : int Seq.t) : int list =
    let counts = List.of_seq counts_seq in
    counts +: counts +: counts +: counts +: counts

  let prepare_input_line ((conds, counts) : Input.line) :
      SpringCond.t list * int list =
    (prepare_conds conds, prepare_counts counts)

  let run : Input.t -> Z.t =
   fun lines ->
    lines
    |> Seq.map prepare_input_line
    |> Seq.map SolutionFinder.num_solutions
    |> Seq.fold_left Z.add Z.zero
end
