let range ?(start = 0) stop = start |> Seq.ints |> Seq.take (stop - start)

module Direction = struct
  type t = Up | Down | Left | Right

  let[@inline] as_int = function Up -> 0 | Down -> 1 | Left -> 2 | Right -> 3
  let[@warning "-32"] compare a b : int = Int.compare (as_int a) (as_int b)
  let all = [ Up; Down; Left; Right ]

  let of_char = function
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right
    | other -> invalid_arg @@ Fmt.str "unrecognized direction '%c'" other

  let pp : t Fmt.t =
    Fmt.string
    |> Fmt.using (function
         | Up -> "Up"
         | Down -> "Down"
         | Left -> "Left"
         | Right -> "Right")
end

module Point = struct
  type t = int * int

  let x : t -> int = fst
  let y : t -> int = snd
  let origin : t = (0, 0)

  let[@inline] compare a b : int =
    match Int.compare (x a) (x b) with
    | 0 -> Int.compare (y a) (y b)
    | other -> other

  let[@inline] add (a : t) (b : t) : t = (x a + x b, y a + y b)

  let[@inline] of_direction : Direction.t -> t = function
    | Direction.Up -> (0, 1)
    | Direction.Down -> (0, -1)
    | Direction.Left -> (-1, 0)
    | Direction.Right -> (1, 0)

  let move : Direction.t -> t -> t = fun d -> add (of_direction d)
  let[@warning "-32"] pp : t Fmt.t = Fmt.Dump.pair Fmt.int Fmt.int
end

module Instruction = struct
  type t = { dir : Direction.t; steps : int; color : string }

  let make ~dir ~steps ~color = { dir; steps; color }
  let dir inst = inst.dir
  let steps inst = inst.steps
  let color inst = inst.color

  let pp : t Fmt.t =
    let open Fmt in
    Dump.record
      [
        Dump.field "dir" dir Direction.pp;
        Dump.field "steps" steps int;
        Dump.field "color" color Dump.string;
      ]
end

module Input = struct
  type t = Instruction.t Seq.t

  let pp : t Fmt.t = Fmt.Dump.seq Instruction.pp

  let scan_instruction (ic : Scanf.Scanning.in_channel) : Instruction.t =
    Scanf.bscanf ic "%c %d (#%s@)\n" (fun dir_c steps color ->
        Instruction.make ~dir:(Direction.of_char dir_c) ~steps ~color)

  let scan_instructions (ic : Scanf.Scanning.in_channel) : Instruction.t Seq.t =
    let f () = try Some (scan_instruction ic) with End_of_file -> None in
    Seq.memoize @@ Seq.of_dispenser f

  let of_in_channel (ic : in_channel) : t =
    scan_instructions (Scanf.Scanning.from_channel ic)

  let of_string (s : string) : t =
    scan_instructions (Scanf.Scanning.from_string s)
end

module PointSet = Set.Make (Point)

module Bounds = struct
  type t = { min_x : int; max_x : int; min_y : int; max_y : int }

  let of_seq (points : Point.t Seq.t) : t =
    let min_x, max_x, min_y, max_y =
      points
      |> Seq.fold_left
           (fun (min_x, max_x, min_y, max_y) (x, y) ->
             Int.(min x min_x, max x max_x, min y min_y, max y max_y))
           (Int.max_int, 0, Int.max_int, 0)
    in

    { min_x; max_x; min_y; max_y }

  let min_x bounds = bounds.min_x
  let min_y bounds = bounds.min_y
  let max_x bounds = bounds.max_x
  let max_y bounds = bounds.max_y

  let[@warning "-32"] pp : t Fmt.t =
    let open Fmt in
    let open Fmt.Dump in
    record
      [
        field "min_x" min_x int;
        field "max_x" max_x int;
        field "min_y" min_y int;
        field "max_y" max_y int;
      ]
end

module Part1 = struct
  let[@warning "-32"] draw_trench (trench : PointSet.t) : string =
    let { Bounds.min_x; max_x; min_y; max_y } =
      trench |> PointSet.to_seq |> Bounds.of_seq
    in

    let normalize_y y = y - min_y
    and normalize_x x = x - min_x
    and dim_y = max_y - min_y + 2
    and dim_x = max_x - min_x + 2 in

    let arr = Array.init dim_y (fun _ -> Array.make dim_x '.') in

    trench
    |> PointSet.iter (fun (x, y) ->
           let normalized = (normalize_x x, normalize_y y) in
           arr.(Point.y normalized).(Point.x normalized) <- '#');

    arr
    |> Array.to_list
    |> List.map (fun row -> row |> Array.to_seq |> String.of_seq)
    |> List.rev
    |> String.concat "\n"

  type dig_state = { trench_edges : PointSet.t; pos : Point.t }

  let init_state =
    { trench_edges = PointSet.singleton Point.origin; pos = Point.origin }

  let trench_edges state = state.trench_edges

  let advance_trench (dir : Direction.t) (state : dig_state) : dig_state =
    let next_pos = Point.move dir state.pos in
    { trench_edges = PointSet.add next_pos state.trench_edges; pos = next_pos }

  let dig (state : dig_state) (inst : Instruction.t) =
    inst
    |> Instruction.steps
    |> range
    |> Seq.fold_left
         (fun cur _ -> advance_trench (Instruction.dir inst) cur)
         state

  let rec points_outside_trench_aux (in_bounds : Point.t -> bool)
      (edges : PointSet.t) (visited : PointSet.t) = function
    | [] -> visited
    | point :: rest when PointSet.mem point visited ->
        points_outside_trench_aux in_bounds edges visited rest
    | point :: rest ->
        let visited' = PointSet.add point visited
        and to_check =
          Direction.all
          |> List.fold_left
               (fun to_check d ->
                 let p = Point.move d point in
                 if
                   in_bounds p
                   && (not (PointSet.mem p edges))
                   && not (PointSet.mem p visited)
                 then p :: to_check
                 else to_check)
               rest
        in
        points_outside_trench_aux in_bounds edges visited' to_check

  let points_outside_trench (in_bounds : Point.t -> bool) (edges : PointSet.t)
      (start : Point.t) : PointSet.t =
    points_outside_trench_aux in_bounds edges PointSet.empty [ start ]

  let run : Input.t -> Z.t =
   fun insts ->
    let edges = insts |> Seq.fold_left dig init_state |> trench_edges in

    let bounds : Bounds.t = Bounds.of_seq (PointSet.to_seq edges) in

    let in_bounds p =
      Point.x p >= bounds.min_x - 1
      && Point.x p <= bounds.max_x + 1
      && Point.y p >= bounds.min_y - 1
      && Point.y p <= bounds.max_y + 1
    in

    let outside =
      points_outside_trench in_bounds edges (bounds.min_x - 1, bounds.min_y - 1)
    and search_area =
      Z.of_int
      @@ ((bounds.max_x - bounds.min_x + 3) * (bounds.max_y - bounds.min_y + 3))
    in

    let outside_count = Z.of_int @@ PointSet.cardinal outside in

    Z.sub search_area outside_count
end

module Part2 = struct
  let run _ = failwith "unimplemented"
end
