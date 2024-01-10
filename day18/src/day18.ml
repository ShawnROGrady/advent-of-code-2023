module Direction = struct
  type t = Up | Down | Left | Right

  let of_char = function
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right
    | other -> invalid_arg @@ Fmt.str "unrecognized direction '%c'" other

  let of_int = function
    | 0 -> Right
    | 1 -> Down
    | 2 -> Left
    | 3 -> Up
    | other -> invalid_arg @@ Fmt.str "unrecognized direction %d" other

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
  let[@warning "-32"] pp : t Fmt.t = Fmt.Dump.pair Fmt.int Fmt.int
end

module Vector = struct
  type t = { magnitude : int; direction : Direction.t }

  let make ~magnitude ~direction = { magnitude; direction }
  let magnitude v = v.magnitude
  let direction v = v.direction

  let apply (pos : Point.t) (vec : t) : Point.t =
    let mag = magnitude vec in
    match direction vec with
    | Direction.Up -> Point.add (0, mag) pos
    | Direction.Down -> Point.add (0, Int.neg mag) pos
    | Direction.Left -> Point.add (Int.neg mag, 0) pos
    | Direction.Right -> Point.add (mag, 0) pos

  let[@warning "-32"] pp : t Fmt.t =
    let open Fmt in
    let open Fmt.Dump in
    record
      [
        field "magnitude" magnitude int;
        field "direction" direction Direction.pp;
      ]
end

module Instruction = struct
  type t = { dir : Direction.t; steps : int; color : string }

  let make ~dir ~steps ~color = { dir; steps; color }
  let dir inst = inst.dir
  let steps inst = inst.steps
  let color inst = inst.color
  let as_vector inst = Vector.make ~magnitude:inst.steps ~direction:inst.dir

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

module PointSet = struct
  include Set.Make (Point)

  let symmetric_diff (s1 : t) (s2 : t) : t = union (diff s1 s2) (diff s2 s1)
end

module Intersections = struct
  module ByCoord = Map.Make (Int)

  type t = {
    all : PointSet.t;
    by_x : PointSet.t ByCoord.t;
    by_y : PointSet.t ByCoord.t;
  }

  let by_y idxs = idxs.by_y

  let empty : t =
    { all = PointSet.empty; by_x = ByCoord.empty; by_y = ByCoord.empty }

  let update_by_coord p = function
    | None -> Some (PointSet.singleton p)
    | Some ps -> Some (PointSet.add p ps)

  let add (point : Point.t) (idxs : t) : t =
    let all = PointSet.add point idxs.all
    and by_x = ByCoord.update (Point.x point) (update_by_coord point) idxs.by_x
    and by_y =
      ByCoord.update (Point.y point) (update_by_coord point) idxs.by_y
    in
    { all; by_x; by_y }

  let of_seq (points : Point.t Seq.t) : t =
    points |> Seq.fold_left (fun cur p -> add p cur) empty
end

module Range = struct
  type t = { start : Z.t; stop : Z.t }

  let make ~start ~stop = { start; stop }

  let length r =
    let open Z in
    r.stop - r.start + one

  let inter (r1 : t) (r2 : t) : t option =
    let open Z.Compare in
    if r1.stop <= r2.start || r1.start >= r2.stop then None
    else
      let start, stop = (Z.max r1.start r2.start, Z.min r1.stop r2.stop) in
      Some (make ~start ~stop)
end

module Lagoon : sig
  type t

  val of_intersections : Intersections.t -> t
  val area : t -> Z.t
end = struct
  type t = { intersections : Intersections.t }

  let of_intersections intersections = { intersections }

  let range_xs_inside (points_at_y : PointSet.t) : Range.t Seq.t =
    points_at_y
    |> PointSet.to_seq
    |> Seq.map Point.x
    |> Seq.map Z.of_int
    |> Seq.scan
         (fun (inside, prev_x, _) x ->
           if not inside then (true, x, None)
           else (false, x, Some (Range.make ~start:prev_x ~stop:x)))
         (false, Z.zero, None)
    |> Seq.filter_map (fun (_, _, r) -> r)

  let range_overlaps (r1s : Range.t Seq.t) (r2s : Range.t Seq.t) : Range.t Seq.t
      =
    Seq.product r1s r2s |> Seq.filter_map (fun (r1, r2) -> Range.inter r1 r2)

  let sum_seq xs = xs |> Seq.fold_left Z.add Z.zero

  let count_in_ranges (rs : Range.t Seq.t) : Z.t =
    rs |> Seq.map Range.length |> sum_seq

  type calculate_state = {
    y : int;
    points : PointSet.t;
    contained : Range.t Seq.t;
    contained_count : Z.t;
    area : Z.t;
  }

  let init_state (y, points) =
    let contained = range_xs_inside points in
    let contained_count = count_in_ranges contained in
    { y; points; contained; contained_count; area = contained_count }

  let find_area_step (prev : calculate_state)
      ((cur_y, cur_points) : int * PointSet.t) : calculate_state =
    let open Z in
    let adjusted_prevs =
      prev.points |> PointSet.map (fun (x, _) -> (x, cur_y))
    in
    let cur_points' = PointSet.symmetric_diff adjusted_prevs cur_points in

    let contained_by_next = range_xs_inside cur_points' in
    let contained_by_both = range_overlaps prev.contained contained_by_next in

    let count_inside_prev = prev.contained_count
    and count_inside_next = count_in_ranges contained_by_next
    and count_inside_both = count_in_ranges contained_by_both in

    (* First find the box representing the area from the previous vertical
       line to the current y. *)
    let box_from_prev = (~$cur_y - ~$(prev.y)) * count_inside_prev
    (* Next find the additional area added by the new intersections. *)
    and extra_at_inter = count_inside_next - count_inside_both in

    let area' = prev.area + box_from_prev + extra_at_inter in

    {
      y = cur_y;
      points = cur_points';
      contained = contained_by_next;
      contained_count = count_inside_next;
      area = area';
    }

  let area (lagoon : t) : Z.t =
    let points_by_y =
      lagoon.intersections |> Intersections.by_y |> Intersections.ByCoord.to_seq
    in
    let hd, rest =
      match points_by_y () with
      | Seq.Cons (hd, rest) -> (hd, rest)
      | Seq.Nil -> failwith "area with empty points_by_y"
    in

    let init = init_state hd in

    rest |> Seq.fold_left find_area_step init |> fun { area; _ } -> area
end

module Part1 = struct
  let run : Input.t -> Z.t =
   fun instructions ->
    let vecs = instructions |> Seq.map Instruction.as_vector in

    let intersections =
      vecs |> Seq.scan Vector.apply Point.origin |> Intersections.of_seq
    in

    intersections |> Lagoon.of_intersections |> Lagoon.area
end

module Part2 = struct
  let int_of_hex_char : char -> int = function
    | '0' .. '9' as c -> Char.code c - 48
    | 'a' .. 'z' as c -> Char.code c - 87
    | other -> invalid_arg @@ Fmt.str "invalid hex char '%c'" other

  let int_of_rgb (cs : char list) : int =
    cs
    |> List.fold_left
         (fun cur c -> Int.add (int_of_hex_char c) (Int.mul cur 16))
         0

  let vector_of_color_chars = function
    | [ c1; c2; c3; c4; c5; c6 ] ->
        let magnitude = int_of_rgb [ c1; c2; c3; c4; c5 ]
        and direction = Direction.of_int @@ int_of_hex_char c6 in
        Vector.make ~magnitude ~direction
    | other -> invalid_arg @@ Fmt.str "invalid length %d" (List.length other)

  let vector_of_color (color : string) : Vector.t =
    try color |> String.to_seq |> List.of_seq |> vector_of_color_chars
    with Invalid_argument msg ->
      failwith @@ Fmt.str "vector_of_color %a: %s" Fmt.Dump.string color msg

  let run : Input.t -> Z.t =
   fun instructions ->
    let vecs =
      instructions |> Seq.map Instruction.color |> Seq.map vector_of_color
    in

    let intersections =
      vecs |> Seq.scan Vector.apply Point.origin |> Intersections.of_seq
    in

    intersections |> Lagoon.of_intersections |> Lagoon.area
end
