[@@@ocaml.warning "-32"]

module Point = struct
  type t = int * int

  let x : t -> int = fst
  let y : t -> int = snd

  let compare (a : t) (b : t) : int =
    match Int.compare (x a) (x b) with
    | 0 -> Int.compare (y a) (y b)
    | other -> other

  let add (a : t) (b : t) : t = (x a + x b, y a + y b)
  let sub (a : t) (b : t) : t = add a (Int.neg @@ x b, Int.neg @@ y b)
  let pp : t Fmt.t = Fmt.(Dump.pair int int)
end

module PointSet = Set.Make (Point)

module Rock = struct
  type t = Moveable | Stationary

  let of_char = function
    | '#' -> Stationary
    | 'O' -> Moveable
    | other -> failwith @@ Fmt.str "invalid rock '%c'" other

  let equal a b =
    match (a, b) with
    | Moveable, Moveable -> true
    | Stationary, Stationary -> true
    | _ -> false
end

module ControlPanel = struct
  (* For simplicity defining (0,0) as the south-west (i.e. bottom left)
     corner. *)
  type t = {
    max_x : int;
    max_y : int;
    stationary_rocks : PointSet.t;
    moveable_rocks : PointSet.t;
  }

  let from_rocks (rocks : Rock.t option Seq.t Seq.t) : t =
    let points, max_x, max_y =
      rocks
      |> Seq.fold_lefti
           (fun acc dy row ->
             row
             |> Seq.fold_lefti
                  (fun (points, max_x, _) x rock ->
                    let points' =
                      match rock with
                      | Some p -> Seq.cons ((x, dy), p) points
                      | None -> points
                    in

                    (points', Int.max x max_x, dy))
                  acc)
           (Seq.empty, 0, 0)
    in

    let points' =
      points
      |> Seq.map (fun (point, rock) ->
             ( Point.add (0, max_y) (Point.x point, Int.neg @@ Point.y point),
               rock ))
    in

    let stationary, moveable =
      Seq.partition (fun (_, rock) -> Rock.(equal Stationary) rock) points'
    in

    let stationary_rocks = PointSet.of_seq @@ Seq.map fst stationary
    and moveable_rocks = PointSet.of_seq @@ Seq.map fst moveable
    and max_x = Int.succ max_x
    and max_y = Int.succ max_y in

    { max_x; max_y; stationary_rocks; moveable_rocks }

  let max_x panel = panel.max_x [@@inline]
  let max_y panel = panel.max_y [@@inline]
  let stationary_rocks panel = panel.stationary_rocks [@@inline]
  let moveable_rocks panel = panel.moveable_rocks [@@inline]

  let is_inbounds (point : Point.t) (panel : t) : bool =
    let x = Point.x point and y = Point.y point in

    x >= 0 && x < max_x panel && y >= 0 && y < max_y panel

  let[@inline] has_stationary_rock_at (point : Point.t) (panel : t) : bool =
    PointSet.mem point (stationary_rocks panel)

  let[@inline] has_moveable_rock_at (point : Point.t) (panel : t) : bool =
    PointSet.mem point (moveable_rocks panel)

  let can_move_to (point : Point.t) (panel : t) : bool =
    is_inbounds point panel
    && not
         (has_stationary_rock_at point panel || has_moveable_rock_at point panel)

  let rec northermost (cur : Point.t) (panel : t) : Point.t =
    let next = Point.add (0, 1) cur in
    if can_move_to next panel then northermost next panel else cur

  let tilt_north (panel : t) : t =
    panel
    |> moveable_rocks
    |> PointSet.elements
    |> List.sort (fun a b ->
           (* We could redefine the comparison used by PointSet to avoid
              tge explicit sort here, but this feels a bit more clear.
              Especially if part 2 requires tilting in other directions.
           *)
           match Int.compare (Point.y b) (Point.y a) with
           | 0 -> Int.compare (Point.x a) (Point.x b)
           | other -> other)
    |> List.fold_left
         (fun panel rock ->
           {
             panel with
             moveable_rocks =
               PointSet.add (northermost rock panel) panel.moveable_rocks;
           })
         { panel with moveable_rocks = PointSet.empty }
end

module Input = struct
  type t = Rock.t option Seq.t Seq.t

  let parse_line s =
    s
    |> String.to_seq
    |> Seq.map (function '.' -> None | other -> Some (Rock.of_char other))

  let of_in_channel (chan : in_channel) : t =
    (fun () ->
      try Some (parse_line @@ input_line chan) with End_of_file -> None)
    |> Seq.of_dispenser
    |> Seq.memoize

  let of_string (s : string) : t =
    s |> String.split_on_char '\n' |> List.to_seq |> Seq.map parse_line
end

module Part1 = struct
  let load_on_north (rock : Point.t) = rock |> Point.y |> Z.of_int |> Z.succ

  let run : Input.t -> Z.t =
   fun rocks ->
    rocks
    |> ControlPanel.from_rocks
    |> ControlPanel.tilt_north
    |> ControlPanel.moveable_rocks
    |> PointSet.to_seq
    |> Seq.map load_on_north
    |> Seq.fold_left Z.add Z.zero
end

module Part2 = struct
  let run _ = failwith "unimplemented"
end
