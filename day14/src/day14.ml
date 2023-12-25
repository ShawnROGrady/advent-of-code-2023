[@@@ocaml.warning "-32-37"]

module Direction = struct
  type t = North | South | East | West
end

module Order = struct
  type 'a t = 'a -> 'a -> int

  let asc : 'a t -> 'a t = Fun.id
  let desc : 'a t -> 'a t = fun cmp a b -> Int.neg (cmp a b)

  let ( && ) (cmp1 : 'a t) (cmp2 : 'a t) : 'a t =
   fun a b -> match cmp1 a b with 0 -> cmp2 a b | other -> other

  let by (type a b) (field : a -> b) (cmp : b t) : a t =
   fun a b -> cmp (field a) (field b)
end

module Point = struct
  type t = int * int

  let x : t -> int = fst
  let y : t -> int = snd

  module OrderBy = struct
    let x (ord : int Order.t -> int Order.t) : t Order.t =
      Order.by x (ord Int.compare)

    let y (ord : int Order.t -> int Order.t) : t Order.t =
      Order.by y (ord Int.compare)

    include Order
  end

  let compare : t -> t -> int = OrderBy.(x asc && y asc)
  let add (a : t) (b : t) : t = (x a + x b, y a + y b)
  let sub (a : t) (b : t) : t = add a (Int.neg @@ x b, Int.neg @@ y b)

  let move : Direction.t -> t -> t = function
    | Direction.North -> add (0, 1)
    | Direction.South -> add (0, -1)
    | Direction.East -> add (1, 0)
    | Direction.West -> add (-1, 0)

  let pp : t Fmt.t = Fmt.(Dump.pair int int)
end

module PointSet = Set.Make (Point)

module Rock = struct
  type t = Moveable | Stationary

  let of_char = function
    | '#' -> Stationary
    | 'O' -> Moveable
    | other -> failwith @@ Fmt.str "invalid rock '%c'" other

  let as_char = function Stationary -> '#' | Moveable -> '0'

  let equal a b =
    match (a, b) with
    | Moveable, Moveable -> true
    | Stationary, Stationary -> true
    | _ -> false
end

module IntMap = Map.Make (Int)
module PointSetMap = Map.Make (PointSet)

let range ?(start = 0) stop : int Seq.t =
  start |> Seq.ints |> Seq.take (stop - start)

module ControlPanel = struct
  (* For simplicity defining (0,0) as the south-west (i.e. bottom left)
     corner. *)
  type t = {
    max_x : int;
    max_y : int;
    stationary_rocks : PointSet.t;
    moveable_rocks : PointSet.t;
  }

  let row_as_string (panel : t) (y : int) : string =
    range panel.max_x
    |> Seq.map (fun x ->
           if PointSet.mem (x, y) panel.stationary_rocks then
             Rock.as_char Rock.Stationary
           else if PointSet.mem (x, y) panel.moveable_rocks then
             Rock.as_char Rock.Moveable
           else '.')
    |> String.of_seq

  let as_string (panel : t) : string =
    range panel.max_y
    |> Seq.map (row_as_string panel)
    |> List.of_seq
    |> List.rev
    |> String.concat "\n"

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

  let[@inline] can_move_to (point : Point.t) (panel : t) : bool =
    is_inbounds point panel
    && not
         (has_stationary_rock_at point panel || has_moveable_rock_at point panel)

  let rec move_as_far (direction : Direction.t) (cur : Point.t) (panel : t) :
      Point.t =
    let next = Point.move direction cur in
    if can_move_to next panel then move_as_far direction next panel else cur

  let rock_order : Direction.t -> Point.t Order.t = function
    | Direction.North -> Point.OrderBy.(y desc && x asc)
    | Direction.South -> Point.OrderBy.(y asc && x asc)
    | Direction.East -> Point.OrderBy.(x desc && y asc)
    | Direction.West -> Point.OrderBy.(x asc && y asc)

  let tilt (direction : Direction.t) (panel : t) : t =
    panel
    |> moveable_rocks
    |> PointSet.elements
    |> List.sort (rock_order direction)
    |> List.fold_left
         (fun panel rock ->
           {
             panel with
             moveable_rocks =
               PointSet.add
                 (move_as_far direction rock panel)
                 panel.moveable_rocks;
           })
         { panel with moveable_rocks = PointSet.empty }

  let tilt_north (panel : t) : t = tilt Direction.North panel
  let cycle_directions = Direction.[ North; West; South; East ]

  let cycle (panel : t) : t =
    cycle_directions |> List.fold_left (fun panel dir -> tilt dir panel) panel

  module CycleCache = struct
    type panel = t

    type t = {
      count_by_rocks : int PointSetMap.t;
      rocks_by_count : PointSet.t IntMap.t;
    }

    let empty =
      { count_by_rocks = PointSetMap.empty; rocks_by_count = IntMap.empty }

    let add ((panel, count) : panel * int) (cache : t) : t =
      let rocks = moveable_rocks panel in
      {
        count_by_rocks = PointSetMap.add rocks count cache.count_by_rocks;
        rocks_by_count = IntMap.add count rocks cache.rocks_by_count;
      }

    let find_prev_count (panel : panel) (cache : t) : int option =
      PointSetMap.find_opt (moveable_rocks panel) cache.count_by_rocks

    let rocks_at_count (count : int) (cache : t) : PointSet.t =
      match IntMap.find_opt count cache.rocks_by_count with
      | Some rocks -> rocks
      | None -> failwith @@ Fmt.str "cache has no rocks for count=%d" count
  end

  let rec cycle_for_aux (count : int) (i : int) (cache : CycleCache.t)
      (panel : t) : t =
    match CycleCache.find_prev_count panel cache with
    | Some prev_count -> (
        let repeat_len = i - prev_count in
        match (count - i) mod repeat_len with
        | 0 -> panel
        | d ->
            {
              panel with
              moveable_rocks = CycleCache.rocks_at_count (d + prev_count) cache;
            })
    | None ->
        if i >= count then panel
        else
          let cache' = CycleCache.add (panel, i) cache
          and panel' = cycle panel in

          cycle_for_aux count (Int.succ i) cache' panel'

  let cycle_for (count : int) (panel : t) : t =
    cycle_for_aux count 0 CycleCache.empty panel

  let load_on_north_of_rock (rock : Point.t) =
    rock |> Point.y |> Z.of_int |> Z.succ

  let total_load_on_north panel =
    panel
    |> moveable_rocks
    |> PointSet.to_seq
    |> Seq.map load_on_north_of_rock
    |> Seq.fold_left Z.add Z.zero
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
  let run : Input.t -> Z.t =
   fun rocks ->
    rocks
    |> ControlPanel.from_rocks
    |> ControlPanel.tilt_north
    |> ControlPanel.total_load_on_north
end

module Part2 = struct
  let run : Input.t -> Z.t =
   fun rocks ->
    rocks
    |> ControlPanel.from_rocks
    |> ControlPanel.cycle_for 1000000000
    |> ControlPanel.total_load_on_north
end
