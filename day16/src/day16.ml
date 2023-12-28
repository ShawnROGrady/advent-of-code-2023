[@@@ocaml.warning "-32"]

module Direction = struct
  type t = Up | Down | Left | Right

  let as_int = function Up -> 0 | Down -> 1 | Left -> 2 | Right -> 3
  let compare a b : int = Int.compare (as_int a) (as_int b)
  let equal a b : bool = Int.equal 0 @@ compare a b

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

  let compare a b : int =
    match Int.compare (x a) (x b) with
    | 0 -> Int.compare (y a) (y b)
    | other -> other

  let add (a : t) (b : t) : t = (x a + x b, y a + y b)

  let move : Direction.t -> t -> t = function
    | Direction.Up -> add (0, 1)
    | Direction.Down -> add (0, -1)
    | Direction.Left -> add (-1, 0)
    | Direction.Right -> add (1, 0)

  let pp : t Fmt.t = Fmt.Dump.pair Fmt.int Fmt.int
end

module Modifier = struct
  type mirror = Sw_ne | Nw_se
  type splitter = H | V
  type t = Mirror of mirror | Splitter of splitter

  let reflect dir mirror =
    match (dir, mirror) with
    | Direction.Up, Sw_ne -> Direction.Right
    | Direction.Up, Nw_se -> Direction.Left
    | Direction.Down, Sw_ne -> Direction.Left
    | Direction.Down, Nw_se -> Direction.Right
    | Direction.Left, Sw_ne -> Direction.Down
    | Direction.Left, Nw_se -> Direction.Up
    | Direction.Right, Sw_ne -> Direction.Up
    | Direction.Right, Nw_se -> Direction.Down

  let of_char = function
    | '-' -> Splitter H
    | '|' -> Splitter V
    | '/' -> Mirror Sw_ne
    | '\\' -> Mirror Nw_se
    | other -> failwith @@ Printf.sprintf "unknown modifier '%c'" other
end

module PointSet = Set.Make (Point)
module PointMap = Map.Make (Point)

module Grid = struct
  type t = { modifiers : Modifier.t PointMap.t; max_x : int; max_y : int }

  let make ~modifiers ~max_x ~max_y = { modifiers; max_x; max_y }
  let max_y grid = grid.max_y
  let max_x grid = grid.max_x

  let in_bounds ((x, y) : Point.t) (grid : t) : bool =
    x >= 0 && y >= 0 && x < grid.max_x && y < grid.max_y

  let modifier_at (point : Point.t) (grid : t) : Modifier.t option =
    PointMap.find_opt point grid.modifiers
end

module Input = struct
  type t = Grid.t

  let of_string (s : string) : t =
    let lines =
      s
      |> String.split_on_char '\n'
      |> List.filter (Fun.negate @@ String.equal "")
      |> List.rev
    in

    let max_y = List.length lines and max_x = String.length (List.hd lines) in

    let modifiers =
      lines
      |> List.fold_left
           (fun (y, mods) line ->
             let mods' =
               line
               |> String.to_seq
               |> Seq.fold_lefti
                    (fun mods x -> function
                      | '.' -> mods
                      | other ->
                          PointMap.add (x, y) (Modifier.of_char other) mods)
                    mods
             in
             (Int.succ y, mods'))
           (0, PointMap.empty)
      |> snd
    in

    Grid.make ~modifiers ~max_x ~max_y

  let of_in_channel (ic : in_channel) : t = of_string (In_channel.input_all ic)
end

module State = struct
  type beam_point = Point.t * Direction.t

  module Beam_point_set = Set.Make (struct
    type t = beam_point

    let compare (point_a, dir_a) (point_b, dir_b) =
      match Point.compare point_a point_b with
      | 0 -> Direction.compare dir_a dir_b
      | other -> other
  end)

  type t = {
    beam_points : beam_point list;
    energized : PointSet.t;
    visited : Beam_point_set.t;
  }

  let step (grid : Grid.t) : t -> t = function
    | { beam_points = []; _ } as state -> state
    | { beam_points = (point, dir) :: rest; energized; visited }
      when Beam_point_set.mem (point, dir) visited ->
        { beam_points = rest; energized; visited }
    | { beam_points = (point, _) :: rest; energized; visited }
      when not (Grid.in_bounds point grid) ->
        { beam_points = rest; energized; visited }
    | { beam_points = (point, dir) :: rest; energized; visited } -> (
        let energized' = PointSet.add point energized
        and visited' = Beam_point_set.add (point, dir) visited in

        match Grid.modifier_at point grid with
        | None ->
            let point' = Point.move dir point in
            {
              beam_points = (point', dir) :: rest;
              energized = energized';
              visited = visited';
            }
        | Some Modifier.(Mirror m) ->
            let dir' = Modifier.reflect dir m in
            let point' = Point.move dir' point in
            {
              beam_points = (point', dir') :: rest;
              energized = energized';
              visited = visited';
            }
        | Some Modifier.(Splitter H) ->
            let beam_points' =
              if Direction.(equal dir Up) || Direction.(equal dir Down) then
                (Point.move Direction.Left point, Direction.Left)
                :: (Point.move Direction.Right point, Direction.Right)
                :: rest
              else (Point.move dir point, dir) :: rest
            in
            {
              beam_points = beam_points';
              energized = energized';
              visited = visited';
            }
        | Some Modifier.(Splitter V) ->
            let beam_points' =
              if Direction.(equal dir Left) || Direction.(equal dir Right) then
                (Point.move Direction.Up point, Direction.Up)
                :: (Point.move Direction.Down point, Direction.Down)
                :: rest
              else (Point.move dir point, dir) :: rest
            in
            {
              beam_points = beam_points';
              energized = energized';
              visited = visited';
            })

  let init (point, dir) =
    {
      beam_points = [ (point, dir) ];
      energized = PointSet.empty;
      visited = Beam_point_set.empty;
    }
end

module Part1 = struct
  let energized_squares grid =
    let rec loop = function
      | { State.beam_points = []; energized; _ } -> PointSet.cardinal energized
      | state -> loop (State.step grid state)
    in

    loop @@ State.init ((0, Int.pred @@ Grid.max_y grid), Direction.Right)

  let run : Input.t -> int = energized_squares
end

let range stop = Seq.take stop (Seq.ints 0)

module Part2 = struct
  let top_edge_points grid =
    let y = Int.pred @@ Grid.max_y grid in
    range (Grid.max_x grid) |> Seq.map (fun x -> (x, y))

  let bottom_edge_points grid =
    let y = 0 in
    range (Grid.max_x grid) |> Seq.map (fun x -> (x, y))

  let left_edge_points grid =
    let x = 0 in
    range (Grid.max_y grid) |> Seq.map (fun y -> (x, y))

  let right_edge_points grid =
    let x = Int.pred @@ Grid.max_x grid in
    range (Grid.max_y grid) |> Seq.map (fun y -> (x, y))

  let possible_starts grid =
    let top =
      top_edge_points grid |> Seq.map (fun point -> (point, Direction.Down))
    and bottom =
      bottom_edge_points grid |> Seq.map (fun point -> (point, Direction.Up))
    and left =
      left_edge_points grid |> Seq.map (fun point -> (point, Direction.Right))
    and right =
      right_edge_points grid |> Seq.map (fun point -> (point, Direction.Left))
    in

    Seq.append top @@ Seq.append bottom @@ Seq.append left right

  let max (xs : int Seq.t) : int = xs |> Seq.fold_left Int.max 0

  let energized_squares grid start =
    let rec loop = function
      | { State.beam_points = []; energized; _ } -> PointSet.cardinal energized
      | state -> loop (State.step grid state)
    in

    loop @@ State.init start

  let run : Input.t -> int =
   fun grid ->
    grid |> possible_starts |> Seq.map (energized_squares grid) |> max
end
