module Direction = struct
  type t = Up | Down | Left | Right

  let pp : t Fmt.t =
   fun ppf -> function
    | Up -> Fmt.string ppf "Up"
    | Down -> Fmt.string ppf "Down"
    | Right -> Fmt.string ppf "Right"
    | Left -> Fmt.string ppf "Left"
end

module Point = struct
  type t = int * int

  let x : t -> int = fst
  let y : t -> int = snd
  let add (a : t) (b : t) : t = (Int.add (x a) (x b), Int.add (y a) (y b))

  let compare (a : t) (b : t) : int =
    match Int.compare (x a) (x b) with
    | 0 -> Int.compare (y a) (y b)
    | other -> other

  let equal (a : t) (b : t) : bool = Int.equal 0 (compare a b)

  let move : Direction.t -> t -> t = function
    | Direction.Up -> add (0, 1)
    | Direction.Down -> add (0, -1)
    | Direction.Right -> add (1, 0)
    | Direction.Left -> add (-1, 0)

  let pp : t Fmt.t = Fmt.Dump.pair Fmt.int Fmt.int
end

module PointMap = Map.Make (Point)
module PointSet = Set.Make (Point)

module Pipe = struct
  type t = NS | EW | NE | NW | SW | SE

  let of_char_opt = function
    | '|' -> Some NS
    | '-' -> Some EW
    | 'L' -> Some NE
    | 'J' -> Some NW
    | '7' -> Some SW
    | 'F' -> Some SE
    | _ -> None

  let of_char c =
    match of_char_opt c with
    | Some p -> p
    | None -> failwith @@ Printf.sprintf "unrecognized pipe %c" c

  let to_char = function
    | NS -> '|'
    | EW -> '-'
    | NE -> 'L'
    | NW -> 'J'
    | SW -> '7'
    | SE -> 'F'

  let pp : t Fmt.t = (Fmt.using to_char) Fmt.char
  let has_entrance_on_left = function EW | NW | SW -> true | _ -> false
  let has_entrance_on_right = function EW | NE | SE -> true | _ -> false
  let has_entrance_above = function NS | NE | NW -> true | _ -> false
  let has_entrance_below = function NS | SE | SW -> true | _ -> false
end

module Graph = struct
  type t = { start_pos : Point.t; pipes : Pipe.t PointMap.t }

  let make ~start_pos ~pipes = { start_pos; pipes }
  let start_pos graph = graph.start_pos

  let pipe_at_opt (pos : Point.t) (graph : t) : Pipe.t option =
    PointMap.find_opt pos graph.pipes

  let pipe_at (pos : Point.t) (graph : t) : Pipe.t =
    match pipe_at_opt pos graph with
    | Some pipe -> pipe
    | None ->
        let msg = Fmt.str "no pipe at %a" Point.pp pos in
        if Point.equal graph.start_pos pos then failwith (msg ^ "; start is")
        else failwith msg
end

module Walker = struct
  type t = { pos : Point.t; direction : Direction.t }

  let create pos direction = { pos; direction }
  let pos (walker : t) : Point.t = walker.pos
  let direction (walker : t) : Direction.t = walker.direction

  let step_from (cur_pipe : Pipe.t) (walker : t) : t =
    let cur_pos = pos walker in
    match (cur_pipe, direction walker) with
    | Pipe.NS, Direction.Up ->
        { walker with pos = Point.move Direction.Up cur_pos }
    | Pipe.NS, Direction.Down ->
        { walker with pos = Point.move Direction.Down cur_pos }
    | Pipe.EW, Direction.Left ->
        { walker with pos = Point.move Direction.Left cur_pos }
    | Pipe.EW, Direction.Right ->
        { walker with pos = Point.move Direction.Right cur_pos }
    | Pipe.NE, Direction.Left ->
        { direction = Direction.Up; pos = cur_pos |> Point.move Direction.Up }
    | Pipe.NE, Direction.Down ->
        {
          direction = Direction.Right;
          pos = cur_pos |> Point.move Direction.Right;
        }
    | Pipe.NW, Direction.Right ->
        { direction = Direction.Up; pos = cur_pos |> Point.move Direction.Up }
    | Pipe.NW, Direction.Down ->
        {
          direction = Direction.Left;
          pos = cur_pos |> Point.move Direction.Left;
        }
    | Pipe.SW, Direction.Right ->
        {
          direction = Direction.Down;
          pos = cur_pos |> Point.move Direction.Down;
        }
    | Pipe.SW, Direction.Up ->
        {
          direction = Direction.Left;
          pos = cur_pos |> Point.move Direction.Left;
        }
    | Pipe.SE, Direction.Left ->
        {
          direction = Direction.Down;
          pos = cur_pos |> Point.move Direction.Down;
        }
    | Pipe.SE, Direction.Up ->
        {
          direction = Direction.Right;
          pos = cur_pos |> Point.move Direction.Right;
        }
    | pipe, direction ->
        failwith
        @@ Fmt.str "cannot move %a from pipe %a at pos=%a" Direction.pp
             direction Pipe.pp pipe Point.pp cur_pos

  let step (graph : Graph.t) (walker : t) : t =
    let cur_pipe = Graph.pipe_at (pos walker) graph in
    step_from cur_pipe walker
end

let rec max_steps_aux (graph : Graph.t) (w0, w1) (visited : PointSet.t)
    (i : int) : int =
  let pos0 = Walker.pos w0 and pos1 = Walker.pos w1 in

  if
    Point.equal pos0 pos1
    || PointSet.mem pos0 visited
    || PointSet.mem pos1 visited
  then i
  else
    let w0' = w0 |> Walker.step graph
    and w1' = w1 |> Walker.step graph
    and visited' = visited |> PointSet.add pos0 |> PointSet.add pos1 in

    max_steps_aux graph (w0', w1') visited' (i + 1)

type 'a surrounding = { left : 'a; right : 'a; up : 'a; down : 'a }

let pp_surrounding : 'a Fmt.t -> 'a surrounding Fmt.t =
 fun ppv ->
  let left x = x.left
  and right x = x.right
  and up x = x.up
  and down x = x.down in
  Fmt.Dump.record
    [
      Fmt.Dump.field "left" left ppv;
      Fmt.Dump.field "right" right ppv;
      Fmt.Dump.field "up" up ppv;
      Fmt.Dump.field "down" down ppv;
    ]

let pipes_around_start (graph : Graph.t) : Pipe.t option surrounding =
  let start = Graph.start_pos graph
  and get_pipe pos = Graph.pipe_at_opt pos graph in

  {
    left = start |> Point.move Direction.Left |> get_pipe;
    right = start |> Point.move Direction.Right |> get_pipe;
    up = start |> Point.move Direction.Up |> get_pipe;
    down = start |> Point.move Direction.Down |> get_pipe;
  }

let implied_start_pipe (graph : Graph.t) : Pipe.t =
  match pipes_around_start graph with
  | { left = Some lp; right = Some rp; _ }
    when Pipe.has_entrance_on_right lp && Pipe.has_entrance_on_left rp ->
      Pipe.EW
  | { up = Some upp; down = Some dp; _ }
    when Pipe.has_entrance_below upp && Pipe.has_entrance_above dp ->
      Pipe.NS
  | { up = Some upp; right = Some rp; _ }
    when Pipe.has_entrance_below upp && Pipe.has_entrance_on_left rp ->
      Pipe.NE
  | { up = Some upp; left = Some lp; _ }
    when Pipe.has_entrance_below upp && Pipe.has_entrance_on_right lp ->
      Pipe.NW
  | { left = Some lp; down = Some dp; _ }
    when Pipe.has_entrance_on_right lp && Pipe.has_entrance_above dp ->
      Pipe.SW
  | { right = Some rp; down = Some dp; _ }
    when Pipe.has_entrance_on_left rp && Pipe.has_entrance_above dp ->
      Pipe.SE
  | other ->
      failwith
      @@ Fmt.str "could not determine start pipe from %a"
           (pp_surrounding (Fmt.Dump.option Pipe.pp))
           other

let max_steps (graph : Graph.t) : int =
  let start = Graph.start_pos graph in
  let new_walker d = Walker.create (Point.move d start) d in

  let d0, d1 =
    match implied_start_pipe graph with
    | Pipe.NS -> Direction.(Up, Down)
    | Pipe.EW -> Direction.(Left, Right)
    | Pipe.NE -> Direction.(Up, Right)
    | Pipe.NW -> Direction.(Up, Left)
    | Pipe.SW -> Direction.(Down, Left)
    | Pipe.SE -> Direction.(Down, Right)
  in

  let w0 = new_walker d0 and w1 = new_walker d1 in

  max_steps_aux graph (w0, w1) PointSet.empty 1

let graph_of_lines (lines : string Seq.t) : Graph.t =
  (* reverse first for intuitive x coords *)
  let pipes, start =
    lines
    |> List.of_seq
    |> List.rev
    |> List.mapi (fun i line -> (i, line))
    |> List.fold_left
         (fun (pipes, start) (y, line) ->
           line
           |> String.to_seq
           |> Seq.fold_lefti
                (fun (pipes, start) x -> function
                  | '.' -> (pipes, start)
                  | 'S' -> (
                      match start with
                      | None -> (pipes, Some (x, y))
                      | Some existing ->
                          failwith
                          @@ Fmt.str "start found at %a and %a" Point.pp
                               existing Point.pp (x, y))
                  | other ->
                      (PointMap.add (x, y) (Pipe.of_char other) pipes, start))
                (pipes, start))
         (PointMap.empty, None)
  in

  let start_pos =
    match start with Some x -> x | None -> failwith "start not found"
  in

  Graph.make ~pipes ~start_pos

module Input = struct
  type t = string Seq.t

  let of_in_channel chan =
    (fun () -> try Some (input_line chan) with End_of_file -> None)
    |> Seq.of_dispenser
    |> Seq.memoize

  let of_string s = s |> String.split_on_char '\n' |> List.to_seq
end

module Part1 = struct
  let run : Input.t -> int = fun lines -> lines |> graph_of_lines |> max_steps
end

module Part2 = struct
  let run _ = failwith "unimplemented"
end
