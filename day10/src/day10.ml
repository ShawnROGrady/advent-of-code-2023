module Direction = struct
  type t = Up | Down | Left | Right

  let as_int : t -> int = function
    | Up -> 0
    | Down -> 1
    | Left -> 2
    | Right -> 3

  let compare (a : t) (b : t) : int = Int.compare (as_int a) (as_int b)

  let pp : t Fmt.t =
   fun ppf -> function
    | Up -> Fmt.string ppf "Up"
    | Down -> Fmt.string ppf "Down"
    | Right -> Fmt.string ppf "Right"
    | Left -> Fmt.string ppf "Left"
end

module DirectionSet = Set.Make (Direction)

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

  let _entrances : t -> Direction.t * Direction.t = function
    | NS -> Direction.(Up, Down)
    | EW -> Direction.(Left, Right)
    | NE -> Direction.(Up, Right)
    | NW -> Direction.(Up, Left)
    | SW -> Direction.(Down, Left)
    | SE -> Direction.(Down, Right)

  let entrances : t -> DirectionSet.t =
   fun p ->
    let d0, d1 = _entrances p in
    DirectionSet.empty |> DirectionSet.add d0 |> DirectionSet.add d1

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

let rec find_loop_aux (graph : Graph.t) (w0, w1) (visited : PointSet.t) :
    PointSet.t =
  let pos0 = Walker.pos w0 and pos1 = Walker.pos w1 in

  if Point.equal pos0 pos1 then PointSet.add pos0 visited
  else if PointSet.mem pos0 visited || PointSet.mem pos1 visited then visited
  else
    let w0' = w0 |> Walker.step graph
    and w1' = w1 |> Walker.step graph
    and visited' = visited |> PointSet.add pos0 |> PointSet.add pos1 in

    find_loop_aux graph (w0', w1') visited'

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

let surrounding_as_seq (surrounding : 'a surrounding) : (Direction.t * 'a) Seq.t
    =
  Seq.cons (Direction.Left, surrounding.left)
  @@ Seq.cons (Direction.Right, surrounding.right)
  @@ Seq.cons (Direction.Up, surrounding.up)
  @@ Seq.cons (Direction.Down, surrounding.down)
  @@ Seq.empty

let map_surrounding (f : 'a -> 'b) (surrounding : 'a surrounding) :
    'b surrounding =
  {
    left = f surrounding.left;
    right = f surrounding.right;
    up = f surrounding.up;
    down = f surrounding.down;
  }

let points_surrounding (pos : Point.t) : Point.t surrounding =
  {
    left = pos |> Point.move Direction.Left;
    right = pos |> Point.move Direction.Right;
    up = pos |> Point.move Direction.Up;
    down = pos |> Point.move Direction.Down;
  }

let pipes_around_start (graph : Graph.t) : Pipe.t option surrounding =
  let start = Graph.start_pos graph
  and get_pipe pos = Graph.pipe_at_opt pos graph in

  start |> points_surrounding |> map_surrounding get_pipe

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

let find_loop (graph : Graph.t) : PointSet.t =
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

  find_loop_aux graph (w0, w1) (PointSet.singleton start)

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
  let max_steps (graph : Graph.t) : int =
    graph |> find_loop |> PointSet.cardinal |> fun loop_size -> loop_size / 2

  let run : Input.t -> int = fun lines -> lines |> graph_of_lines |> max_steps
end

module Part2 = struct
  let bounds_of_loop (loop : PointSet.t) : Point.t * Point.t =
    loop
    |> PointSet.to_seq
    |> Seq.fold_left
         (fun ((min_x, min_y), (max_x, max_y)) (x, y) ->
           ( (Int.min min_x x, Int.min min_y y),
             (Int.max max_x x, Int.max max_y y) ))
         (Int.(max_int, max_int), (0, 0))

  let doubled_loop_points (loop_pipes : Pipe.t PointMap.t) : PointSet.t =
    loop_pipes
    |> PointMap.to_seq
    |> Seq.map (fun ((x, y), p) -> ((x * 2, y * 2), p))
    |> Seq.fold_left
         (fun acc (point, pipe) ->
           let extra =
             pipe
             |> Pipe.entrances
             |> DirectionSet.to_seq
             |> Seq.map (fun d -> Point.move d point)
           in

           acc |> PointSet.add point |> PointSet.add_seq extra)
         PointSet.empty

  let points_outside_of_loop (loop_pipes : Pipe.t PointMap.t) (start : Point.t)
      (target : Point.t) : PointSet.t =
    let visited = ref (PointSet.singleton start)
    and q = Queue.create ()
    and loop_points = doubled_loop_points loop_pipes in

    let target = (2 * Point.x target, 2 * Point.y target)
    and start = (2 * Point.x start, 2 * Point.y start) in
    Queue.push start q;

    while not (Queue.is_empty q) do
      let v = Queue.pop q in
      let w =
        v
        |> points_surrounding
        |> surrounding_as_seq
        |> Seq.filter (fun (_, (x, y)) ->
               (not (PointSet.mem (x, y) !visited))
               && x >= Point.x start
               && y >= Point.y start
               && x <= Point.x target
               && y <= Point.y target
               && (false || not (PointSet.mem (x, y) loop_points)))
        |> Seq.map snd
      in

      w
      |> Seq.iter (fun p ->
             visited := PointSet.add p !visited;
             Queue.push p q)
    done;

    let loop_elems =
      loop_pipes |> PointMap.to_seq |> Seq.map fst |> PointSet.of_seq
    in

    let visited_points =
      !visited
      |> PointSet.to_seq
      |> Seq.filter (fun (x, y) -> x mod 2 = 0 && y mod 2 = 0)
      |> Seq.map (fun (x, y) -> (x / 2, y / 2))
      |> PointSet.of_seq
    in

    PointSet.diff visited_points loop_elems

  let run : Input.t -> int =
   fun lines ->
    let graph : Graph.t = lines |> graph_of_lines in
    let loop = find_loop graph and implied_start = implied_start_pipe graph in

    let _, (max_x, max_y) = bounds_of_loop loop
    and pipes =
      PointMap.add (Graph.start_pos graph) implied_start graph.pipes
    in

    let search_start = (-1, -1)
    and search_end = (max_x + 1, max_y + 1)
    and loop_pipes =
      loop
      |> PointSet.to_seq
      |> Seq.map (fun p -> (p, PointMap.find p pipes))
      |> PointMap.of_seq
    in

    let search_area =
      (Point.x search_end - Point.x search_start + 1)
      * (Point.y search_end - Point.y search_start + 1)
    and num_points_outside =
      points_outside_of_loop loop_pipes search_start search_end
      |> PointSet.cardinal
    and num_points_of_loop = PointSet.cardinal loop in

    search_area - num_points_of_loop - num_points_outside
end
