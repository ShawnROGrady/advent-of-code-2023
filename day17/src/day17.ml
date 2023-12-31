[@@@ocaml.warning "-32"]

module Direction = struct
  type t = Up | Down | Left | Right

  let as_int = function Up -> 0 | Down -> 1 | Left -> 2 | Right -> 3
  let compare a b : int = Int.compare (as_int a) (as_int b)
  let equal a b : bool = Int.equal 0 @@ compare a b

  let rotate_left = function
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up

  let rotate_right = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

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

  let equal a b = Int.equal 0 (compare a b)
  let add (a : t) (b : t) : t = (x a + x b, y a + y b)

  let move : Direction.t -> t -> t = function
    | Direction.Up -> add (0, 1)
    | Direction.Down -> add (0, -1)
    | Direction.Left -> add (-1, 0)
    | Direction.Right -> add (1, 0)

  let pp : t Fmt.t = Fmt.Dump.pair Fmt.int Fmt.int
end

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module Tree = struct
  exception Empty

  module type S = sig
    type 'a t
    type key

    (* Constructors *)
    val empty : 'a t
    val singleton : key * 'a -> 'a t

    (* Checks *)
    val is_empty : 'a t -> bool

    (* Transformations *)
    val add : key * 'a -> 'a t -> 'a t
    val pop_min : 'a t -> (key * 'a) * 'a t
  end

  module Make (Ord : OrderedType) : S with type key = Ord.t = struct
    type key = Ord.t
    type 'a data = key * 'a

    type 'a node = { data : 'a data; left : 'a t; right : 'a t }
    and 'a t = 'a node option

    let empty : 'a t = None
    let singleton data : 'a t = Some { data; left = None; right = None }

    (* Checks *)
    let is_empty : 'a t -> bool = Option.is_none

    (* Transformations *)
    let rec add (data : 'a data) : 'a t -> 'a t = function
      | None -> singleton data
      | Some node ->
          if Ord.compare (fst data) (fst node.data) <= 0 then
            Some { node with left = add data node.left }
          else Some { node with right = add data node.right }

    let rec pop_min = function
      | None -> raise Empty
      | Some node -> (
          try
            let min, left_tree = pop_min node.left in
            (min, Some { node with left = left_tree })
          with Empty -> (node.data, node.right))
  end
end

module PQueue = struct
  exception Empty

  module T = Tree.Make (Int)

  type 'a t = 'a T.t

  let empty : 'a t = T.empty
  let singleton : int * 'a -> 'a t = T.singleton
  let is_empty : 'a t -> bool = T.is_empty
  let add : int * 'a -> 'a t -> 'a t = T.add

  let pop_min (q : 'a t) : (int * 'a) * 'a t =
    try T.pop_min q with Tree.Empty -> raise Empty

  let pop_min_opt (q : 'a t) : ((int * 'a) * 'a t) option =
    try Some (pop_min q) with Empty -> None
end

(* Explicit sig to prevent mutating array *)
module Grid : sig
  type t

  val in_bounds : Point.t -> t -> bool
  val heat_loss_at : Point.t -> t -> int
  val max_x : t -> int
  val max_y : t -> int
  val of_seq : int Seq.t Seq.t -> t
end = struct
  type t = {
    heat_losses : (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array2.t;
    max_x : int;
    max_y : int;
  }

  let make ~heat_losses ~max_x ~max_y = { heat_losses; max_x; max_y }
  let max_y grid = grid.max_y
  let max_x grid = grid.max_x

  let in_bounds ((x, y) : Point.t) (grid : t) : bool =
    x >= 0 && y >= 0 && x < grid.max_x && y < grid.max_y

  let heat_loss_at (point : Point.t) (grid : t) : int =
    if not (in_bounds point grid) then
      failwith @@ Fmt.str "no heat loss at %a" Point.pp point
    else Bigarray.Array2.get grid.heat_losses (Point.y point) (Point.x point)

  let reverse_arr a =
    let len = Array.length a and mid = Array.length a / 2 in
    for i = 0 to mid do
      let j = len - i - 1 in
      let a_i = a.(i) and a_j = a.(j) in
      a.(i) <- a_j;
      a.(j) <- a_i
    done

  let of_seq (seq : int Seq.t Seq.t) : t =
    let a = seq |> Seq.map Array.of_seq |> Array.of_seq in
    if Int.equal 0 (Array.length a) then failwith "Grid.of_seq with empty";

    reverse_arr a;

    let heat_losses =
      Bigarray.Array2.of_array Bigarray.int Bigarray.c_layout a
    in

    let max_y = Bigarray.Array2.dim1 heat_losses
    and max_x = Bigarray.Array2.dim2 heat_losses in

    make ~heat_losses ~max_x ~max_y
end

module Input = struct
  type t = int Seq.t Seq.t

  let int_of_char c = Int.sub (Char.code c) (Char.code '0')

  let parse_line (s : string) : int Seq.t =
    s |> String.to_seq |> Seq.map int_of_char

  let of_in_channel (chan : in_channel) : t =
    (fun () ->
      try Some (parse_line @@ input_line chan) with End_of_file -> None)
    |> Seq.of_dispenser
    |> Seq.memoize

  let of_string (s : string) : t =
    s |> String.split_on_char '\n' |> List.to_seq |> Seq.map parse_line
end

module Crucible = struct
  type t = { pos : Point.t; dir : Direction.t; steps_in_dir : int }

  let make ~pos ~dir ~steps_in_dir = { pos; dir; steps_in_dir }
  let init (pos, dir) = make ~pos ~dir ~steps_in_dir:0
  let pos crucible = crucible.pos
  let dir crucible = crucible.dir
  let steps_in_dir crucible = crucible.steps_in_dir

  let advance crucible =
    {
      crucible with
      pos = Point.move crucible.dir crucible.pos;
      steps_in_dir = Int.succ crucible.steps_in_dir;
    }

  let turn_left crucible =
    { crucible with dir = Direction.rotate_left crucible.dir; steps_in_dir = 0 }

  let turn_right crucible =
    {
      crucible with
      dir = Direction.rotate_right crucible.dir;
      steps_in_dir = 0;
    }

  let next_states crucible =
    [
      advance crucible;
      turn_left crucible |> advance;
      turn_right crucible |> advance;
    ]

  let compare (a : t) (b : t) : int =
    match Point.compare (pos a) (pos b) with
    | 0 -> (
        match Direction.compare (dir a) (dir b) with
        | 0 -> Int.compare (steps_in_dir a) (steps_in_dir b)
        | other -> other)
    | other -> other
end

module CrucibleMap = Map.Make (Crucible)
module CrucibleSet = Set.Make (Crucible)

let rec min_heat_loss_aux (grid : Grid.t) (target : Point.t) (cur_min : Int.t)
    (q : Crucible.t PQueue.t) (visited : CrucibleSet.t) =
  if PQueue.is_empty q then cur_min
  else
    let (dist_u, u), q' = PQueue.pop_min q in
    if Point.equal target (Crucible.pos u) && dist_u < cur_min then
      min_heat_loss_aux grid target dist_u q' visited
    else
      let q'', visited' =
        u
        |> Crucible.next_states
        |> List.filter (fun crucible ->
               Grid.in_bounds (Crucible.pos crucible) grid
               && Crucible.steps_in_dir crucible <= 3
               && not (CrucibleSet.mem crucible visited))
        |> List.fold_left
             (fun (q, visited) v ->
               let dist_v = dist_u + Grid.heat_loss_at (Crucible.pos v) grid in
               (PQueue.add (dist_v, v) q, CrucibleSet.add v visited))
             (q', visited)
      in
      min_heat_loss_aux grid target cur_min q'' visited'

let min_heat_loss (grid : Grid.t) (start : Crucible.t) (target : Point.t) =
  let q = PQueue.singleton (0, start)
  and visited = CrucibleSet.singleton start
  and cur_min = Int.max_int in
  min_heat_loss_aux grid target cur_min q visited |> Z.of_int

module Part1 = struct
  let run : Input.t -> Z.t =
   fun seq ->
    let grid = Grid.of_seq seq in

    let source = (0, Int.pred @@ Grid.max_y grid)
    and target = (Int.pred @@ Grid.max_x grid, 0) in

    let start = Crucible.init (source, Direction.Right) in

    min_heat_loss grid start target
end

module Part2 = struct
  let run _ = failwith "unimplemented"
end
