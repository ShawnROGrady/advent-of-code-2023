let range ?(start = 0) stop = start |> Seq.ints |> Seq.take (stop - start)

module Direction = struct
  type t = Up | Down | Left | Right

  let[@inline] as_int = function Up -> 0 | Down -> 1 | Left -> 2 | Right -> 3
  let[@inline] compare a b : int = Int.compare (as_int a) (as_int b)

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
end

module Point = struct
  type t = int * int

  let x : t -> int = fst
  let y : t -> int = snd

  let[@inline] compare a b : int =
    match Int.compare (x a) (x b) with
    | 0 -> Int.compare (y a) (y b)
    | other -> other

  let[@inline] equal a b = Int.equal 0 (compare a b)
  let[@inline] add (a : t) (b : t) : t = (x a + x b, y a + y b)
  let[@inline] sub (a : t) (b : t) : t = (x a - x b, y a - y b)
  let[@inline] mul scale ((x, y) : t) : t = (scale * x, scale * y)

  let[@inline] of_direction : Direction.t -> t = function
    | Direction.Up -> (0, 1)
    | Direction.Down -> (0, -1)
    | Direction.Left -> (-1, 0)
    | Direction.Right -> (1, 0)

  let move : Direction.t -> t -> t = fun d -> add (of_direction d)
  let pp : t Fmt.t = Fmt.Dump.pair Fmt.int Fmt.int
end

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module PQueue = struct
  exception Empty

  module type S = sig
    type data
    type elem = int * data
    type t

    val empty : t

    (* Checks *)
    val is_empty : t -> bool

    (* Transformations *)
    val add : elem -> t -> t
    val pop_min : t -> elem * t
  end

  module Make (Ord : OrderedType) : S with type data = Ord.t = struct
    type data = Ord.t

    module Elem = struct
      type t = int * data

      let priority : t -> int = fst
      let data : t -> data = snd

      let compare (a : t) (b : t) : int =
        match Int.compare (priority a) (priority b) with
        | 0 -> Ord.compare (data a) (data b)
        | other -> other
    end

    type elem = Elem.t

    module Elems = Set.Make (Elem)

    type t = Elems.t

    let empty : t = Elems.empty
    let is_empty : t -> bool = Elems.is_empty
    let add : Elem.t -> t -> t = Elems.add

    let pop_min (q : t) =
      try
        let min_elem = Elems.min_elt q in
        (min_elem, Elems.remove min_elem q)
      with Invalid_argument _ -> raise Empty
  end
end

(* Explicit sig to prevent mutating array *)
module Grid : sig
  type t

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

  let[@inline] in_bounds ((x, y) : Point.t) (grid : t) : bool =
    x >= 0 && y >= 0 && x < grid.max_x && y < grid.max_y

  let heat_loss_at (point : Point.t) (grid : t) : int =
    if not (in_bounds point grid) then
      raise @@ Invalid_argument (Fmt.str "no heat loss at %a" Point.pp point)
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

module type CRUCIBLE = sig
  type t

  val init : Point.t * Direction.t -> t
  val pos : t -> Point.t
  val dir : t -> Direction.t
  val heat_loss : t -> int
  val steps_in_dir : t -> int
  val next_states : Grid.t -> t -> t Seq.t
end

module Path (Crucible : CRUCIBLE) = struct
  let compare_crucibles (a : Crucible.t) (b : Crucible.t) : int =
    let open Crucible in
    match Point.compare (pos a) (pos b) with
    | 0 -> (
        match Direction.compare (dir a) (dir b) with
        | 0 -> Int.compare (steps_in_dir a) (steps_in_dir b)
        | other -> other)
    | other -> other

  module OrderedCrucible = struct
    type t = Crucible.t

    let compare = compare_crucibles
  end

  module CrucibleSet = Set.Make (OrderedCrucible)
  module Q = PQueue.Make (OrderedCrucible)

  let rec min_heat_loss_aux (grid : Grid.t) (target : Point.t) (cur_min : Int.t)
      (q : Q.t) (visited : CrucibleSet.t) =
    if Q.is_empty q then cur_min
    else
      let (dist_u, u), q' = Q.pop_min q in
      if dist_u >= cur_min then
        (min_heat_loss_aux [@tailcall]) grid target cur_min q' visited
      else if Point.equal target (Crucible.pos u) then
        let cur_min' = Int.min cur_min dist_u in
        (min_heat_loss_aux [@tailcall]) grid target cur_min' q' visited
      else
        let q'', visited' =
          u
          |> Crucible.next_states grid
          |> Seq.fold_left
               (fun (q, visited) v ->
                 let dx, dy = Point.sub target (Crucible.pos v) in
                 let dist_v = Crucible.heat_loss v in
                 if dist_v + Int.abs dx + Int.abs dy >= cur_min then (q, visited)
                 else if CrucibleSet.mem v visited then (q, visited)
                 else (Q.add (dist_v, v) q, CrucibleSet.add v visited))
               (q', visited)
        in
        (min_heat_loss_aux [@tailcall]) grid target cur_min q'' visited'

  let min_heat_loss (grid : Grid.t) (source : Point.t) (target : Point.t) =
    let start1, start2 =
      ( Crucible.init (source, Direction.Right),
        Crucible.init (source, Direction.Down) )
    in
    let q = Q.empty |> Q.add (0, start1) |> Q.add (0, start2)
    and visited =
      CrucibleSet.empty |> CrucibleSet.add start1 |> CrucibleSet.add start2
    and cur_min = Int.max_int in
    min_heat_loss_aux grid target cur_min q visited |> Z.of_int
end

module BaseCrucible = struct
  type t = {
    pos : Point.t;
    dir : Direction.t;
    steps_in_dir : int;
    heat_loss : int;
  }

  let make ~pos ~dir ~steps_in_dir ~heat_loss =
    { pos; dir; steps_in_dir; heat_loss }

  let init (pos, dir) = make ~pos ~dir ~steps_in_dir:0 ~heat_loss:0
  let pos crucible = crucible.pos
  let dir crucible = crucible.dir
  let steps_in_dir crucible = crucible.steps_in_dir
  let heat_loss crucible = crucible.heat_loss

  let advance grid crucible =
    let pos' = Point.move (dir crucible) (pos crucible)
    and steps_in_dir' = Int.succ crucible.steps_in_dir in
    let heat_loss' = heat_loss crucible + Grid.heat_loss_at pos' grid in
    {
      crucible with
      pos = pos';
      steps_in_dir = steps_in_dir';
      heat_loss = heat_loss';
    }

  let advance_by_aux n grid crucible =
    let d = dir crucible in
    let visited =
      range n |> Seq.scan (fun point _ -> Point.move d point) (pos crucible)
    in
    let pos' = Point.add (pos crucible) (Point.mul n (Point.of_direction d))
    and heat_loss' =
      visited
      |> Seq.drop 1
      |> Seq.fold_left
           (fun loss point -> loss + Grid.heat_loss_at point grid)
           (heat_loss crucible)
    in
    {
      crucible with
      pos = pos';
      steps_in_dir = n + steps_in_dir crucible;
      heat_loss = heat_loss';
    }

  let advance_by = function 1 -> advance | other -> advance_by_aux other

  let advance_opt grid crucible =
    try Some (advance grid crucible) with Invalid_argument _ -> None

  let advance_by_opt n grid crucible =
    try Some (advance_by n grid crucible) with Invalid_argument _ -> None

  let turn_left crucible =
    { crucible with dir = Direction.rotate_left crucible.dir; steps_in_dir = 0 }

  let turn_right crucible =
    {
      crucible with
      dir = Direction.rotate_right crucible.dir;
      steps_in_dir = 0;
    }
end

module Crucible = struct
  include BaseCrucible

  let orientations : (t -> t) list = [ turn_left; turn_right; Fun.id ]
  let turns : (t -> t) list = [ turn_left; turn_right ]

  let next_states_aux grid crucible turns =
    turns
    |> List.fold_left
         (fun states turn ->
           match advance_opt grid (turn crucible) with
           | None -> states
           | Some crucible' -> Seq.cons crucible' states)
         Seq.empty

  let next_states grid crucible : t Seq.t =
    if steps_in_dir crucible >= 3 then next_states_aux grid crucible turns
    else next_states_aux grid crucible orientations
end

module Part1 = struct
  module P = Path (Crucible)

  let run : Input.t -> Z.t =
   fun seq ->
    let grid = Grid.of_seq seq in

    let source = (0, Int.pred @@ Grid.max_y grid)
    and target = (Int.pred @@ Grid.max_x grid, 0) in

    P.min_heat_loss grid source target
end

module UltraCrucible = struct
  include BaseCrucible

  let advance_to_stop grid crucible =
    advance_by_opt (Int.max 1 (4 - steps_in_dir crucible)) grid crucible

  let orientations : (t -> t) list = [ turn_left; turn_right; Fun.id ]
  let turns : (t -> t) list = [ turn_left; turn_right ]

  let next_states_aux grid crucible turns =
    turns
    |> List.fold_left
         (fun states turn ->
           match advance_to_stop grid (turn crucible) with
           | None -> states
           | Some crucible' -> Seq.cons crucible' states)
         Seq.empty

  let next_states grid crucible : t Seq.t =
    if steps_in_dir crucible < 4 then
      match advance_to_stop grid crucible with
      | None -> Seq.empty
      | Some next -> Seq.return next
    else if steps_in_dir crucible >= 10 then next_states_aux grid crucible turns
    else next_states_aux grid crucible orientations
end

module Part2 = struct
  module P = Path (UltraCrucible)

  let run : Input.t -> Z.t =
   fun seq ->
    let grid = Grid.of_seq seq in

    let source = (0, Int.pred @@ Grid.max_y grid)
    and target = (Int.pred @@ Grid.max_x grid, 0) in

    P.min_heat_loss grid source target
end
