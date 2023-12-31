let pp_z : Z.t Fmt.t = Fmt.using Z.to_string Fmt.string

module Point = struct
  type t = Z.t * Z.t

  let x : t -> Z.t = fst
  let y : t -> Z.t = snd

  let compare (a : t) (b : t) : int =
    match Z.compare (x a) (x b) with
    | 0 -> Z.compare (y a) (y b)
    | other -> other

  let add : t -> t -> t = fun (x1, y1) (x2, y2) -> Z.(x1 + x2, y1 + y2)
  let sub : t -> t -> t = fun (x1, y1) (x2, y2) -> Z.(sub x1 x2, sub y1 y2)

  let dist : t -> t -> t =
   fun a b ->
    let dx, dy = sub a b in
    Z.(abs dx, abs dy)

  let pp : t Fmt.t = Fmt.Dump.pair pp_z pp_z
end

module PointSet = Set.Make (Point)
module ZSet = Set.Make (Z)
module ZMap = Map.Make (Z)

module Galaxy = struct
  type t = {
    planets : PointSet.t;
    rows_with_planet : ZSet.t;
    cols_with_planet : ZSet.t;
    start : Point.t;
    stop : Point.t;
    planets_by_x : Point.t list ZMap.t;
    planets_by_y : Point.t list ZMap.t;
  }

  let bounds_of_planets (planets : PointSet.t) : Point.t * Point.t =
    planets
    |> PointSet.to_seq
    |> Seq.fold_left
         (fun ((min_x, min_y), (max_x, max_y)) (x, y) ->
           ((Z.min min_x x, Z.min min_y y), (Z.max max_x x, Z.max max_y y)))
         (Z.(of_int max_int, of_int max_int), Z.(zero, zero))

  let find_rows_with_planet (planets : PointSet.t) : ZSet.t =
    planets |> PointSet.to_seq |> Seq.map Point.y |> ZSet.of_seq

  let find_cols_with_planet (planets : PointSet.t) : ZSet.t =
    planets |> PointSet.to_seq |> Seq.map Point.x |> ZSet.of_seq

  let find_planets_by_x (planets : PointSet.t) : Point.t list ZMap.t =
    planets
    |> PointSet.to_seq
    |> Seq.fold_left
         (fun acc planet ->
           acc
           |> ZMap.update (Point.x planet) (function
                | None -> Some [ planet ]
                | Some planets -> Some (planet :: planets)))
         ZMap.empty

  let find_planets_by_y (planets : PointSet.t) : Point.t list ZMap.t =
    planets
    |> PointSet.to_seq
    |> Seq.fold_left
         (fun acc planet ->
           acc
           |> ZMap.update (Point.y planet) (function
                | None -> Some [ planet ]
                | Some planets -> Some (planet :: planets)))
         ZMap.empty

  let make ~planets ~rows_with_planet ~cols_with_planet ~planets_by_x
      ~planets_by_y ~start ~stop =
    {
      planets;
      rows_with_planet;
      cols_with_planet;
      planets_by_x;
      planets_by_y;
      start;
      stop;
    }

  let create planets : t =
    let start, stop = bounds_of_planets planets in

    let rows_with_planet = find_rows_with_planet planets
    and cols_with_planet = find_cols_with_planet planets
    and planets_by_x = find_planets_by_x planets
    and planets_by_y = find_planets_by_y planets in

    make ~planets ~rows_with_planet ~cols_with_planet ~planets_by_x
      ~planets_by_y ~start ~stop

  let has_planet_in_col (x : Z.t) (galaxy : t) : bool =
    ZSet.mem x galaxy.cols_with_planet

  let has_planet_in_row (y : Z.t) (galaxy : t) : bool =
    ZSet.mem y galaxy.rows_with_planet

  let start (galaxy : t) : Point.t = galaxy.start [@@inline]
  let stop (galaxy : t) : Point.t = galaxy.stop [@@inline]
  let planets (galaxy : t) : PointSet.t = galaxy.planets [@@inline]

  let planets_with_x (x : Z.t) (galaxy : t) : Point.t Seq.t =
    match ZMap.find_opt x galaxy.planets_by_x with
    | None -> Seq.empty
    | Some l -> List.to_seq l

  let planets_with_y (y : Z.t) (galaxy : t) : Point.t Seq.t =
    match ZMap.find_opt y galaxy.planets_by_y with
    | None -> Seq.empty
    | Some l -> List.to_seq l

  let of_lines (lines : string Seq.t) : t =
    let planets =
      lines
      |> List.of_seq
      |> List.rev
      |> List.fold_left
           (fun (y, planets) line ->
             let planets' =
               line
               |> String.to_seq
               |> Seq.fold_lefti
                    (fun planets x -> function
                      | '#' -> PointSet.add Z.(of_int x, of_int y) planets
                      | _ -> planets)
                    planets
             in
             (y + 1, planets'))
           (0, PointSet.empty)
      |> snd
    in

    create planets
end

module Input = struct
  type t = Galaxy.t

  let of_in_channel (chan : in_channel) : t =
    (fun () -> try Some (input_line chan) with End_of_file -> None)
    |> Seq.of_dispenser
    |> Galaxy.of_lines

  let of_string s =
    s |> String.split_on_char '\n' |> List.to_seq |> Galaxy.of_lines
end

module PointMap = Map.Make (Point)

let adjusted_points (galaxy : Galaxy.t) (empty_scale : int) : Point.t PointMap.t
    =
  let startx = Z.to_int @@ Point.x @@ Galaxy.start galaxy
  and stopx = Z.to_int @@ Point.x @@ Galaxy.stop galaxy
  and starty = Z.to_int @@ Point.y @@ Galaxy.start galaxy
  and stopy = Z.to_int @@ Point.y @@ Galaxy.stop galaxy in

  let adjust_xs points =
    Seq.ints startx
    |> Seq.take (stopx + 1 - startx)
    |> Seq.map Z.of_int
    |> Seq.fold_left
         (fun (extra, points) x ->
           let adjustment =
             if Galaxy.has_planet_in_col x galaxy then Z.zero
             else Z.of_int (Int.pred empty_scale)
           in
           if Z.equal Z.zero extra then (Z.add extra adjustment, points)
           else
             let extra' = Z.add extra adjustment
             and points' =
               galaxy
               |> Galaxy.planets_with_x x
               |> Seq.fold_left
                    (fun points planet ->
                      points
                      |> PointMap.update planet (function
                           | None ->
                               failwith
                               @@ Fmt.str "adjust_xs %a" Point.pp planet
                           | Some p -> Some (Point.add p (extra, Z.zero))))
                    points
             in
             (extra', points'))
         (Z.zero, points)
    |> snd
  and adjust_ys points =
    Seq.ints starty
    |> Seq.take (stopy + 1 - starty)
    |> Seq.map Z.of_int
    |> Seq.fold_left
         (fun (extra, points) y ->
           let adjustment =
             if Galaxy.has_planet_in_row y galaxy then Z.zero
             else Z.of_int (Int.pred empty_scale)
           in
           if Z.equal Z.zero extra then (Z.add extra adjustment, points)
           else
             let extra' = Z.add extra adjustment
             and points' =
               galaxy
               |> Galaxy.planets_with_y y
               |> Seq.fold_left
                    (fun points planet ->
                      points
                      |> PointMap.update planet (function
                           | None ->
                               failwith
                               @@ Fmt.str "adjust_ys %a" Point.pp planet
                           | Some p -> Some (Point.add p (Z.zero, extra))))
                    points
             in
             (extra', points'))
         (Z.zero, points)
    |> snd
  in

  galaxy
  |> Galaxy.planets
  |> PointSet.to_seq
  |> Seq.map (fun p -> (p, p))
  |> PointMap.of_seq
  |> adjust_xs
  |> adjust_ys

let unique_pairs planets_seq =
  let planets : Point.t array = Array.of_seq planets_seq in
  let num_planets = Array.length planets in
  let n = num_planets - 1 in

  let num_pairs = ((n * n) + n) / 2 in

  let pairs : (Point.t * Point.t) array =
    Array.make num_pairs Z.((zero, zero), (zero, zero))
  in

  let idx = ref 0 in

  for i = 0 to num_planets - 1 do
    for j = i + 1 to num_planets - 1 do
      pairs.(!idx) <- (planets.(i), planets.(j));
      idx := !idx + 1
    done
  done;

  Array.to_seq pairs

let find_dists (galaxy : Galaxy.t) ~(empty_scale : int) :
    ((Point.t * Point.t) * Z.t) Seq.t =
  let pairs = unique_pairs (PointSet.to_seq @@ Galaxy.planets galaxy)
  and adjusted_planets = adjusted_points galaxy empty_scale in

  pairs
  |> Seq.map (fun pair ->
         let p0 = PointMap.find (fst pair) adjusted_planets
         and p1 = PointMap.find (snd pair) adjusted_planets in

         let dx, dy = Point.dist p0 p1 in
         (pair, Z.add dx dy))

module Part1 = struct
  let run : Input.t -> Z.t =
   fun galaxy ->
    galaxy
    |> find_dists ~empty_scale:2
    |> Seq.map snd
    |> Seq.fold_left Z.add Z.zero
end

module Part2 = struct
  let run ?(empty_scale : int = 1000000) (galaxy : Input.t) : Z.t =
    galaxy
    |> find_dists ~empty_scale
    |> Seq.map snd
    |> Seq.fold_left Z.add Z.zero
end
