module Point = struct
  type t = { x : int; y : int }

  let compare a b =
    match Int.compare a.x b.x with 0 -> Int.compare a.y b.y | other -> other

  let from_coords (x, y) = { x; y }

  let adjacent point =
    let d = [ -1; 0; 1 ] in
    d
    |> List.fold_left
         (fun points dx ->
           d
           |> List.fold_left
                (fun points dy ->
                  let p = { x = point.x + dx; y = point.y + dy } in
                  Seq.cons p points)
                points)
         Seq.empty
end

module PointSet = Set.Make (Point)
module IntSet = Set.Make (Int)
module PointMap = Map.Make (Point)

module Part = struct
  type t = { points : PointSet.t; value : int }

  let points part = part.points
  let value part = part.value
end

module Symbol = struct
  type t = { pos : Point.t; value : char }

  let is_gear sym = Char.compare '*' sym.value = 0

  let adjacent_points (sym : t) : PointSet.t =
    PointSet.of_seq @@ Point.adjacent sym.pos
end

module Schematic = struct
  type t = { parts : Part.t Seq.t; symbols : Symbol.t Seq.t }

  let parts scheme = scheme.parts
  let symbols scheme = scheme.symbols
  let empty = { parts = Seq.empty; symbols = Seq.empty }
  let add_part part scheme = { scheme with parts = Seq.cons part scheme.parts }

  let add_symbol symbol scheme =
    { scheme with symbols = Seq.cons symbol scheme.symbols }
end

module SchematicParser = struct
  type state = { parsed : Schematic.t; parsing : Part.t option }

  let step state (x, y) c =
    match c with
    | '.' -> (
        match state.parsing with
        | Some part ->
            { parsed = Schematic.add_part part state.parsed; parsing = None }
        | None -> state)
    | '0' .. '9' ->
        let v = Char.code c - 48 and pos = Point.from_coords (x, y) in
        let parsing' : Part.t =
          match state.parsing with
          | Some part ->
              {
                value = (10 * Part.value part) + v;
                points = PointSet.add pos (Part.points part);
              }
          | None -> { value = v; points = PointSet.singleton pos }
        in
        { state with parsing = Some parsing' }
    | _ -> (
        let sym : Symbol.t = { value = c; pos = Point.from_coords (x, y) } in
        let parsed = Schematic.add_symbol sym state.parsed in

        match state.parsing with
        | Some part ->
            { parsed = Schematic.add_part part parsed; parsing = None }
        | None -> { state with parsed })

  let parse_line init y line =
    let state =
      line
      |> String.to_seq
      |> Seq.fold_lefti (fun state x c -> step state (x, y) c) init
    in

    match state.parsing with
    | Some part ->
        { parsed = Schematic.add_part part state.parsed; parsing = None }
    | None -> state

  let empty : state = { parsed = Schematic.empty; parsing = None }

  let parse (lines : string Seq.t) : Schematic.t =
    let final = lines |> Seq.fold_lefti parse_line empty in
    final.parsed
end

module Input = struct
  type t = string Seq.t

  let of_in_channel chan =
    (fun () -> try Some (input_line chan) with End_of_file -> None)
    |> Seq.of_dispenser
    |> Seq.memoize

  let of_string s = s |> String.split_on_char '\n' |> List.to_seq
end

module Part1 = struct
  let run : Input.t -> int =
   fun lines ->
    let scheme = SchematicParser.parse lines in
    let valid_points =
      scheme
      |> Schematic.symbols
      |> Seq.fold_left
           (fun points sym ->
             PointSet.union points (Symbol.adjacent_points sym))
           PointSet.empty
    in
    scheme
    |> Schematic.parts
    |> Seq.filter_map (fun part ->
           if not (PointSet.disjoint valid_points (Part.points part)) then
             Some (Part.value part)
           else None)
    |> Seq.fold_left ( + ) 0
end

module Part2 = struct
  let parts_by_pos scheme =
    scheme
    |> Schematic.parts
    |> Seq.fold_left
         (fun acc part ->
           acc
           |> PointMap.add_seq
                (part
                |> Part.points
                |> PointSet.to_seq
                |> Seq.map (fun point -> (point, Part.value part))))
         PointMap.empty

  let adjacent_parts parts sym =
    sym
    |> Symbol.adjacent_points
    |> PointSet.to_seq
    |> Seq.filter_map (fun point -> PointMap.find_opt point parts)
    |> IntSet.of_seq

  let run : Input.t -> int =
   fun lines ->
    let scheme = SchematicParser.parse lines in
    let gears = scheme |> Schematic.symbols |> Seq.filter Symbol.is_gear
    and parts = parts_by_pos scheme in

    gears
    |> Seq.filter_map (fun gear ->
           match IntSet.elements (adjacent_parts parts gear) with
           | [ x; y ] -> Some (x * y)
           | _ -> None)
    |> Seq.fold_left ( + ) 0
end
