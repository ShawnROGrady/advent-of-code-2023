module Color = struct
  type t = Red | Green | Blue

  let as_int = function Red -> 0 | Green -> 1 | Blue -> 2
  let compare a b = Int.compare (as_int a) (as_int b)

  let of_string = function
    | "red" -> Red
    | "green" -> Green
    | "blue" -> Blue
    | other -> failwith @@ Printf.sprintf "unknown color: %s" other
end

module ColorMap = Map.Make (Color)

module Totals = struct
  type t = int ColorMap.t

  let add color count totals =
    totals
    |> ColorMap.update color (function
         | None -> Some count
         | Some old -> Some (old + count))

  let of_seq : (Color.t * int) Seq.t -> t =
    Seq.fold_left
      (fun totals (color, count) -> add color count totals)
      ColorMap.empty

  let of_list : (Color.t * int) list -> t = fun x -> of_seq @@ List.to_seq x

  let get color totals =
    totals |> ColorMap.find_opt color |> Option.value ~default:0
end

module Game = struct
  type t = { id : int; rolls : (Color.t * int) list list }

  let id (game : t) : int = game.id
  let rolls (game : t) : (Color.t * int) list list = game.rolls

  let parse_cube_count s =
    match String.split_on_char ' ' s with
    | [ a; b ] -> (Color.of_string b, int_of_string a)
    | _ -> raise @@ Invalid_argument s

  let parse_game_id s =
    match String.split_on_char ' ' s with
    | [ "Game"; x ] -> int_of_string x
    | _ -> raise @@ Invalid_argument s

  let cubes_sep = Str.regexp ", "
  let rolls_sep = Str.regexp "; "

  let of_string (s : string) : t =
    let def, contents =
      match String.split_on_char ':' s with
      | [ x; y ] -> (String.trim x, String.trim y)
      | _ -> raise @@ Invalid_argument s
    in

    let id = parse_game_id def
    and rolls =
      contents
      |> Str.split rolls_sep
      |> List.map (fun roll ->
             roll |> Str.split cubes_sep |> List.map parse_cube_count)
    in

    { id; rolls }
end

module Input = struct
  type t = Game.t Seq.t

  let parse_line = Game.of_string

  let of_in_channel chan =
    (fun () ->
      try Some (parse_line @@ input_line chan) with End_of_file -> None)
    |> Seq.of_dispenser
    |> Seq.memoize

  let of_string s =
    s |> String.split_on_char '\n' |> List.to_seq |> Seq.map parse_line
end

module Part1 = struct
  let roll_is_possible roll =
    let totals = Totals.of_list roll in
    let total color = Totals.get color totals in

    total Color.Red <= 12 && total Color.Green <= 13 && total Color.Blue <= 14

  let is_possible game =
    let rec loop = function
      | [] -> true
      | roll :: rest -> if roll_is_possible roll then loop rest else false
    in

    loop (Game.rolls game)

  let run : Input.t -> int =
   fun games ->
    games
    |> Seq.filter_map (fun game ->
           if is_possible game then Some (Game.id game) else None)
    |> Seq.fold_left ( + ) 0
end

module Part2 = struct
  let run _ = failwith "unimplemented"
end
