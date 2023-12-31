module Race = struct
  type t = { time : float; distance : float }

  let make ~time ~distance = { time; distance }
  let time race = race.time
  let distance race = race.distance
end

module Input = struct
  type t = Race.t list

  let sep = Str.regexp "[ \t]+"

  let parse_line line =
    let to_parse =
      match String.split_on_char ':' line with
      | [ _; nums ] -> nums
      | other ->
          failwith
          @@ Printf.sprintf "line should have 2 parts; got %d"
               (List.length other)
    in

    to_parse |> Str.split sep |> List.map float_of_string

  let parse_lines = function
    | [ times_line; dists_line ] ->
        let times = parse_line times_line and dists = parse_line dists_line in
        List.map2 (fun time distance -> Race.make ~time ~distance) times dists
    | other ->
        failwith
        @@ Printf.sprintf "should have 2 lines; got %d" (List.length other)

  let of_string s = s |> String.trim |> String.split_on_char '\n' |> parse_lines
  let of_in_channel chan = chan |> In_channel.input_all |> of_string
end

let ( +- ) x y = (x +. y, x -. y)

let quadratic_roots a b c =
  let plus, minus =
    Float.neg b +- Float.sqrt (Float.pow b 2. -. (4. *. a *. c))
  in
  (plus /. (2. *. a), minus /. (2. *. a))

let ways_to_win race =
  let roots =
    quadratic_roots Float.minus_one (Race.time race)
      (Float.neg @@ Race.distance race)
  in
  let min_hold = Float.floor @@ fst roots
  and max_hold = Float.ceil @@ snd roots in
  Int.pred @@ Float.to_int (max_hold -. min_hold)

module Part1 = struct
  let run : Input.t -> int =
   fun races -> races |> List.map ways_to_win |> List.fold_left ( * ) 1
end

module Part2 = struct
  let join_races races =
    let ( ^. ) a b = (Int.to_string @@ Float.to_int a) ^ b in

    let time_s, dist_s =
      List.fold_right
        (fun race (t, d) -> (Race.time race ^. t, Race.distance race ^. d))
        races ("", "")
    in

    let time = float_of_string time_s and distance = float_of_string dist_s in

    Race.make ~time ~distance

  let run : Input.t -> int = fun races -> races |> join_races |> ways_to_win
end
