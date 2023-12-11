module Direction = struct
  type t = Left | Right

  let of_char = function
    | 'L' -> Left
    | 'R' -> Right
    | other ->
        raise
        @@ Invalid_argument (Printf.sprintf "unknown direction '%c'" other)
end

module Input = struct
  type node = string * (string * string)
  type t = Direction.t Seq.t * node Seq.t

  let parse_directions (s : string) : Direction.t Seq.t =
    s |> String.to_seq |> Seq.map Direction.of_char

  let parse_node (ic : Scanf.Scanning.in_channel) : node =
    Scanf.bscanf ic "%s = (%s@, %s@)\n" (fun k l r -> (k, (l, r)))

  let parse_nodes (ic : Scanf.Scanning.in_channel) : node Seq.t =
    let f () = try Some (parse_node ic) with End_of_file -> None in
    Seq.memoize @@ Seq.of_dispenser f

  let of_in_channel (ic : in_channel) : t =
    let direction_part = input_line ic in
    ignore (input_line ic);

    ( parse_directions direction_part,
      parse_nodes (Scanf.Scanning.from_channel ic) )

  let split_once re s =
    match Str.bounded_split re s 2 with
    | [ a; b ] -> (a, b)
    | _ -> failwith "unreachable"

  let newlines = Str.regexp "\n\n"

  let of_string (s : string) : t =
    let direction_part, nodes_part = split_once newlines s in
    ( parse_directions direction_part,
      parse_nodes (Scanf.Scanning.from_string nodes_part) )
end

module StringMap = Map.Make (String)

module Graph = struct
  type t = (string * string) StringMap.t

  let of_seq : (string * (string * string)) Seq.t -> t = StringMap.of_seq

  let get (k : string) (d : Direction.t) (g : t) : string =
    let l, r = StringMap.find k g in
    match d with Direction.Left -> l | Direction.Right -> r
end

let uncons (x : 'a Seq.t) : 'a * 'a Seq.t =
  match x () with
  | Seq.Cons (y, ys) -> (y, ys)
  | Seq.Nil -> raise (Invalid_argument "uncons empty seq")

module Walker = struct
  type t = { graph : Graph.t; directions : Direction.t Seq.t }

  let make ~graph ~directions = { graph; directions }

  let steps ~start ~stop (walker : t) : int =
    let g = walker.graph in
    let rec loop cur_steps cur_key ds =
      if String.equal cur_key stop then cur_steps
      else
        let d, ds' = uncons ds in
        let next_key = Graph.get cur_key d g
        and cur_steps' = Int.succ cur_steps in
        loop cur_steps' next_key ds'
    in
    loop 0 start (Seq.cycle walker.directions)
end

module Part1 = struct
  let run : Input.t -> int =
   fun (directions, nodes) ->
    let graph = Graph.of_seq nodes in
    let walker = Walker.make ~graph ~directions in
    walker |> Walker.steps ~start:"AAA" ~stop:"ZZZ"
end

module Part2 = struct
  let run _ = failwith "unimplemented"
end
