let range ?(start = 0) stop = start |> Seq.ints |> Seq.take (stop - start)

module StringMap = Map.Make (String)

let dump_string_map : 'a Fmt.t -> 'a StringMap.t Fmt.t =
 fun ppv ->
  let open Fmt in
  using StringMap.to_seq Dump.(seq @@ pair string ppv)

module StringSet = Set.Make (String)

let drop_first_char s = String.sub s 1 (Int.pred @@ String.length s)

module ModuleLabel = struct
  type t = Flip of string | And of string | Broadcaster

  let of_string = function
    | "broadcaster" -> Broadcaster
    | other ->
        if String.starts_with ~prefix:"%" other then
          Flip (drop_first_char other)
        else if String.starts_with ~prefix:"&" other then
          And (drop_first_char other)
        else Fmt.invalid_arg "unknown module label %a" Fmt.Dump.string other
end

module Input = struct
  type line = ModuleLabel.t * string list
  type t = line Seq.t

  let arrow = Str.regexp " -> "
  let comma = Str.regexp ", "

  let parse_line (s : string) : line =
    match Str.split arrow s with
    | [ s1; s2 ] -> (ModuleLabel.of_string s1, Str.split comma s2)
    | _ -> Fmt.failwith "parse_line: %a" Fmt.Dump.string s

  let parse_lines lines =
    lines |> Seq.filter (Fun.negate @@ String.equal "") |> Seq.map parse_line

  let of_string (s : string) : t =
    s |> String.split_on_char '\n' |> List.to_seq |> parse_lines

  let of_in_channel (ic : in_channel) : t =
    ic |> In_channel.input_all |> of_string
end

module Pulse = struct
  type t = Low | High

  let equal a b = match (a, b) with Low, Low | High, High -> true | _ -> false

  let pp : t Fmt.t =
    Fmt.using (function Low -> "Low" | High -> "High") Fmt.string
end

module FlipFlop = struct
  type t = { name : string; on : bool; children : string list }

  let make ~name ~on ~children = { name; on; children }
  let init ~name ~children : t = make ~name ~on:false ~children
  let name m = m.name
  let on m = m.on
  let off = Fun.negate on
  let children m = m.children
  let on_high cur = (None, cur)

  let on_low cur =
    if off cur then (Some Pulse.High, { cur with on = true })
    else (Some Pulse.Low, { cur with on = false })

  let react : string * Pulse.t -> t -> Pulse.t option * t = function
    | _, Pulse.High -> on_high
    | _, Pulse.Low -> on_low

  let pp : t Fmt.t =
    let open Fmt in
    let open Dump in
    record
      [
        field "name" name string;
        field "on" on bool;
        field "children" children (list string);
      ]
end

module Conjunction = struct
  type t = {
    name : string;
    last_pulses : Pulse.t StringMap.t;
    children : string list;
  }

  let make ~name ~last_pulses ~children = { name; last_pulses; children }

  let init ~name ~children ~(input_names : string list) =
    let last_pulses =
      input_names
      |> List.to_seq
      |> Seq.map (fun name -> (name, Pulse.Low))
      |> StringMap.of_seq
    in
    make ~name ~children ~last_pulses

  let name m = m.name
  let last_pulses m = m.last_pulses
  let children m = m.children

  let inputs_all_high cur =
    cur
    |> last_pulses
    |> StringMap.to_seq
    |> Seq.map snd
    |> Seq.for_all Pulse.(equal High)

  let remember input_name pulse cur =
    let last_pulses' =
      cur
      |> last_pulses
      |> StringMap.update input_name (function
           | None ->
               Fmt.failwith "conjunction %a has no input %a" Fmt.Dump.string
                 (name cur) Fmt.Dump.string input_name
           | Some _ -> Some pulse)
    in
    { cur with last_pulses = last_pulses' }

  let react ((input_name, pulse) : string * Pulse.t) (cur : t) :
      Pulse.t option * t =
    let cur' = remember input_name pulse cur in
    let out = if inputs_all_high cur' then Pulse.Low else Pulse.High in
    (Some out, cur')

  let pp : t Fmt.t =
    let open Fmt.Dump in
    record
      [
        field "name" name string;
        field "last_pulses" last_pulses (dump_string_map Pulse.pp);
        field "children" children (list string);
      ]
end

module Broadcaster = struct
  type t = { children : string list }

  let make ~children = { children }
  let init = make
  let children m = m.children
  let name : t -> string = Fun.const "broadcaster"

  let react : string * Pulse.t -> t -> Pulse.t option * t =
   fun (_, pulse) cur -> (Some pulse, cur)

  let pp : t Fmt.t =
    let open Fmt.Dump in
    record [ field "name" name string; field "children" children (list string) ]
end

module Module = struct
  type t =
    | FlipFlop of FlipFlop.t
    | Conjunction of Conjunction.t
    | Broadcaster of Broadcaster.t

  let flip_flop m = FlipFlop m
  let conjunction m = Conjunction m
  let broadcaster m = Broadcaster m

  let name : t -> string = function
    | FlipFlop m -> FlipFlop.name m
    | Conjunction m -> Conjunction.name m
    | Broadcaster m -> Broadcaster.name m

  let children : t -> string list = function
    | FlipFlop m -> FlipFlop.children m
    | Conjunction m -> Conjunction.children m
    | Broadcaster m -> Broadcaster.children m

  let react : string * Pulse.t -> t -> Pulse.t option * t =
   fun input -> function
    | FlipFlop m ->
        let out, next = FlipFlop.react input m in
        (out, FlipFlop next)
    | Conjunction m ->
        let out, next = Conjunction.react input m in
        (out, Conjunction next)
    | Broadcaster m ->
        let out, next = Broadcaster.react input m in
        (out, Broadcaster next)

  let pp : t Fmt.t =
   fun ppf v ->
    let open Fmt in
    match v with
    | FlipFlop m -> parens (any "FlipFlop" ++ sp ++ FlipFlop.pp) ppf m
    | Conjunction m -> parens (any "Conjunction" ++ sp ++ Conjunction.pp) ppf m
    | Broadcaster m -> parens (any "Broadcaster" ++ sp ++ Broadcaster.pp) ppf m
end

module Modules = struct
  type t = Module.t StringMap.t

  let add_parent ~(child : string) ~(parent : string)
      (parents : StringSet.t StringMap.t) =
    parents
    |> StringMap.update child (function
         | None -> Some (StringSet.singleton parent)
         | Some existing -> Some (StringSet.add parent existing))

  let add_parent_for_each ~(children : string list) ~(parent : string)
      (parents : StringSet.t StringMap.t) =
    children
    |> List.fold_left (fun acc child -> add_parent ~child ~parent acc) parents

  let key_of_module_label = function
    | ModuleLabel.Flip s -> s
    | ModuleLabel.And s -> s
    | ModuleLabel.Broadcaster -> "broadcaster"

  let of_labels (lines : (ModuleLabel.t * string list) Seq.t) : t =
    let labels = Seq.memoize lines in
    let parents =
      labels
      |> Seq.fold_left
           (fun parents (label, children) ->
             let name = key_of_module_label label in
             let parents' =
               add_parent_for_each ~children ~parent:name parents
             in
             parents')
           StringMap.empty
    in

    let parents_of k =
      parents |> StringMap.find k |> StringSet.to_seq |> List.of_seq
    in

    labels
    |> Seq.map (function
         | ModuleLabel.Flip name, children ->
             Module.flip_flop (FlipFlop.init ~name ~children)
         | ModuleLabel.And name, children ->
             Module.conjunction
               (Conjunction.init ~name ~children ~input_names:(parents_of name))
         | ModuleLabel.Broadcaster, children ->
             Module.broadcaster (Broadcaster.init ~children))
    |> Seq.map (fun m -> (Module.name m, m))
    |> StringMap.of_seq

  let get name (ms : t) = StringMap.find_opt name ms

  let replace (name : string) (m : Module.t) (ms : t) : t =
    ms
    |> StringMap.update name (function
         | None -> Fmt.failwith "replace %a" Fmt.Dump.string name
         | Some _ -> Some m)

  let[@warning "-32"] pp : t Fmt.t = dump_string_map Module.pp
end

module Part1 = struct
  type pulse_counts = { high : Z.t; low : Z.t }

  let init_pulse_counts = { high = Z.zero; low = Z.zero }

  let inc_count : Pulse.t -> pulse_counts -> pulse_counts = function
    | Pulse.High -> fun counts -> { counts with high = Z.succ counts.high }
    | Pulse.Low -> fun counts -> { counts with low = Z.succ counts.low }

  let add_pulse_counts (a : pulse_counts) (b : pulse_counts) : pulse_counts =
    { high = Z.add a.high b.high; low = Z.add a.low b.low }

  type q_item = { src_name : string; dst_name : string; pulse : Pulse.t }

  let rec push_button_aux (q : q_item Queue.t) (ms : Modules.t)
      (counts : pulse_counts) =
    match Queue.take_opt q with
    | None -> (counts, ms)
    | Some { src_name; dst_name; pulse } -> (
        let counts' = inc_count pulse counts in

        match Modules.get dst_name ms with
        | None -> push_button_aux q ms counts'
        | Some dst ->
            let dst' =
              dst |> Module.react (src_name, pulse) |> function
              | None, m -> m
              | Some next_pulse, m ->
                  dst
                  |> Module.children
                  |> List.to_seq
                  |> Seq.map (fun child ->
                         {
                           src_name = dst_name;
                           dst_name = child;
                           pulse = next_pulse;
                         })
                  |> Queue.add_seq q;
                  m
            in

            let ms' = Modules.replace dst_name dst' ms in
            push_button_aux q ms' counts')

  let push_button (ms : Modules.t) : pulse_counts * Modules.t =
    let q = Queue.create () and counts = init_pulse_counts in

    Queue.add { src_name = ""; dst_name = "broadcaster"; pulse = Pulse.Low } q;

    push_button_aux q ms counts

  let run input =
    let ms = Modules.of_labels input in
    let { high; low } =
      range 1000
      |> Seq.fold_left
           (fun (counts, state) _ ->
             let new_counts, state' = push_button state in
             (add_pulse_counts counts new_counts, state'))
           (init_pulse_counts, ms)
      |> fst
    in

    Z.mul high low
end

module Part2 = struct
  let run _ = failwith "unimplemented"
end
