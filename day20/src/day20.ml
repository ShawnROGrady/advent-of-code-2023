let range ?(start = 0) stop = start |> Seq.ints |> Seq.take (stop - start)
let compose f g x = f (g x)
let ( <.> ) = compose

module StringMap = Map.Make (String)

let dump_string_map : 'a Fmt.t -> 'a StringMap.t Fmt.t =
 fun ppv ->
  let open Fmt in
  using StringMap.to_seq Dump.(seq @@ pair string ppv)

module StringSet = Set.Make (String)

let dump_string_set : StringSet.t Fmt.t =
  Fmt.using StringSet.to_seq Fmt.Dump.(seq string)

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

module PureQueue : sig
  [@@@ocaml.warning "-32"]

  type 'a t

  (* Constructors. *)
  val empty : 'a t
  val singleton : 'a -> 'a t

  (* Checks. *)
  val is_empty : 'a t -> bool
  val length : 'a t -> int
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  (* Transformations. *)
  val push : 'a -> 'a t -> 'a t
  val pop_opt : 'a t -> ('a * 'a t) option
  val pop : 'a t -> 'a * 'a t

  (* Conversions. *)
  val to_seq : 'a t -> 'a Seq.t
  val add_seq : 'a Seq.t -> 'a t -> 'a t
  val of_seq : 'a Seq.t -> 'a t
  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
end = struct
  type 'a t = { hds : 'a list; tls : 'a list }

  let empty = { hds = []; tls = [] }
  let singleton x = { hds = [ x ]; tls = [] }
  let is_empty = function { hds = []; tls = [] } -> true | _ -> false
  let length { hds; tls } = List.length hds + List.length tls

  let push x = function
    | { hds = []; tls = [] } -> singleton x
    | q -> { q with tls = x :: q.tls }

  let uncons l = (List.hd l, List.tl l)

  let pop_opt = function
    | { hds = []; tls = [] } -> None
    | { hds = x :: hds; tls } -> Some (x, { hds; tls })
    | { hds = []; tls } ->
        let x, hds = uncons @@ List.rev tls in
        Some (x, { hds; tls = [] })

  let pop q =
    match pop_opt q with
    | Some (x, q') -> (x, q')
    | None -> invalid_arg "PureQueue.pop"

  let to_seq q = Seq.unfold pop_opt q
  let to_list q = List.of_seq @@ to_seq q

  let add_list xs = function
    | { hds = []; tls = [] } -> { hds = xs; tls = [] }
    | q -> xs |> List.fold_left (Fun.flip push) q

  let add_seq xs = function
    | { hds = []; tls = [] } -> { hds = List.of_seq xs; tls = [] }
    | q -> xs |> Seq.fold_left (Fun.flip push) q

  let of_seq xs = add_seq xs empty
  let of_list xs = add_list xs empty
  let compare cmp a b = Seq.compare cmp (to_seq a) (to_seq b)
  let equal cmp a b = Seq.equal cmp (to_seq a) (to_seq b)
end

module State : sig
  [@@@ocaml.warning "-32"]

  type ('out, 'state) t

  (* Constructor *)
  val create : ('state -> 'out * 'state) -> ('out, 'state) t

  (* Core operations *)
  val run : ('out, 'state) t -> 'state -> 'out * 'state
  val put : 'state -> (unit, 'state) t
  val eval : ('out, 'state) t -> 'state -> 'out
  val exec : ('out, 'state) t -> 'state -> 'state

  (* Helpers *)
  val get : ('state -> 'b) -> ('b, 'state) t

  (* Applicative API *)
  val map : ('out -> 'b) -> ('out, 'state) t -> ('b, 'state) t
  val apply : ('a -> 'b, 'state) t -> ('a, 'state) t -> ('b, 'state) t

  (* Monadic API *)
  val bind : ('out, 'state) t -> ('out -> ('b, 'state) t) -> ('b, 'state) t
  val return : 'out -> ('out, 'state) t

  (* States and sequences *)
  val traverse : ('a -> ('out, 'state) t) -> 'a Seq.t -> ('out Seq.t, 'state) t

  val fold :
    ('out -> 'a -> ('out, 'state) t) -> 'out -> 'a Seq.t -> ('out, 'state) t

  module Syntax : sig
    val ( >>= ) : ('out, 'state) t -> ('out -> ('b, 'state) t) -> ('b, 'state) t
    val ( >>| ) : ('out, 'state) t -> ('out -> 'b) -> ('b, 'state) t
    val return : 'out -> ('out, 'state) t
  end
end = struct
  type ('out, 'state) t = { run : 'state -> 'out * 'state }

  let return (v : 'out) : ('out, 'state) t = { run = (fun s -> (v, s)) }
  let create run = { run }
  let run st = st.run
  let put state = create (fun _ -> ((), state))

  let eval p s =
    let r, _ = run p s in
    r

  let exec p s =
    let _, next = run p s in
    next

  let get : ('state -> 'b) -> ('b, 'state) t =
   fun f -> create @@ fun s -> (f s, s)

  let bind prev f =
    let next s0 =
      let x, s1 = run prev s0 in
      run (f x) s1
    in
    { run = next }

  let map f x = bind x (fun a -> return (f a))
  let apply a b = bind a (fun a2 -> bind b (fun b2 -> return (a2 b2)))

  module Syntax = struct
    let ( >>= ) = bind
    let ( >>| ) x fn = map fn x
    let ( <$> ) = map
    let ( <*> ) = apply
    let return = return
  end

  include Syntax

  let rec sequence xs =
    match Seq.uncons xs with
    | None -> return Seq.empty
    | Some (x, xs) -> Seq.cons <$> x <*> sequence xs

  let traverse f xs = sequence (Seq.map f xs)

  let fold f init xs =
    Seq.fold_left (fun cur x -> cur >>= fun cur' -> f cur' x) (return init) xs
end

module Pulse = struct
  type t = Low | High

  let equal a b = match (a, b) with Low, Low | High, High -> true | _ -> false

  let pp : t Fmt.t =
    Fmt.using (function Low -> "low" | High -> "high") Fmt.string
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
  type t = { by_name : Module.t StringMap.t; parents : StringSet.t StringMap.t }

  let by_name ms = ms.by_name
  let parents ms = ms.parents

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

    let by_name =
      labels
      |> Seq.map (function
           | ModuleLabel.Flip name, children ->
               Module.flip_flop (FlipFlop.init ~name ~children)
           | ModuleLabel.And name, children ->
               Module.conjunction
                 (Conjunction.init ~name ~children
                    ~input_names:(parents_of name))
           | ModuleLabel.Broadcaster, children ->
               Module.broadcaster (Broadcaster.init ~children))
      |> Seq.map (fun m -> (Module.name m, m))
      |> StringMap.of_seq
    in

    { by_name; parents }

  let get name (ms : t) = ms |> by_name |> StringMap.find_opt name

  let parents_of name (ms : t) =
    ms |> parents |> StringMap.find name |> StringSet.to_seq

  let has_output name (ms : t) = ms |> parents |> StringMap.mem name

  let replace (name : string) (m : Module.t) (ms : t) : t =
    ms.by_name
    |> StringMap.update name (function
         | None -> Fmt.failwith "replace %a" Fmt.Dump.string name
         | Some _ -> Some m)
    |> fun by_name -> { ms with by_name }

  let[@warning "-32"] pp : t Fmt.t =
    let open Fmt.Dump in
    record
      [
        field "by_name" by_name (dump_string_map Module.pp);
        field "parents" parents (dump_string_map dump_string_set);
      ]
end

module SentPulse = struct
  type t = { src_name : string; dst_name : string; pulse : Pulse.t }

  let make ~src_name ~dst_name ~pulse = { src_name; dst_name; pulse }
  let default = make ~src_name:"button" ~dst_name:"broadcaster" ~pulse:Pulse.Low
  let src_name p = p.src_name
  let dst_name p = p.dst_name
  let pulse p = p.pulse

  let[@warning "-32"] pp : t Fmt.t =
    let open Fmt.Dump in
    record
      [
        field "src_name" src_name string;
        field "dst_name" dst_name string;
        field "pulse" pulse Pulse.pp;
      ]
end

module PulseTemplate = struct
  type t = {
    src_name : string option;
    dst_name : string option;
    pulse : Pulse.t option;
  }

  let make ?src_name ?dst_name ?pulse () = { src_name; dst_name; pulse }
  let src_name p = p.src_name
  let dst_name p = p.dst_name
  let pulse p = p.pulse
  let option_matches cmp other = function None -> true | Some x -> cmp x other

  let field_matches cmp other_field tmp_field other tmp =
    option_matches cmp (other_field other) (tmp_field tmp)

  let ( &&. ) fl fr a b = fl a b && fr a b

  let matches : SentPulse.t -> t -> bool =
    field_matches String.equal SentPulse.src_name src_name
    &&. field_matches String.equal SentPulse.dst_name dst_name
    &&. field_matches Pulse.equal SentPulse.pulse pulse

  let[@warning "-32"] pp : t Fmt.t =
    let open Fmt.Dump in
    record
      [
        field "src_name" src_name @@ option string;
        field "dst_name" dst_name @@ option string;
        field "pulse" pulse @@ option Pulse.pp;
      ]
end

module type MODULES_STATE = sig
  type t

  val get : string -> t -> Module.t option
  val push_button : t -> SentPulse.t * t
  val replace : string -> Module.t -> t -> t
  val queue_is_empty : t -> bool
  val enqueue : SentPulse.t -> t -> t
  val dequeue : t -> (SentPulse.t * t) option
end

module MakeTransitions (S : MODULES_STATE) = struct
  type 'a t = ('a, S.t) State.t

  let get_module name : Module.t option t = State.get @@ S.get name
  let queue_is_empty : bool t = State.get @@ S.queue_is_empty

  let replace name m : unit t =
    State.create @@ fun prev -> ((), S.replace name m prev)

  let enqueue p : unit t = State.create @@ fun prev -> ((), S.enqueue p prev)

  let enqueue_seq (ps : SentPulse.t Seq.t) : unit t =
    State.fold (fun () -> enqueue) () ps

  let dequeue : SentPulse.t option t =
    State.create @@ fun prev ->
    match S.dequeue prev with
    | None -> (None, prev)
    | Some (x, cur) -> (Some x, cur)

  let _push_button : SentPulse.t t = State.create @@ S.push_button

  open State.Syntax

  let react_and_replace ~(src_name : string) ~(pulse : Pulse.t) (m : Module.t) :
      Pulse.t option t =
    let out, m' = m |> Module.react (src_name, pulse) in
    replace (Module.name m) m' >>| fun () -> out

  let next_pulses ~(src : Module.t) (emitted : Pulse.t) =
    src
    |> Module.children
    |> List.to_seq
    |> Seq.map (fun child ->
           SentPulse.make ~src_name:(Module.name src) ~dst_name:child
             ~pulse:emitted)

  let react ~(src_name : string) ~(pulse : Pulse.t) (dst : Module.t) :
      SentPulse.t Seq.t t =
    dst |> react_and_replace ~src_name ~pulse >>| function
    | None -> Seq.empty
    | Some emitted -> next_pulses ~src:dst emitted

  let push_button : unit t =
    queue_is_empty >>= function
    | true -> _push_button >>= enqueue
    | false -> failwith "push_button"

  let handle_one : SentPulse.t option t =
    let open State.Syntax in
    dequeue >>= function
    | None -> return None
    | Some { src_name; dst_name; pulse } as sent -> (
        if false then
          Format.printf "%s -%a-> %s@." src_name Pulse.pp pulse dst_name;
        get_module dst_name >>= function
        | None -> return sent
        | Some dst ->
            dst |> react ~src_name ~pulse >>= enqueue_seq >>| fun () -> sent)
end

module Part1 = struct
  type pulse_counts = { high : Z.t; low : Z.t }

  let init_pulse_counts = { high = Z.zero; low = Z.zero }

  let inc_count : Pulse.t -> pulse_counts -> pulse_counts = function
    | Pulse.High -> fun counts -> { counts with high = Z.succ counts.high }
    | Pulse.Low -> fun counts -> { counts with low = Z.succ counts.low }

  let add_pulse_counts (a : pulse_counts) (b : pulse_counts) : pulse_counts =
    { high = Z.add a.high b.high; low = Z.add a.low b.low }

  let counts_of_seq pulses =
    pulses |> Seq.fold_left (Fun.flip inc_count) init_pulse_counts

  module ModulesState = struct
    type t = { modules : Modules.t; queue : SentPulse.t PureQueue.t }

    let init modules = { modules; queue = PureQueue.empty }
    let modules state = state.modules
    let queue state = state.queue

    (* Implementing MODULES_STATE *)
    let push_button cur = (SentPulse.default, cur)
    let get name = Modules.get name <.> modules

    let replace name v state =
      { state with modules = Modules.replace name v state.modules }

    let queue_is_empty = PureQueue.is_empty <.> queue
    let enqueue v state = { state with queue = PureQueue.push v state.queue }

    let dequeue state =
      match PureQueue.pop_opt state.queue with
      | None -> None
      | Some (x, q') -> Some (x, { state with queue = q' })
  end

  module Action = MakeTransitions (ModulesState)
  open State.Syntax

  let rec consume_queue (sent : SentPulse.t PureQueue.t) :
      SentPulse.t PureQueue.t Action.t =
    Action.handle_one >>= function
    | None -> return sent
    | Some received ->
        let sent' = PureQueue.push received sent in
        consume_queue sent'

  let count_pulses_from_button : pulse_counts Action.t =
    (Action.push_button >>= fun () -> consume_queue PureQueue.empty)
    >>| fun sent ->
    sent |> PureQueue.to_seq |> Seq.map SentPulse.pulse |> counts_of_seq

  let run input =
    let ms = Modules.of_labels input in
    let init_state = ModulesState.init ms in

    let { high; low } =
      State.eval
        (range 1000
        |> State.fold
             (fun counts _ ->
               count_pulses_from_button >>| add_pulse_counts counts)
             init_pulse_counts)
        init_state
    in

    Z.mul high low
end

module Part2 = struct
  module ModulesState = struct
    type t = {
      modules : Modules.t;
      queue : SentPulse.t PureQueue.t;
      button_presses : Z.t;
    }

    let init modules =
      { modules; queue = PureQueue.empty; button_presses = Z.zero }

    let modules state = state.modules
    let queue state = state.queue
    let button_presses state = state.button_presses

    (* Implementing MODULES_STATE *)
    let push_button state =
      ( SentPulse.default,
        { state with button_presses = Z.succ state.button_presses } )

    let get name = Modules.get name <.> modules

    let replace name v state =
      { state with modules = Modules.replace name v state.modules }

    let queue_is_empty = PureQueue.is_empty <.> queue
    let enqueue v state = { state with queue = PureQueue.push v state.queue }

    let dequeue state =
      match PureQueue.pop_opt state.queue with
      | None -> None
      | Some (x, q') -> Some (x, { state with queue = q' })
  end

  module Action = struct
    include MakeTransitions (ModulesState)

    let get_button_presses : Z.t t = State.get @@ ModulesState.button_presses
  end

  open State.Syntax

  let rec consume_queue_and_search_aux acc target =
    Action.handle_one >>= function
    | None -> return acc
    | Some received ->
        let matched = PulseTemplate.matches received target in
        consume_queue_and_search_aux (acc || matched) target

  let consume_queue_and_search = consume_queue_and_search_aux false

  let press_button_and_check target : bool Action.t =
    Action.push_button >>= fun () -> consume_queue_and_search target

  let rec search target : Z.t Action.t =
    target |> press_button_and_check >>= function
    | true -> Action.get_button_presses
    | false -> search target

  let search_targets (target_name : string) (modules : Modules.t) =
    let parent =
      modules
      |> Modules.parents_of target_name
      |> Seq.map (fun parent -> Modules.get parent modules)
      |> List.of_seq
      |> function
      | [ Some (Module.Conjunction m) ] -> m
      | other ->
          Fmt.failwith "unexpected parents of %a: %a" Fmt.Dump.string
            target_name
            Fmt.Dump.(list @@ option Module.pp)
            other
    in

    modules
    |> Modules.parents_of (Conjunction.name parent)
    |> Seq.map (fun grandparent ->
           ignore grandparent;
           PulseTemplate.make ~src_name:grandparent
             ~dst_name:(Conjunction.name parent) ~pulse:Pulse.High ())

  let lcm_of_seq (xs : Z.t Seq.t) : Z.t = Seq.fold_left Z.lcm Z.one xs

  let run input =
    let target_name = "rx" in
    let modules = Modules.of_labels input in
    if not (Modules.has_output target_name modules) then
      Fmt.failwith "no output named %a" Fmt.Dump.string target_name;

    let init_state = ModulesState.init modules
    and button_presses_to target =
      search target >>= fun presses ->
      State.put (ModulesState.init modules) >>| fun () -> presses
    and targets = search_targets target_name modules in

    let min_presses =
      targets |> State.traverse button_presses_to >>| lcm_of_seq
    in

    init_state |> State.eval min_presses
end
