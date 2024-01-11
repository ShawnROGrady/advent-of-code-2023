let compose f g x = f (g x)
let ( @. ) = compose
let pp_z : Z.t Fmt.t = Fmt.using Z.to_string Fmt.string

module Category = struct
  type t = X | M | A | S

  let of_string = function
    | "x" -> X
    | "m" -> M
    | "a" -> A
    | "s" -> S
    | other -> Fmt.invalid_arg "invalid category: %a" Fmt.Dump.string other

  let to_string = function X -> "x" | M -> "m" | A -> "a" | S -> "s"
  let all = [ X; M; A; S ]

  let pp : t Fmt.t =
    Fmt.using
      (function
        | X -> "Category.X"
        | M -> "Category.M"
        | A -> "Category.A"
        | S -> "Category.S")
      Fmt.string
end

module Part = struct
  type 'a t = { x : 'a; m : 'a; a : 'a; s : 'a }

  let make ~x ~m ~a ~s = { x; m; a; s }
  let x part = part.x
  let m part = part.m
  let a part = part.a
  let s part = part.s

  let get : Category.t -> 'a t -> 'a = function
    | Category.X -> x
    | Category.M -> m
    | Category.A -> a
    | Category.S -> s

  let with_x x part = { part with x }
  let with_m m part = { part with m }
  let with_a a part = { part with a }
  let with_s s part = { part with s }

  let replace : Category.t -> 'a -> 'a t -> 'a t = function
    | Category.X -> with_x
    | Category.M -> with_m
    | Category.A -> with_a
    | Category.S -> with_s

  let pp : 'a Fmt.t -> 'a t Fmt.t =
   fun ppv ->
    let pp_field cat = Fmt.Dump.field (Category.to_string cat) (get cat) ppv in

    Category.all |> List.map pp_field |> Fmt.Dump.record
end

module PartStatus = struct
  type t = Accepted | Rejected

  let pp : t Fmt.t =
    Fmt.using
      (function Accepted -> "Accepted" | Rejected -> "Rejected")
      Fmt.string
end

module RuleResult = struct
  type t = Resolved of PartStatus.t | Pending of string

  let of_string = function
    | "" -> invalid_arg "rule result of empty string"
    | "A" -> Resolved PartStatus.Accepted
    | "R" -> Resolved PartStatus.Rejected
    | other -> Pending other

  let pp : t Fmt.t =
   fun ppf -> function
    | Resolved st -> Fmt.pf ppf "(Resolved %a)" PartStatus.pp st
    | Pending next -> Fmt.pf ppf "(Pending %a)" Fmt.Dump.string next
end

module Operator = struct
  type t = Lt | Gt | Le | Ge

  let inv = function Lt -> Ge | Le -> Gt | Gt -> Le | Ge -> Lt

  let pp_expr : t Fmt.t =
    Fmt.string
    |> Fmt.using (function Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">=")
end

module Expr = struct
  type cond = { op : Operator.t; lhs : Category.t; rhs : Z.t }
  type t = If of cond * t * t | Return of RuleResult.t

  let if_ cond t e : t = If (cond, t, e)
  let return v : t = Return v
  let ( > ) lhs rhs = { op = Operator.Gt; lhs; rhs }
  let ( < ) lhs rhs = { op = Operator.Lt; lhs; rhs }

  let pp_cond : cond Fmt.t =
   fun ppf v ->
    Fmt.pf ppf "(%a %a %a)" Category.pp v.lhs Operator.pp_expr v.op pp_z v.rhs

  let rec pp ppf expr =
    let open Fmt in
    match expr with
    | Return v -> (parens (any "return" ++ sp ++ RuleResult.pp)) ppf v
    | If (cond, t, e) ->
        (parens
           (any "if_"
           ++ sp
           ++ using fst (box pp_cond)
           ++ sp
           ++ using (fst @. snd) (box pp)
           ++ sp
           ++ using (snd @. snd) (box pp)))
          ppf
          (cond, (t, e))
end

module Workflow = struct
  type t = { name : string; rules : Expr.t }

  let make ~name ~rules = { name; rules }
  let name wf = wf.name
  let rules wf = wf.rules

  let pp : t Fmt.t =
    let open Fmt.Dump in
    record [ field "name" name string; field "rules" rules Expr.pp ]
end

module Input = struct
  type t = { workflows : Workflow.t list; parts : Z.t Part.t Seq.t }
  type scan_ic = Scanf.Scanning.in_channel

  let read_z (ic : scan_ic) : Z.t = Scanf.bscanf ic "%d" Z.of_int

  let rec parse_rules_aux = function
    | [] -> failwith "parse_rules_aux"
    | [ last ] -> Expr.return (RuleResult.of_string last)
    | to_parse :: rest ->
        let open Expr in
        Scanf.sscanf to_parse "%1[xmas]%1[<>]%r:%[a-zA-Z]" read_z
        @@ fun lhs_s op_s rhs res_s ->
        let part_field = Category.of_string lhs_s
        and res = RuleResult.of_string res_s in
        let cond =
          match op_s with
          | ">" -> part_field > rhs
          | "<" -> part_field < rhs
          | other -> Fmt.invalid_arg "unrecognized op=%a" Fmt.Dump.string other
        in

        if_ cond (return res) (parse_rules_aux rest)

  let comma = Str.regexp ","
  let parse_rules s = s |> Str.split comma |> parse_rules_aux

  let read_workflow (ic : scan_ic) =
    Scanf.bscanf ic "%[a-zA-Z]{%s@}\n" (fun name to_parse ->
        let rules = parse_rules to_parse in
        Workflow.make ~name ~rules)

  let read_workflows (ic : scan_ic) : Workflow.t Seq.t =
    let dispense () =
      try Some (read_workflow ic) with Scanf.Scan_failure _ -> None
    in
    Seq.of_dispenser dispense

  let read_part (ic : scan_ic) =
    Scanf.bscanf ic "{x=%r,m=%r,a=%r,s=%r} " read_z read_z read_z read_z
      (fun x m a s -> Part.make ~x ~m ~a ~s)

  let read_parts (ic : scan_ic) : Z.t Part.t Seq.t =
    let dispense () = try Some (read_part ic) with End_of_file -> None in
    Seq.memoize @@ Seq.of_dispenser dispense

  let of_scanning_ic (ic : scan_ic) : t =
    let workflows = List.of_seq @@ read_workflows ic in
    Scanf.bscanf ic "\n" ();
    let parts = read_parts ic in
    { workflows; parts }

  let of_in_channel (ic : in_channel) : t =
    ic |> Scanf.Scanning.from_channel |> of_scanning_ic

  let of_string (s : string) : t =
    s |> Scanf.Scanning.from_string |> of_scanning_ic

  let workflows i = i.workflows
  let parts i = i.parts

  let pp : t Fmt.t =
    let open Fmt.Dump in
    let pp_part = Part.pp pp_z in
    record
      [
        field "workflows" workflows @@ list Workflow.pp;
        field "parts" parts @@ seq pp_part;
      ]
end

module StringMap = Map.Make (String)

module Part1 = struct
  type context = { wfs : Workflow.t StringMap.t; part : Z.t Part.t }

  let get_workflow ctx name = StringMap.find name ctx.wfs

  let cmp_of_op = function
    | Operator.Lt -> Z.Compare.( < )
    | Operator.Gt -> Z.Compare.( > )
    | _ -> failwith "cmp_of_op"

  let eval_cond ctx ({ op; lhs = field; rhs } : Expr.cond) =
    (cmp_of_op op) (Part.get field ctx.part) rhs

  let rec eval_expr ctx = function
    | Expr.Return (RuleResult.Resolved st) -> st
    | Expr.Return (RuleResult.Pending next_name) ->
        next_name |> get_workflow ctx |> Workflow.rules |> eval_expr ctx
    | Expr.If (cond, then_, else_) ->
        if eval_cond ctx cond then eval_expr ctx then_ else eval_expr ctx else_

  let classify_part wfs part =
    let ctx = { wfs; part } in
    let init = get_workflow ctx "in" in
    eval_expr ctx (Workflow.rules init)

  let part_rating part =
    let open Z in
    Category.all |> List.fold_left (fun cur cat -> cur + Part.get cat part) zero

  let run (input : Input.t) : Z.t =
    let wfs =
      input
      |> Input.workflows
      |> List.to_seq
      |> Seq.map (fun wf -> (Workflow.name wf, wf))
      |> StringMap.of_seq
    in

    let part_is_valid part =
      match classify_part wfs part with
      | PartStatus.Accepted -> true
      | PartStatus.Rejected -> false
    in

    input
    |> Input.parts
    |> Seq.filter part_is_valid
    |> Seq.map part_rating
    |> Seq.fold_left Z.add Z.zero
end

module Range : sig
  type t

  val make : start:Z.t -> stop:Z.t -> t
  val is_empty : t -> bool
  val length : t -> Z.t
  val mem : Z.t -> t -> bool [@@warning "-32"]
  val clamp : Expr.cond -> t -> t
  val pp : t Fmt.t [@@warning "-32"]
end = struct
  type span = { start : Z.t; stop : Z.t }
  type t = Empty | Singleton of Z.t | Span of span

  let make ~start ~stop =
    let open Z.Compare in
    if start >= stop then
      Fmt.invalid_arg "make range [%a..%a]" pp_z start pp_z stop;
    Span { start; stop }

  let is_empty = function Empty -> true | _ -> false

  let length = function
    | Empty -> Z.zero
    | Singleton _ -> Z.one
    | Span { start; stop } -> Z.succ @@ Z.sub stop start

  let mem x = function
    | Empty -> false
    | Singleton y -> Z.equal x y
    | Span { start; stop } -> Z.Compare.(x >= start && x <= stop)

  let with_min_incl x = function
    | Empty -> Empty
    | Singleton y ->
        let open Z.Compare in
        if x <= y then Singleton x else Empty
    | Span { start; stop } ->
        let open Z.Compare in
        if x <= start then Span { start; stop }
        else if x = stop then Singleton x
        else if x > stop then Empty
        else Span { start = x; stop }

  let with_min_excl x = with_min_incl @@ Z.succ x

  let with_max_incl x = function
    | Empty -> Empty
    | Singleton y ->
        let open Z.Compare in
        if x >= y then Singleton x else Empty
    | Span { start; stop } ->
        let open Z.Compare in
        if x >= stop then Span { start; stop }
        else if x = start then Singleton x
        else if x < start then Empty
        else Span { start; stop = x }

  let with_max_excl x = with_max_incl @@ Z.pred x

  let clamp : Expr.cond -> t -> t = function
    | { op = Operator.Lt; rhs; _ } -> with_max_excl rhs
    | { op = Operator.Le; rhs; _ } -> with_max_incl rhs
    | { op = Operator.Gt; rhs; _ } -> with_min_excl rhs
    | { op = Operator.Ge; rhs; _ } -> with_min_incl rhs

  let pp ppf = function
    | Empty -> Fmt.string ppf "[]"
    | Singleton x -> Fmt.pf ppf "[%a]" pp_z x
    | Span { start; stop } -> Fmt.pf ppf "[%a..%a]" pp_z start pp_z stop
end

module Part2 = struct
  let inv_cond (cond : Expr.cond) = { cond with op = Operator.inv cond.op }
  let ( ~! ) = inv_cond

  type part_range = Range.t Part.t

  let part_range_is_invalid (part_range : part_range) =
    Category.all
    |> List.exists (fun field -> Range.is_empty @@ Part.get field part_range)

  let clamp_part_range (cond : Expr.cond) part_range =
    let clamped = Range.clamp cond (Part.get cond.lhs part_range) in
    Part.replace cond.lhs clamped part_range

  type context = { wfs : Workflow.t StringMap.t; part_range : part_range }

  let get_workflow ctx name = StringMap.find name ctx.wfs

  let rec eval_expr ctx expr =
    if part_range_is_invalid ctx.part_range then []
    else
      match expr with
      | Expr.Return (RuleResult.Resolved PartStatus.Accepted) ->
          [ ctx.part_range ]
      | Expr.Return (RuleResult.Resolved PartStatus.Rejected) -> []
      | Expr.Return (RuleResult.Pending next_name) ->
          next_name |> get_workflow ctx |> Workflow.rules |> eval_expr ctx
      | Expr.If (cond, then_, else_) ->
          List.append
            (eval_expr
               { ctx with part_range = clamp_part_range cond ctx.part_range }
               then_)
            (eval_expr
               { ctx with part_range = clamp_part_range ~!cond ctx.part_range }
               else_)

  let resolve_ranges wfs init_ranges =
    let ctx = { wfs; part_range = init_ranges } in
    let init_wf = get_workflow ctx "in" in
    eval_expr ctx (Workflow.rules init_wf)

  let default_range = Range.make ~start:Z.one ~stop:(Z.of_int 4000)

  let default_ranges =
    Part.make ~x:default_range ~m:default_range ~a:default_range
      ~s:default_range

  let part_range_count part_range =
    Category.all
    |> List.map (fun field -> Part.get field part_range)
    |> List.map Range.length
    |> List.fold_left Z.mul Z.one

  let run (input : Input.t) : Z.t =
    let wfs =
      input
      |> Input.workflows
      |> List.to_seq
      |> Seq.map (fun wf -> (Workflow.name wf, wf))
      |> StringMap.of_seq
    in

    let valid_ranges = resolve_ranges wfs default_ranges in

    valid_ranges |> List.map part_range_count |> List.fold_left Z.add Z.zero
end
