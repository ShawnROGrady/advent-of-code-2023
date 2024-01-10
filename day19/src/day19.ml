module Category = struct
  type t = X | M | A | S

  let of_string = function
    | "x" -> X
    | "m" -> M
    | "a" -> A
    | "s" -> S
    | other ->
        invalid_arg @@ Fmt.str "invalid category: %a" Fmt.Dump.string other

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
  type t = { x : int; m : int; a : int; s : int }

  let make ~x ~m ~a ~s = { x; m; a; s }
  let x part = part.x
  let m part = part.m
  let a part = part.a
  let s part = part.s

  let get : Category.t -> t -> int = function
    | Category.X -> x
    | Category.M -> m
    | Category.A -> a
    | Category.S -> s

  let to_string part =
    let body =
      Category.all
      |> List.map (fun cat ->
             Printf.sprintf "%s=%d" (Category.to_string cat) (get cat part))
      |> String.concat ","
    in
    "{" ^ body ^ "}"

  let pp : t Fmt.t =
    let pp_field cat =
      Fmt.Dump.field (Category.to_string cat) (get cat) Fmt.int
    in

    Category.all |> List.map pp_field |> Fmt.Dump.record
end

module Cmp = struct
  type t = Lt | Gt

  let of_string = function
    | "<" -> Lt
    | ">" -> Gt
    | other -> invalid_arg @@ Fmt.str "invalid cmp: %a" Fmt.Dump.string other

  let to_string = function Lt -> "<" | Gt -> ">"
  let ints = function Lt -> ( < ) | Gt -> ( > )

  let pp : t Fmt.t =
    Fmt.using (function Lt -> "Cmp.Lt" | Gt -> "Cmp.Gt") Fmt.string
end

module Cond = struct
  type t = { lhs : Category.t; op : Cmp.t; rhs : int }

  let make ~lhs ~op ~rhs = { lhs; op; rhs }
  let lhs cond = cond.lhs
  let op cond = cond.op
  let rhs cond = cond.rhs

  let to_fun (cond : t) : Part.t -> bool =
   fun part -> (Cmp.ints @@ op cond) (Part.get (lhs cond) part) (rhs cond)

  let matches part cond = (to_fun cond) part

  let to_string cond =
    Printf.sprintf "%s%s%d"
      (Category.to_string @@ lhs cond)
      (Cmp.to_string @@ op cond)
      (rhs cond)

  let pp : t Fmt.t =
    let open Fmt in
    let open Dump in
    record
      [ field "lhs" lhs Category.pp; field "op" op Cmp.pp; field "rhs" rhs int ]
end

module Predicate = struct
  type t = Always | If of Cond.t

  let matches (part : Part.t) = function
    | Always -> true
    | If cond -> Cond.matches part cond

  let to_string = function
    | Always -> ""
    | If cond -> Printf.sprintf "%s:" (Cond.to_string cond)

  let pp : t Fmt.t =
   fun ppf -> function
    | Always -> Fmt.string ppf "Always"
    | If cond -> Fmt.pf ppf "(If %a)" Cond.pp cond
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

  let to_string = function
    | Resolved PartStatus.Accepted -> "A"
    | Resolved PartStatus.Rejected -> "R"
    | Pending next -> next

  let pp : t Fmt.t =
   fun ppf -> function
    | Resolved st -> Fmt.pf ppf "(Resolved %a)" PartStatus.pp st
    | Pending next -> Fmt.pf ppf "(Pending %a)" Fmt.Dump.string next
end

module Rule = struct
  type t = { predicate : Predicate.t; result : RuleResult.t }

  let make ~predicate ~result = { predicate; result }
  let predicate r = r.predicate
  let result r = r.result

  let to_string rule =
    (Predicate.to_string @@ predicate rule)
    ^ RuleResult.to_string
    @@ result rule

  let pp : t Fmt.t =
    let get_result = result in
    let open Fmt.Dump in
    record
      [
        field "predicate" predicate Predicate.pp;
        field "result" get_result RuleResult.pp;
      ]
end

module Workflow = struct
  type t = { name : string; rules : Rule.t list }

  let make ~name ~rules = { name; rules }
  let name wf = wf.name
  let rules wf = wf.rules

  let to_string wf =
    name wf
    ^ "{"
    ^ (wf |> rules |> List.map Rule.to_string |> String.concat ",")
    ^ "}"

  let pp : t Fmt.t =
    let open Fmt.Dump in
    record [ field "name" name string; field "rules" rules @@ list Rule.pp ]
end

module Input = struct
  type t = { workflows : Workflow.t list; parts : Part.t Seq.t }
  type scan_ic = Scanf.Scanning.in_channel

  let rec parse_rules_aux xs () =
    match xs with
    | [] -> Seq.Nil
    | [ x ] ->
        let predicate = Predicate.Always and result = RuleResult.of_string x in
        Seq.Cons (Rule.make ~predicate ~result, Seq.empty)
    | x :: rest ->
        let rule =
          Scanf.sscanf x "%1[xmas]%1[<>]%d:%[a-zA-Z]"
            (fun lhs_s op_s rhs res_s ->
              let cond =
                Cond.make ~lhs:(Category.of_string lhs_s)
                  ~op:(Cmp.of_string op_s) ~rhs
              in
              let predicate = Predicate.If cond
              and result = RuleResult.of_string res_s in
              Rule.make ~predicate ~result)
        in
        Seq.Cons (rule, parse_rules_aux rest)

  let comma = Str.regexp ","
  let parse_rules s = s |> Str.split comma |> parse_rules_aux |> List.of_seq

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
    Scanf.bscanf ic "{x=%d,m=%d,a=%d,s=%d} " (fun x m a s ->
        Part.make ~x ~m ~a ~s)

  let read_parts (ic : scan_ic) : Part.t Seq.t =
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

  let to_string i =
    let wfs =
      i |> workflows |> List.map Workflow.to_string |> String.concat "\n"
    in

    let ps =
      i |> parts |> Seq.map Part.to_string |> List.of_seq |> String.concat "\n"
    in

    wfs ^ "\n" ^ ps

  let pp : t Fmt.t =
    let open Fmt.Dump in
    record
      [
        field "workflows" workflows @@ list Workflow.pp;
        field "parts" parts @@ seq Part.pp;
      ]
end

module Classifier = struct
  module StringMap = Map.Make (String)

  type t = Workflow.t StringMap.t

  let from_workflows (workflows : Workflow.t Seq.t) : t =
    workflows |> Seq.map (fun wf -> (Workflow.name wf, wf)) |> StringMap.of_seq

  let get_workflow name (wfs : t) : Workflow.t =
    match StringMap.find_opt name wfs with
    | None ->
        failwith @@ Fmt.str "no workflow with name=%a" Fmt.Dump.string name
    | Some wf -> wf

  let run_workflow wf part =
    let rec loop = function
      | [] ->
          failwith
          @@ Fmt.str "no matching rules in %a for %a" Fmt.Dump.string
               (Workflow.name wf) Part.pp part
      | rule :: rules ->
          if Predicate.matches part (Rule.predicate rule) then Rule.result rule
          else loop rules
    in

    loop (Workflow.rules wf)

  let rec eval_workflow wf part wfs =
    match run_workflow wf part with
    | RuleResult.Resolved st -> st
    | RuleResult.Pending next -> eval_workflow (get_workflow next wfs) part wfs

  let classify (part : Part.t) (wfs : t) : PartStatus.t =
    eval_workflow (get_workflow "in" wfs) part wfs
end

module Part1 = struct
  let part_rating part =
    let open Z in
    Category.all
    |> List.fold_left (fun cur cat -> cur + ~$(Part.get cat part)) zero

  let run : Input.t -> Z.t =
   fun input ->
    let classifier =
      input |> Input.workflows |> List.to_seq |> Classifier.from_workflows
    in
    let is_accepted part =
      match Classifier.classify part classifier with
      | PartStatus.Accepted -> true
      | PartStatus.Rejected -> false
    in

    let accepted_parts = input |> Input.parts |> Seq.filter is_accepted in

    accepted_parts |> Seq.map part_rating |> Seq.fold_left Z.add Z.zero
end

module Part2 = struct
  let run _ = failwith "unimplemented"
end
