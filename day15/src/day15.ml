module Input = struct
  type t = string Seq.t

  let comma = Str.regexp ","

  let of_string (s : string) : t =
    s |> Str.split comma |> List.to_seq |> Seq.map String.trim

  let of_in_channel (ic : in_channel) : t =
    ic |> In_channel.input_all |> of_string
end

let hash_string (s : string) : int =
  s
  |> String.to_seq
  |> Seq.map Char.code
  |> Seq.fold_left (fun cur code -> (cur + code) * 17 mod 256) 0

module Part1 = struct
  let run : Input.t -> int =
   fun seq -> seq |> Seq.map hash_string |> Seq.fold_left ( + ) 0
end

module Operation = struct
  type t = Add of (string * int) | Remove of string

  let of_string (s : string) : t =
    if String.ends_with ~suffix:"-" s then
      let label = String.init (Int.pred (String.length s)) (fun i -> s.[i]) in
      Remove label
    else
      let label = String.init (String.length s - 2) (fun i -> s.[i])
      and focal_len = Char.code s.[String.length s - 1] - Char.code '0' in
      Add (label, focal_len)
end

module StringMap = Map.Make (String)

module Box = struct
  type t = (int * int) StringMap.t

  let empty : t = StringMap.empty
  let is_empty : t -> bool = StringMap.is_empty
  let remove (label : string) (box : t) : t = StringMap.remove label box

  let add (label : string) (focal_len : int) (operation_num : int) (box : t) : t
      =
    box
    |> StringMap.update label (function
         | None -> Some (focal_len, operation_num)
         | Some (_, num) -> Some (focal_len, num))

  let items (box : t) : int Seq.t =
    box
    |> StringMap.to_seq
    |> Seq.map snd
    |> List.of_seq
    |> List.sort (fun a b -> Int.compare (snd a) (snd b))
    |> List.to_seq
    |> Seq.map fst

  let slots (box : t) : (int * int) Seq.t =
    box |> items |> Seq.mapi (fun slot len -> (Int.succ slot, len))
end

module IntMap = Map.Make (Int)

module Boxes = struct
  type t = Box.t IntMap.t

  let empty : t = IntMap.empty

  let remove (label : string) (boxes : t) : t =
    let k = hash_string label in
    boxes
    |> IntMap.update k (function
         | None -> None
         | Some box ->
             let box' = Box.remove label box in
             if Box.is_empty box' then None else Some box')

  let add (label : string) (focal_len : int) (operation_num : int) (boxes : t) :
      t =
    let k = hash_string label in
    boxes
    |> IntMap.update k (function
         | None -> Some (Box.add label focal_len operation_num Box.empty)
         | Some box -> Some (Box.add label focal_len operation_num box))

  let handle (operation_num : int) : Operation.t -> t -> t = function
    | Operation.Add (label, focal_len) -> add label focal_len operation_num
    | Operation.Remove label -> remove label

  let by_num : t -> (int * Box.t) Seq.t = IntMap.to_seq
end

module Part2 = struct
  let power_of_box (box_num : int) (box : Box.t) : int =
    box
    |> Box.slots
    |> Seq.map (fun (slot, focal_len) -> slot * focal_len * Int.succ box_num)
    |> Seq.fold_left ( + ) 0

  let run : Input.t -> int =
   fun seq ->
    let boxes =
      seq
      |> Seq.map Operation.of_string
      |> Seq.fold_lefti
           (fun boxes operation_num op -> Boxes.handle operation_num op boxes)
           Boxes.empty
    in

    boxes
    |> Boxes.by_num
    |> Seq.map (fun (box_num, box) -> power_of_box box_num box)
    |> Seq.fold_left ( + ) 0
end
