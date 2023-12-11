module Rank = struct
  type t =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard

  let to_int = function
    | FiveOfAKind -> 6
    | FourOfAKind -> 5
    | FullHouse -> 4
    | ThreeOfAKind -> 3
    | TwoPair -> 2
    | OnePair -> 1
    | HighCard -> 0

  let compare a b = Int.compare (to_int a) (to_int b)
end

module Card = struct
  type t =
    | A
    | K
    | Q
    | J
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two

  let to_int = function
    | A -> 14
    | K -> 13
    | Q -> 12
    | J -> 11
    | Ten -> 10
    | Nine -> 9
    | Eight -> 8
    | Seven -> 7
    | Six -> 6
    | Five -> 5
    | Four -> 4
    | Three -> 3
    | Two -> 2

  let compare a b = Int.compare (to_int a) (to_int b)

  let of_char = function
    | 'A' -> A
    | 'K' -> K
    | 'Q' -> Q
    | 'J' -> J
    | 'T' -> Ten
    | '9' -> Nine
    | '8' -> Eight
    | '7' -> Seven
    | '6' -> Six
    | '5' -> Five
    | '4' -> Four
    | '3' -> Three
    | '2' -> Two
    | other -> failwith @@ Printf.sprintf "unknown card: '%c'" other
end

module Hand = struct
  type t = { cards : Card.t list; wager : int }

  let make ~cards ~wager = { cards; wager }
  let cards hand = hand.cards
  let wager hand = hand.wager
end

module Input = struct
  type line = Hand.t
  type t = line Seq.t

  let parse_cards s = s |> String.to_seq |> Seq.map Card.of_char

  let parse_line s =
    let cards, wager =
      match String.split_on_char ' ' s with
      | [ a; b ] -> (List.of_seq @@ parse_cards a, int_of_string b)
      | other ->
          failwith
          @@ Printf.sprintf "line should have 2 parts; got %d"
               (List.length other)
    in
    Hand.make ~cards ~wager

  let of_in_channel chan =
    (fun () ->
      try Some (parse_line @@ input_line chan) with End_of_file -> None)
    |> Seq.of_dispenser
    |> Seq.memoize

  let of_string s =
    s |> String.split_on_char '\n' |> List.to_seq |> Seq.map parse_line
end

module CardMap = Map.Make (Card)

module Totals = struct
  type t = int CardMap.t

  let empty : t = CardMap.empty

  let add card : t -> t =
    CardMap.update card (function
      | None -> Some 1
      | Some c -> Some (Int.succ c))

  let of_list : Card.t list -> t =
    List.fold_left (fun m card -> add card m) empty

  let to_seq : t -> (Card.t * int) Seq.t = CardMap.to_seq

  let get (card : Card.t) (totals : t) : int =
    totals |> CardMap.find_opt card |> Option.value ~default:0
end

module Part1 = struct
  let rank_of_cards (cards : Card.t list) : Rank.t =
    let counts =
      cards
      |> Totals.of_list
      |> Totals.to_seq
      |> List.of_seq
      |> List.sort (fun (card1, count1) (card2, count2) ->
             match Int.compare count1 count2 with
             | 0 -> Card.compare card1 card2
             | other -> Int.neg other)
    in

    match counts with
    | [ (_, 5) ] -> Rank.FiveOfAKind
    | (_, 4) :: _ -> Rank.FourOfAKind
    | [ (_, 3); (_, 2) ] -> Rank.FullHouse
    | (_, 3) :: _ -> Rank.ThreeOfAKind
    | (_, 2) :: (_, 2) :: _ -> Rank.TwoPair
    | (_, 2) :: _ -> Rank.OnePair
    | _ -> HighCard

  type ranked_hand = Rank.t * Hand.t

  let rank_hand (hand : Hand.t) : ranked_hand =
    (rank_of_cards @@ Hand.cards hand, hand)

  let cmp_ranked_hands : ranked_hand -> ranked_hand -> int =
   fun a b ->
    match Rank.compare (fst a) (fst b) with
    | 0 -> List.compare Card.compare (Hand.cards @@ snd a) (Hand.cards @@ snd b)
    | other -> other

  let sort_hands : Hand.t Seq.t -> Hand.t Seq.t =
   fun hands ->
    hands
    |> Seq.map rank_hand
    |> List.of_seq
    |> List.sort cmp_ranked_hands
    |> List.to_seq
    |> Seq.map snd

  let run : Input.t -> int =
   fun hands ->
    hands
    |> sort_hands
    |> Seq.mapi (fun i hand ->
           let rank = Int.succ i and wager = Hand.wager hand in

           Int.mul rank wager)
    |> Seq.fold_left ( + ) 0
end

module Part2 = struct
  let rank_of_cards (cards : Card.t list) : Rank.t =
    let totals = cards |> Totals.of_list in
    let non_joker_counts =
      totals
      |> Totals.to_seq
      |> Seq.filter (function Card.J, _ -> false | _ -> true)
      |> List.of_seq
      |> List.sort (fun (card1, count1) (card2, count2) ->
             match Int.compare count1 count2 with
             | 0 -> Card.compare card1 card2
             | other -> Int.neg other)
    and num_jokers = Totals.get Card.J totals in

    match (non_joker_counts, num_jokers) with
    | [ (_, 5) ], _ -> Rank.FiveOfAKind
    | (_, 4) :: _, 0 -> Rank.FourOfAKind
    | (_, 4) :: _, 1 -> Rank.FiveOfAKind
    | [ (_, 3); (_, 2) ], _ -> Rank.FullHouse
    | (_, 3) :: _, 0 -> Rank.ThreeOfAKind
    | (_, 3) :: _, 1 -> Rank.FourOfAKind
    | (_, 3) :: _, 2 -> Rank.FiveOfAKind
    | (_, 2) :: (_, 2) :: _, 0 -> Rank.TwoPair
    | (_, 2) :: (_, 2) :: _, 1 -> Rank.FullHouse
    | (_, 2) :: _, 0 -> Rank.OnePair
    | (_, 2) :: _, 1 -> Rank.ThreeOfAKind
    | (_, 2) :: _, 2 -> Rank.FourOfAKind
    | (_, 2) :: _, 3 -> Rank.FiveOfAKind
    | _, 1 -> OnePair
    | _, 2 -> ThreeOfAKind
    | _, 3 -> FourOfAKind
    | _, 4 -> FiveOfAKind
    | _, 5 -> FiveOfAKind
    | _ -> HighCard

  let card_as_int = function Card.J -> 1 | other -> Card.to_int other
  let cmp_cards a b = Int.compare (card_as_int a) (card_as_int b)

  type ranked_hand = Rank.t * Hand.t

  let rank_hand (hand : Hand.t) : ranked_hand =
    (rank_of_cards @@ Hand.cards hand, hand)

  let cmp_ranked_hands : ranked_hand -> ranked_hand -> int =
   fun a b ->
    match Rank.compare (fst a) (fst b) with
    | 0 -> List.compare cmp_cards (Hand.cards @@ snd a) (Hand.cards @@ snd b)
    | other -> other

  let sort_hands : Hand.t Seq.t -> Hand.t Seq.t =
   fun hands ->
    hands
    |> Seq.map rank_hand
    |> List.of_seq
    |> List.sort cmp_ranked_hands
    |> List.to_seq
    |> Seq.map snd

  let run : Input.t -> int =
   fun hands ->
    hands
    |> sort_hands
    |> Seq.mapi (fun i hand ->
           let rank = Int.succ i and wager = Hand.wager hand in

           Int.mul rank wager)
    |> Seq.fold_left ( + ) 0
end
