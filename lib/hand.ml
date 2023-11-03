type rank =
  | Deuce
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
[@@deriving show, eq, ord]

let show_rank r =
  match r with
  | Ace -> "A"
  | King -> "K"
  | Queen -> "Q"
  | Jack -> "J"
  | Ten -> "T"
  | Nine -> "9"
  | Eight -> "8"
  | Seven -> "7"
  | Six -> "6"
  | Five -> "5"
  | Four -> "4"
  | Three -> "3"
  | Deuce -> "2"

type suit =
  | Heart
  | Spade
  | Club
  | Diamond
[@@deriving show, eq]

let show_suit s =
  match s with
  | Heart -> "h"
  | Spade -> "s"
  | Club -> "c"
  | Diamond -> "d"


type card = {suit: suit; rank: rank}
[@@deriving show, eq]

type hand = card list
[@@deriving show]

type hand_value =
  | High of rank * rank * rank * rank * rank
  | Pair of rank * rank * rank * rank
  | TwoPairs of rank * rank * rank
  | Triple of rank * rank * rank
  | Straight of rank
  | Flush of rank
  | FullHouse of rank * rank
  | Square of rank * rank
  | StraightFlush of rank 
[@@deriving show, eq]

let get_next_rank r =
  match r with
  | Ace -> Deuce
  | King -> Ace
  | Queen -> King
  | Jack -> Queen
  | Ten -> Jack
  | Nine -> Ten
  | Eight -> Nine
  | Seven -> Eight
  | Six -> Seven
  | Five -> Six
  | Four -> Five
  | Three -> Four
  | Deuce -> Three

let card_as_string c = (show_rank c.rank) ^ (show_suit c.suit)

let hand_as_string h =
  List.map card_as_string h

let sort_hand h = 
  let open List in
  let cmp c1 c2 = if c1.rank < c2.rank then 1 else -1 in
  stable_sort cmp h

let is_flush h =
  let head = List.hd h in
  let tail = List.tl h in
  let first_suit = head.suit in
  Utils.all tail (fun x -> x.suit == first_suit)
  
let is_straight h =
  let rec f l acc =
    match l with
    | [] -> acc
    | [_] -> acc
    | x :: rest ->
        let rank1 = x.rank in
        let rank2 = (List.hd rest).rank in
        if get_next_rank rank2 == rank1 ||  rank2 == Five && rank1 == Ace then
          f rest true
        else
          false
  in
  f h true

let get_staight_kicker kicker =  
  if kicker == Ace then Five else kicker

let get_hand_value h =
  let flush_p = is_flush h in
  let straight_p = is_straight h in
  let kicker = (List.hd h).rank in
  if (flush_p && straight_p) then
    StraightFlush (get_staight_kicker kicker)
  else if straight_p then
    Straight (get_staight_kicker kicker)
  else if flush_p then
    Flush kicker
  else
    let cmp c1 c2 = c1.rank == c2.rank in
    let open Utils in
    match group_by h cmp with
    | [[c1;_;_;_];[k]] -> Square (c1.rank, k.rank)
    | [[c1;_;_];[k; _]] -> FullHouse (c1.rank, k.rank)
    | [[c1;_;_];[k1];[k2]] -> Triple (c1.rank, k1.rank, k2.rank)
    | [[c1;_];[k; _];[k2]] -> TwoPairs (c1.rank, k.rank, k2.rank)
    | [[c1;_];[k1];[k2];[k3]] -> Pair (c1.rank, k1.rank, k2.rank, k3.rank)
    | [[k1];[k2];[k3];[k4];[k5]] ->
        High (k1.rank, k2.rank, k3.rank, k4.rank, k5.rank)
    |_ -> failwith "Invalid grouped by"

let parse_rank c =
  match c with
  | 'A' -> Ace
  | 'K' -> King
  | 'Q' -> Queen
  | 'J' -> Jack
  | 'T' -> Ten
  | '9' -> Nine
  | '8' -> Eight
  | '7' -> Seven
  | '6' -> Six
  | '5' -> Five
  | '4' -> Four
  | '3' -> Three
  | '2' -> Deuce
  (** TODO may be handle error *)
  | _ -> failwith "Couldn't properly parse rank"

let parse_suit c =
  match c with
  | 's' -> Spade
  | 'h' -> Heart
  | 'd' -> Diamond
  | 'c' -> Club
  (** TODO may be handle error *)
  | _ -> failwith "Couldn't properly parse suit"

let parse_card str =
  let char_seq = String.to_seq str in
  let char_list = List.of_seq char_seq in
  match char_list with
    | [r; s] -> { rank = parse_rank r; suit = parse_suit s}
    (** TODO may be handle error *)
    | _ -> failwith "Couldn't properly parse card"


let parse_hand nb str =
    let rec get_hands acc string nb =
      if String.length string != 2 * nb then
        failwith "invalid string length"
      else
        let sub = String.sub string 0 2 in
        if String.length string == 2 then
          (parse_card sub) :: acc
        else
          let rest = String.sub string 2 (String.length string - 2) in
          get_hands ((parse_card sub) :: acc) rest (nb -1)
    in
    let res = get_hands [] str nb in
    sort_hand res

