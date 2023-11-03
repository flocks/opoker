open Alcotest
open Opoker

let format_card fmt v = Format.fprintf fmt "%s" (Hand.show_card v)               
let format_hand fmt v =
  Format.fprintf fmt "%s" (String.concat "" (List.map (fun c -> Hand.show_card c) v))
let format_hand_value fmt v = Format.fprintf fmt "%s" (Hand.show_hand_value v)               

let parsing_tests =
  [
    "parse card", `Quick, (fun () ->
      let open Hand in
      let expected : card = { rank = Ace; suit = Heart } in
      let result = (parse_card "Ah") in
      check (of_pp format_card) "Ah" expected result);
    "parse hand", `Quick, (fun () ->
      let open Hand in
      let expected = [{ rank = Ace; suit = Heart };
                      { rank = Ace; suit = Diamond };
                      { rank = Ace; suit = Club };
                      { rank = Ace; suit = Spade };
                      { rank = King; suit = Heart };
                     ] in
      let result = parse_hand 5 "AsAcAdAhKh" in
      check (of_pp format_hand) "AsAcKhAdAh" expected result);
  ]

let hand_value_tests =
  let open Hand in
  [
    "is hand a straight", `Quick, (fun () ->
      let expected = true in
      let result = is_straight (parse_hand 5 "ThJhQhKhAh") in
      check bool "isStraight" expected result);

    "is hand a straight from Ace", `Quick, (fun () ->
      let expected = true in
      let result = is_straight (parse_hand 5 "Ac2c3c4c5c") in
      check bool "isStraight" expected result);

    "is a flush", `Quick, (fun () ->
      let expected = true in
      let result = is_flush (parse_hand 5 "Ac2c3c4c5c") in
      check bool "isFlush" expected result);

    "is a Pair", `Quick, (fun () ->
      let expected = Pair (Ace,Ten,Nine,Deuce) in
      let result = get_hand_value (parse_hand 5 "AcAsTd9h2d") in
      check (of_pp format_hand_value) "pair" expected result);

    "is a High Card", `Quick, (fun () ->
      let expected = High (Ace,Ten,Nine,Eight, Deuce) in
      let result = get_hand_value (parse_hand 5 "AcTs2d9h8d") in
      check (of_pp format_hand_value) "high" expected result);

    "is a Double Pair", `Quick, (fun () ->
      let expected = TwoPairs (Ace,Ten,Deuce) in
      let result = get_hand_value (parse_hand 5 "AcAsTdTh2d") in
      check (of_pp format_hand_value) "TwoPair" expected result);

    "is a Square", `Quick, (fun () ->
      let expected = Square (Ace, Four) in
      let result = get_hand_value (parse_hand 5 "AcAsAdAh4d") in 
      check (of_pp format_hand_value) "is a Square " expected result);

    "is a Full House", `Quick, (fun () ->
      let expected = FullHouse (Ace, Four) in
      let result = get_hand_value (parse_hand 5 "AcAsAd4c4d") in
      check (of_pp format_hand_value) "is a full house" expected result);

    "is a Straight to J", `Quick, (fun () ->
      let expected = Straight Jack in
      let result = get_hand_value (parse_hand 5 "Th8c9sJd7d") in
      check (of_pp format_hand_value) "is a straight to J" expected result);

    "is a Straight to 5", `Quick, (fun () ->
      let expected = Straight Five in
      let result = get_hand_value (parse_hand 5 "Ac2c3h4c5c") in
      check (of_pp format_hand_value) "is a straight to 5" expected result);

    "is a Straight Flush", `Quick, (fun () ->
      let expected = StraightFlush Five in
      let result = get_hand_value (parse_hand 5 "Ac2c3c4c5c") in
      check (of_pp format_hand_value) "is straight flush" expected result);
  ]

let () =
  run "My Module Tests"
    ["parsing", parsing_tests;
     "hand values", hand_value_tests
    ];
