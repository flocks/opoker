open List
open Opoker.Hand

let parse_hands = map (parse_hand 5)

let () =
  let args = Array.to_list Sys.argv in
  if length args <= 1 then
    begin
      Printf.fprintf stderr "Usage: ./opoker AhKdKc8s8j AcTcJcQcKc ....\n";
      exit 1
    end
  else
    let hands = parse_hands (List.tl args) in
    let print_hands_values hands =
      map (fun h ->
                 let hv = show_hand_value (get_hand_value h) in
                 let hand = String.concat "" (hand_as_string h) in
                 Printf.sprintf "%s: %s" hand hv)
        hands
    in
    let hv = print_hands_values hands in
    iter (print_endline) hv

