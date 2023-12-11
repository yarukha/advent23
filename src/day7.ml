[@@@warning "-32"]

let test =
  "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"

let input =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin "./input/day7.txt" in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

type card = int
type hand = card list
type bid = int
type hand_bid = hand * bid
type data = hand_bid list

let pp_hand =
  let open Format in
  pp_print_list
    ~pp_sep:(fun p () -> fprintf p "")
    (fun p i ->
      if i = 1 then fprintf p "A"
      else if i < 10 then fprintf p "%i" i
      else
        fprintf p (match i with 10 -> "T" | 11 -> "J" | 12 -> "Q" | _ -> "K"))

let parse s : data =
  let open Angstrom in
  let card =
    satisfy (function
      | '2' .. '9' | 'A' | 'J' | 'T' | 'Q' | 'K' -> true
      | _ -> false)
    >>| function
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'T' -> 10
    | 'J' -> 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> assert false
  in
  let number =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  in
  let space = take_while1 (function ' ' | '\t' -> true | _ -> false) in
  let line =
    let* c = many1 card in
    let* bid = space *> number in
    return (c, bid)
  in
  let m = sep_by1 end_of_line line <* option () end_of_line in
  match parse_string ~consume:All m s with
  | Ok x -> x
  | Error msg -> failwith msg

let score_hand : hand -> int * int =
  let rec aux dupl = function
    | [] -> dupl
    | x :: q when List.exists (function y :: _ -> x = y | _ -> false) dupl ->
        aux (List.map (fun z -> if List.mem x z then x :: z else z) dupl) q
    | x :: q -> aux ([ x ] :: dupl) q
  in
  fun l ->
    let l = aux [] l in
    let n1 = List.fold_left (fun m x -> max m @@ List.length x) 0 l in
    let n2 = List.length l in
    (n1, 5 - n2)

let q1 l =
  List.map
    (fun (h, b) ->
      let s1, s2 = score_hand h in
      ((s1, s2), h, b))
    l
  |> List.sort compare
  |> List.mapi (fun i (_, _, bid) -> (i + 1) * bid)
  |> List.fold_left ( + ) 0

(* let score_cj : hand -> int * int = *)
(*   let rec aux dupl = function *)
(*     | [] -> dupl *)
(*     | x :: q when List.exists (function y :: _ -> x = y | _ -> false) dupl -> *)
(*         aux (List.map (fun z -> if List.mem x z then x :: z else z) dupl) q *)
(*     | x :: q -> aux ([ x ] :: dupl) q *)
(*   in *)
(*   fun l -> *)
(*     let l = aux [] l in *)
(*     let n1 = *)
(*       let x1, x2 = *)
(*         List.fold_left *)
(*           (fun (m, mj) x -> *)
(*             if List.hd x = 11 then (m, List.length x) *)
(*             else (max m @@ List.length x, mj)) *)
(*           (0, 0) l *)
(*       in *)
(*       x1 + x2 *)
(*     in *)
(*     let n2 = *)
(*       match List.exists (function 11 :: _ -> true | _ -> false) l with *)
(*       | true -> List.length l + 1 *)
(*       | false -> List.length l *)
(*     in *)
(*     (n1, 5 - n2) *)

let score_cj (h : hand) =
  List.init 13 (fun i ->
      List.map (function 11 -> i + 1 | k -> k) h |> score_hand)
  |> List.sort compare |> List.rev |> List.hd

let q2 l =
  List.map
    (fun (h, b) ->
      let s1, s2 = score_cj h in
      ((s1, s2), h, b))
    l
  |> List.sort compare
  |> List.mapi (fun i ((s1, s2), h, bid) ->
         Format.printf "%a, score %i,%i bid %i rank %i\n" pp_hand h s1 s2 bid
           (i + 1);
         (i + 1) * bid)
  |> List.fold_left ( + ) 0

let () =
  let x = test in
  let l = parse x in
  let i1 = q1 l in
  let i2 = q2 l in
  Format.printf "q1 = %i\nq2 = %i" i1 i2
