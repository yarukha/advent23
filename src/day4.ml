[@@@warning "-32"]

let test =
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
   Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
   Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
   Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
   Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
   Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"


let read_file file = In_channel.with_open_bin file In_channel.input_all
let input = read_file "./input/day4.txt"

type card = int * int list * int list
type data = card list

let parse s : data =
  let open Angstrom in
  let m =
    let number =
      take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
    in
    let space = take_while1 (function ' ' -> true | _ -> false) in
    let num_list = sep_by1 space number in
    let line =
      let* n = string "Card" *> space *> number <* char ':' <* space in
      let* l1 = num_list <* space <* char '|' <* space in
      let* l2 = num_list in
      return (n, l1, l2)
    in
    sep_by1 end_of_line line <* end_of_line
  in
  match parse_string ~consume:All m s with
  | Ok x -> x
  | Error msg -> failwith msg

let card_score (_, l1, l2) =
  List.fold_left
    (fun s x -> if List.mem x l1 then match s with 0 -> 1 | n -> 2 * n else s)
    0 l2

let q1 = List.fold_left (fun s x -> s + card_score x) 0

let card_winning_number (_, l1, l2) =
  let x = List.fold_left (fun s x -> if List.mem x l1 then s + 1 else s) 0 l2 in
  (* Format.printf "%i->%i\n" n x; *)
  x

let rec aux = function
  | [] -> []
  | (copies, score) :: q ->
      let q = propagate (copies, score) q in
      (copies, score) :: aux q

and propagate (copies, score) l =
  if score = 0 then l
  else
    match l with
    | (c, s) :: q -> (c + copies, s) :: propagate (copies, score - 1) q
    | [] -> assert false

let q2 l =
  (* let open Format in *)
  let copies_score = List.map (fun x -> (1, card_winning_number x)) l in
  let l = aux copies_score in
  (* printf "%a" *)
  (*   (pp_print_list *)
  (*      ~pp_sep:(fun p () -> fprintf p "\n") *)
  (*      (fun p (i, j) -> fprintf p "%i,%i" i j)) *)
  (* l; *)
  List.fold_left (fun s (i, _) -> s + i) 0 l

let () =
  let x = input in
  let l1 = parse x in
  let i1 = q1 l1 in
  let i2 = q2 l1 in
  Format.printf "q1 = %i\nq2 = %i\n" i1 i2
