[@@@warning "-32"]

let input =
  let f = open_in_bin "./input/day2.txt" in
  really_input_string f (in_channel_length f)

let test =
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
   Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
   Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
   Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
   Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

let input =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin "./input/day2.txt" in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s


type col = R | G | B

let parse s =
  let open Angstrom in
  let m =
    let number =
      take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
    in
    let red = (fun _ -> R) <$> string "red" in
    let blue = (fun _ -> B) <$> string "blue" in
    let green = (fun _ -> G) <$> string "green" in
    let col = choice [ red; blue; green ] in
    let data =
      let* n = number in
      let* col = char ' ' *> col in
      return (n, col)
    in
    let set = sep_by1 (string ", ") data in
    let line =
      let* n = string "Game " *> number <* string ": " in
      let* g = sep_by1 (string "; ") set in
      return (n, g)
    in
    (sep_by1 end_of_line line) <* end_of_line
  in
  match parse_string ~consume:All m s with
  | Ok x -> x
  | Error msg -> failwith msg

let possible_set set =
  let r, g, b =
    List.fold_left
      (fun (r, g, b) (x, c) ->
        match c with
        | R -> (r + x, g, b)
        | G -> (r, g + x, b)
        | B -> (r, g, b + x))
      (0, 0, 0) set
  in
  r <= 12 && g <= 13 && b <= 14

let possible_game data = List.for_all possible_set data
let q1 = List.fold_left (fun s (i, g) -> s + if possible_game g then i else 0) 0

let max_colors_in_game g =
  List.flatten g
  |> List.fold_left
       (fun (r, g, b) (x, c) ->
         match c with
         | R -> (max x r, g, b)
         | G -> (r, max x g, b)
         | B -> (r, g, max x b))
       (0, 0, 0)

let q2 =
  List.fold_left
    (fun acc (_,g) ->
      let r, g, b = max_colors_in_game g in
      acc + (r * g * b))
    0

let () =
  let x = input in
  let l1 = parse x in
  let i1 = q1 l1 in
  let i2 = q2 l1 in 
  Format.printf "q1= %i@,q2=%i" i1 i2
