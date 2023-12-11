[@@@warning "-32"]

let read_file file = In_channel.with_open_bin file In_channel.input_all
let input = read_file "./input/day3.txt"

let test =
  "467..114..\n\
   ...*......\n\
   ..35..633.\n\
   ......#...\n\
   617*......\n\
   .....+.58.\n\
   ..592.....\n\
   ......755.\n\
   ...$.*....\n\
   .664.598.."

type data = N of int * int | Symbol of char | Dot

module M = Map.Make (struct
  type t = int * int

  let compare = compare
end)

let parse s =
  let open Angstrom in
  let c =
    satisfy (function '0' .. '9' | '.' | ' ' | '\n' -> false | _ -> true)
    >>| fun x -> Symbol x
  in
  let dot = char '.' *> return Dot in
  let num =
    let* x = take_while1 (function '0' .. '9' -> true | _ -> false) in
    return @@ N (String.length x, int_of_string x)
  in
  let line = many1 (choice [ c; dot; num ]) in
  let m = sep_by1 end_of_line line <* end_of_line in
  let transform_line i m l =
    List.fold_left
      (fun (j, m) x ->
        match x with
        | N (l, _) -> (j + l, M.add (i, j) x m)
        | _ -> (j + 1, M.add (i, j) x m))
      (0, m) l
    |> snd
  in
  match parse_string ~consume:All m s with
  | Ok x ->
      List.fold_left
        (fun (i, m) l -> (i + 1, transform_line i m l))
        (0, M.empty) x
      |> snd
  | Error msg -> failwith msg

let is_adjacent_to_symbol (i, j) (l, _) m =
  let l1 = List.init (l + 2) (fun k -> (i - 1, k + j - 1)) in
  let l2 = List.init (l + 2) (fun k -> (i + 1, k + j - 1)) in
  let neighbours_indexes = ((i, j - 1) :: (i, j + l) :: l1) @ l2 in
  List.exists
    (fun (i, j) ->
      match M.find_opt (i, j) m with Some (Symbol _) -> true | _ -> false)
    neighbours_indexes

let q1 m =
  M.fold
    (fun (i, j) x s ->
      match x with
      | N (l, n) when is_adjacent_to_symbol (i, j) (l, n) m ->
          (* Format.printf "%i\n" n; *)
          s + n
      | _ -> s)
    m 0

let extend_gear_map (i, j) (l, n) m gear_map =
  let l1 = List.init (l + 2) (fun k -> (i - 1, k + j - 1)) in
  let l2 = List.init (l + 2) (fun k -> (i + 1, k + j - 1)) in
  let neighbours_indexes = ((i, j - 1) :: (i, j + l) :: l1) @ l2 in
  let adj_gears =
    List.filter_map
      (fun (i, j) ->
        match M.find_opt (i, j) m with
        | Some (Symbol '*') -> Some (i, j)
        | _ -> None)
      neighbours_indexes
  in
  List.fold_left
    (fun gm (i', j') ->
      match M.find_opt (i', j') gear_map with
      | Some t -> M.add (i', j') (n :: t) gm
      | None -> M.add (i', j') [ n ] gm)
    gear_map adj_gears

let q2 m =
  let gm =
    M.fold
      (fun (i, j) x gm ->
        match x with N (l, n) -> extend_gear_map (i, j) (l, n) m gm | _ -> gm)
      m M.empty
  in
  M.fold (fun _ l s -> match l with [ a; b ] -> s + (a * b) | _ -> s) gm 0

let () =
  let x = input in
  let l1 = parse x in
  let i1 = q1 l1 in
  let i2 = q2 l1 in
  Format.printf "q1 = %i\nq2 = %i" i1 i2
