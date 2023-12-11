[@@@warning "-32"]

let read_file file = In_channel.with_open_bin file In_channel.input_all
let input = read_file "./input/day1.txt"

let test = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

let test2 =
  "two1nine\n\
   eightwothree\n\
   abcone2threexyz\n\
   xtwone3four\n\
   4nineeightseven2\n\
   zoneight234\n\
   7pqrstsixteen"

let list_of_string s = String.to_seq s |> List.of_seq

type z = I of int | C of char
type data = z list list

let parse s =
  let open Angstrom in
  let m =
    let alpha =
      let* x = satisfy (function 'a' .. 'z' -> true | _ -> false) in
      return @@ C x
    in
    let int =
      let* x = satisfy (function '0' .. '9' -> true | _ -> false) in
      return @@ I (int_of_string @@ String.make 1 x)
    in
    let z = alpha <|> int in
    let line = many z in
    sep_by1 end_of_line line
  in
  match parse_string ~consume:All m s with
  | Ok x -> x
  | Error msg -> failwith msg

let get_only_numbers : data -> int list list =
  List.map (List.filter_map (function I i -> Some i | _ -> None))

let add_fst_last l =
  if l = [] then 0
  else
    let fst = List.hd l in
    let lst = try List.(hd @@ rev @@ tl l) with _ -> List.hd l in
    (* Format.printf "%i %i\n" fst lst; *)
    (10 * fst) + lst

let q1 l =
  get_only_numbers l |> List.fold_left (fun s l -> s + add_fst_last l) 0

let rec replace_good_old = function
  | 'o' :: 'n' :: 'e' :: q -> 1 :: replace_good_old q
  | 't' :: 'w' :: 'o' :: q -> 2 :: replace_good_old q
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: q -> 3 :: replace_good_old q
  | 'f' :: 'o' :: 'u' :: 'r' :: q -> 4 :: replace_good_old q
  | 'f' :: 'i' :: 'v' :: 'e' :: q -> 5 :: replace_good_old q
  | 's' :: 'i' :: 'x' :: q -> 6 :: replace_good_old q
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: q -> 7 :: replace_good_old q
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: q -> 8 :: replace_good_old q
  | 'n' :: 'i' :: 'n' :: 'e' :: q -> 9 :: replace_good_old q
  | '1' :: q -> 1 :: replace_good_old q
  | '2' :: q -> 2 :: replace_good_old q
  | '3' :: q -> 3 :: replace_good_old q
  | '4' :: q -> 4 :: replace_good_old q
  | '5' :: q -> 5 :: replace_good_old q
  | '6' :: q -> 6 :: replace_good_old q
  | '7' :: q -> 7 :: replace_good_old q
  | '8' :: q -> 8 :: replace_good_old q
  | '9' :: q -> 9 :: replace_good_old q
  | _ :: q -> replace_good_old q
  | [] -> []

let q2 s =
  let open Angstrom in
  let m =
    let line =
      let* x = take_while (function '\n' -> false | _ -> true) in
      let x = list_of_string x in
      return @@ replace_good_old x
    in
    sep_by1 end_of_line line
  in
  match parse_string ~consume:All m s with
  | Ok x ->
      let open Format in
    printf "length=%i@[<v 0>@,%a@,@,@]" (List.length x)
        (pp_print_list
           ~pp_sep:(fun p () -> fprintf p "@,")
           (pp_print_list ~pp_sep:(fun p () -> fprintf p ",") pp_print_int))
        x;
      List.fold_left (fun s l -> s + add_fst_last l) 0 x
  | Error msg -> failwith msg

let () =
  let x = input in
  let l1 = parse x in
  let i1 = q1 l1 in
  let i2 = q2 x in
  Format.printf "q1 = %i@,q2 = %i" i1 i2
