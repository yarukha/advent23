[@@@warning "-32"]

let test = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"
let read_file file = In_channel.with_open_bin file In_channel.input_all
let input = read_file "./input/day9.txt"

let parse s =
  let open Angstrom in
  let number =
    take_while1 (function '0' .. '9' | '-' -> true | _ -> false)
    >>| int_of_string
  in
  let space = take_while1 (function ' ' -> true | _ -> false) in
  let line = sep_by1 space number in
  let m = sep_by1 end_of_line line <* option () end_of_line in
  match parse_string ~consume:All m s with
  | Ok x -> x
  | Error msg -> failwith msg

let pp_list =
  Format.pp_print_list
    ~pp_sep:(fun p () -> Format.fprintf p ",")
    Format.pp_print_int

let go_down l =
  let rec step = function x :: y :: q -> (y - x) :: step (y :: q) | _ -> [] in
  let rec aux acc l =
    if List.is_empty l || List.for_all (fun x -> x = 0) l then acc
    else
      let s = step l in
      (* Format.printf "%a down %a\n" pp_list l pp_list s; *)
      aux (s :: acc) s
  in
  aux [ l ] l

let go_up big_list =
  (* Format.printf "size=%i\n" (List.length big_list); *)
  let step x0 diff =
    List.fold_left (fun l x -> (x + List.hd l) :: l) [ x0 ] diff |> List.rev
  in
  List.fold_left
    (fun l diff ->
      let nl = step (List.hd diff) l in
      (* Format.printf "%a diff %a = %a\n" pp_list diff pp_list l pp_list nl; *)
      nl)
    (0 :: List.hd big_list) (List.tl big_list)
  |> List.rev |> List.hd

let q1 = List.fold_left (fun s l -> s + (go_down l |> go_up)) 0

let go_up_left big_list =
  (* Format.printf "size=%i\n" (List.length big_list); *)
  let step x0 diff =
    List.fold_right
      (fun x l ->
        (* Format.printf "%i-%i=%i\n" (List.hd l) x (List.hd l - x); *)
        (List.hd l - x) :: l)
      diff [ x0 ]
  in
  List.fold_left
    (fun diff l ->
      let nl = step (List.hd @@ List.rev l) diff in
      (* Format.printf "%a diff %a = %a\n" pp_list l pp_list diff pp_list nl; *)
      nl)
    (0 :: List.hd big_list) (List.tl big_list)
  |>  List.hd

let q2 = List.fold_left (fun s l -> s + (go_down l |> go_up_left)) 0

let () =
  let x = input in
  let l = parse x in
  let i1 = q1 l in
  let i2 = q2 l in
  Format.printf "q1 = %i\nq2 = %i" i1 i2
