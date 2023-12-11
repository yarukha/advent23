[@@@warning "-32"]

let test = "Time:      7  15   30\nDistance:  9  40  200"

let input =
  "Time:        54     81     70     88\nDistance:   446   1292   1035   1007"

let () = ()

let parse s =
  let open Angstrom in
  let number =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  in
  let space = take_while1 (function ' ' | '\t' -> true | _ -> false) in
  let m =
    let* times =
      string "Time:" *> space *> sep_by1 space number <* end_of_line
    in
    let* distances = string "Distance:" *> space *> sep_by1 space number in
    return (times, distances)
  in
  match parse_string ~consume:All m s with
  | Ok x -> x
  | Error msg -> failwith msg

(*sketchy trick absolutely not working in general to accept entire solutions of (30,200)*)

let number_of_records (t, d) =
  let delta = (t * t) - (4 * d) |> float_of_int in
  let eps = 0.0000001 in
  let tf = t |> float_of_int in
  let x1 = (-.tf -. sqrt delta) /. -2. and x2 = (-.tf +. sqrt delta) /. -2. in
  let n = abs @@ (int_of_float x2 - int_of_float (x1 -. eps)) in
  Format.printf "%f::%f  %i " x1 x2 n;
  n

let q1 l =
  List.fold_left2
    (fun p time distance -> p * number_of_records (time, distance))
    1 (fst l) (snd l)

let merge_one_race (d, t) =
  let aux l =
    let ls = List.map string_of_int l in
    String.concat "" ls |> int_of_string
  in
  (aux d, aux t)

let q2 l = merge_one_race l |> number_of_records

let () =
  let x = input in
  let l = parse x in
  let i1 = q1 l in
  let i2 = q2 l in
  Format.printf "q1= %i\nq2= %i" i1 i2
