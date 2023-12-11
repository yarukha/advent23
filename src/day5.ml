[@@@warning "-32-33-69-26"]

let test =
  "seeds: 79 14 55 13\n\n\
   seed-to-soil map:\n\
   50 98 2\n\
   52 50 48\n\n\
   soil-to-fertilizer map:\n\
   0 15 37\n\
   37 52 2\n\
   39 0 15\n\n\
   fertilizer-to-water map:\n\
   49 53 8\n\
   0 11 42\n\
   42 0 7\n\
   57 7 4\n\n\
   water-to-light map:\n\
   88 18 7\n\
   18 25 70\n\n\
   light-to-temperature map:\n\
   45 77 23\n\
   81 45 19\n\
   68 64 13\n\n\
   temperature-to-humidity map:\n\
   0 69 1\n\
   1 0 69\n\n\
   humidity-to-location map:\n\
   60 56 37\n\
   56 93 4\n\n"

let read_file file = In_channel.with_open_bin file In_channel.input_all
let input = read_file "./input/day5.txt"

type map_t = { dest_start : int; source_start : int; length : int }
type map = map_t list
type data = { seeds : int list; maps : map list }

let pp_mt p mt =
  let open Format in
  fprintf p "dest=%i;source=%i;l=%i" mt.dest_start mt.source_start mt.length

let parse s =
  let open Angstrom in
  let space = take_while1 (function ' ' -> true | _ -> false) in
  let number =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  in
  let map_header =
    take_while1 (function 'a' .. 'z' | '-' -> true | _ -> false)
    *> string " map:"
  in
  let map_t =
    let* dest_start = number <* space in
    let* source_start = number <* space in
    let* length = number in
    return { dest_start; source_start; length }
  in
  let map =
    map_header *> end_of_line *> sep_by1 end_of_line map_t <* end_of_line
  in
  let seeds = string "seeds:" *> space *> sep_by1 space number <* end_of_line in
  let m =
    let* seeds = seeds <* end_of_line in
    let* maps = sep_by1 end_of_line map <* option () end_of_line in
    return { seeds; maps }
  in
  match parse_string ~consume:All m s with
  | Ok x -> x
  | Error msg -> failwith msg

let pass_through_map i l =
  match
    List.find_opt
      (fun m -> i >= m.source_start && i < m.source_start + m.length)
      l
  with
  | Some m -> i + m.dest_start - m.source_start
  | None -> i

let q1 d =
  let l =
    List.map (fun x -> List.fold_left pass_through_map x d.maps) d.seeds
  in
  (* List.iter2 (Format.printf "%i->%i\n") d.seeds l; *)
  List.fold_left min max_int l

let rec get_intervals = function
  | [] -> []
  | x :: y :: q -> (x, y) :: get_intervals q
  | _ -> assert false

let ( let* ) m f = List.concat_map f m
let return x = [ x ]

type slice = Changed of int * int | Unchanged of int * int

let pp_slice p =
  let open Format in
  function
  | Changed (s, l) -> fprintf p "C(%i,%i)" s l
  | Unchanged (s, l) -> fprintf p "U(%i,%i)" s l

let pp_slices =
  let open Format in
  pp_print_list ~pp_sep:(fun p () -> fprintf p ";") pp_slice

let normalize_slice = function
  | Changed (s, l) -> (s, l)
  | Unchanged (s, l) -> (s, l)

let slice_interval slices { dest_start; source_start; length } =
  let* x = slices in
  match x with
  | Changed (_, 0) | Unchanged (_, 0) -> []
  | Changed (s, l) -> [ Changed (s, l) ]
  | Unchanged (s, l) when s >= source_start && s + l < source_start + length ->
      [ Changed (s + dest_start - source_start, l) ]
  | Unchanged (s, l) when s >= source_start + length || s + l <= source_start ->
      [ Unchanged (s, l) ]
  | Unchanged (s, l) when s >= source_start && s + l >= source_start + length ->
      [
        Changed (s + dest_start - source_start, source_start + length - s);
        Unchanged (source_start + length, s + l - source_start - length);
      ]
  | Unchanged (s, l) when s < source_start && s + l < source_start + length ->
      [
        Changed (dest_start, s + l - source_start);
        Unchanged (s, s + l - source_start);
      ]
  | Unchanged (s, l) when s < source_start && s + l >= source_start + length ->
      [
        Changed (dest_start, length);
        Unchanged (s, source_start - s);
        Unchanged (source_start + length, s + l - source_start - length);
      ]
  | _ -> assert false

let slice_through_map slices (map : map) =
  List.fold_left
    (fun sl mt ->
      let sl' = slice_interval sl mt in
      sl')
    slices map
  |> List.map (function
       | Unchanged (i, j) -> Unchanged (i, j)
       | Changed (i, j) -> Unchanged (i, j))

let q2 d =
  let slices =
    get_intervals d.seeds |> List.map (fun (i, j) -> Unchanged (i, j))
  in
  List.fold_left
    (fun s m ->
      let x = slice_through_map s m in
      x)
    slices d.maps
  |> List.map normalize_slice
  |> List.fold_left (fun m (x, _) -> min x m) max_int

let () =
  let x = input in
  let l1 = parse x in
  let i1 = q1 l1 in
  let i2 = q2 l1 in
  Format.printf "q1 = %i\nq2 = %i\n" i1 i2
