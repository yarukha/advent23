let input =
  let f = open_in_bin "./input/day1.txt" in
  really_input_string f (in_channel_length f)

let () = Format.printf "%s" input
