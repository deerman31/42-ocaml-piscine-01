let repeat_x n =
  if n < 0 then "Error"
  else
    let rec loop acc i = if i >= n then acc else loop ("x" ^ acc) (i + 1) in
    loop "" 0

let test () =
  print_endline (repeat_x (-1));
  print_endline (repeat_x 0);
  print_endline (repeat_x 1);
  print_endline (repeat_x 2);
  print_endline (repeat_x 5)

let () = test ()
