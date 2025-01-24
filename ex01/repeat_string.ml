let repeat_string ?(str = "x") n =
  if n < 0 then "Error"
  else
    let rec loop acc i = if i >= n then acc else loop (str ^ acc) (i + 1) in
    loop "" 0

let test () =
  print_endline (repeat_string (-1));
  print_endline (repeat_string 0);
  print_endline (repeat_string ~str:"Toto" 1);
  print_endline (repeat_string 2);
  print_endline (repeat_string ~str:"a" 5);
  print_endline (repeat_string ~str:"what" 3)

let () = test ()
