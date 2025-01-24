let rec iter f x n = if n <= 0 then x else iter f (f x) (n - 1)

let test () =
  print_endline (string_of_int (iter (fun x -> x * x) 2 4));
  print_endline (string_of_int (iter (fun x -> x * 2) 2 4))

let () = test ()
