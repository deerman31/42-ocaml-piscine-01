(* let rec iter f x n = if n <= 0 then x else iter f (f x) (n - 1) *)

let iter f x n =
  let rec loop acc n = if n <= 0 then acc else loop (f acc) (n - 1) in
  loop x n

let () =
  print_endline (string_of_int (iter (fun x -> x * x) 2 4));
  print_endline (string_of_int (iter (fun x -> x * 2) 2 4))
