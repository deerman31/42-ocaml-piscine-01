let ft_sum f lower upper =
  if lower > upper then nan
  else
    let rec loop acc n = if n > upper then acc else loop (acc +. f n) (n + 1) in
    loop 0. lower

let () = Printf.printf "%f\n" (ft_sum (fun i -> float_of_int (i * i)) 1 10)
