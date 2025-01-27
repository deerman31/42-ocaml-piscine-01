let leibniz_pi delta =
  let pi = 4. *. atan 1. in

  let abs x = if x < 0.0 then -1.0 *. x else x in

  let leibniz_term i =
    let f_i = float_of_int i in
    4.0 *. (-1.0 ** f_i) /. ((2.0 *. f_i) +. 1.0)
  in

  if delta < 0. then -1
  else
    let rec loop acc i =
      if abs (acc -. pi) <= delta then i
      else loop (acc +. leibniz_term (i + 1)) (i + 1)
    in
    loop (leibniz_term 0) 0


let () =
   (* 正常系のテスト *)
   let tests =
     [
       (0.1, 9);
       (* 比較的大きいデルタ *)
       (0.01, 99);
       (* より小さいデルタ *)
       (0.001, 999);
       (* さらに小さいデルタ *)
       (-1.0, -1);
       (* 負のデルタ - エラーケース *)
     ]
   in

   List.iter
     (fun (delta, expected) ->
       let result = leibniz_pi delta in
       Printf.printf "delta: %f, iterations: %d, expected: %d\n" delta result
         expected;
       assert (result = expected))
     tests;

   print_endline "All tests passed!"
