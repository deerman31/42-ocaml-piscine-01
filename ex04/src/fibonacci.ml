(*
素直にフィボナッチ数列の数式を再帰関数に表すと以下のような関数となる。
普通の再帰では計算処理などは深く考えなくても、数式をそのままコードに起こしていくだけで完成する。
しかし、普通の再帰関数では大きい入力などがあると再帰によるスタックの積まれすぎで、スタックオーバーフローを起こすため、
末尾再帰にする必要が生じる。

let rec fibonacci n =
   if n < 0 then -1
   else if n = 0 || n = 1 then n
   else fibonacci (n - 2) + fibonacci (n - 1) *)

let fibonacci n =
  if n < 0 then -1
  else
    let rec loop n a b =
      if n = 0 then a else if n = 1 then b else loop (n - 1) b (a + b)
    in
    loop n 0 1

let test () =
  print_endline (string_of_int (fibonacci (-42)));
  print_endline (string_of_int (fibonacci 1));
  print_endline (string_of_int (fibonacci 3));
  print_endline (string_of_int (fibonacci 6))

let () = test ()
