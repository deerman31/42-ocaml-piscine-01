(*
andキーワードは相互再帰関数を定義する際に使用する.
複数の関数が互いを参照し合う場合, let recで最初の関数を定義し, 続く関数をandで接続する.
これにより,定義順序に関係なく関数が互いを認識できる.
*)
let rec hfs_f n = if n = 0 then 1 else n - hfs_m (hfs_f (n - 1))
and hfs_m n = if n = 0 then 0 else n - hfs_f (hfs_m (n - 1))

let test () =
  print_endline (string_of_int (hfs_m 0));
  print_endline (string_of_int (hfs_f 0));
  print_endline (string_of_int (hfs_m 4));
  print_endline (string_of_int (hfs_f 4))

let () = test ()
