let pp_array pp a =
  Printf.fprintf pp "[|";
  Array.iter (fun x -> Printf.fprintf pp "%f; " x) a;
  Printf.fprintf pp "|]"

let () =
  let a = Array.init 13 (fun i -> float_of_int i) in
  Printf.printf "initial array: %a\n" pp_array a;
  let x = Atomic.Array.get a 11 in
  Printf.printf "Atomic.Array.get a 11 -> %f\n" x;
  let x = Atomic.Array.unsafe_exchange a 11 42. in
  Printf.printf "unsafe_exchange a 11 42. -> %f\n" x;
  Printf.printf "a = %a\n" pp_array a;
  let b = Atomic.Array.compare_and_set a 12 12. 2048. in
  Printf.printf "Atomic.Array.compare_and_set a 12 12. 2048. -> %B\n" b;
  Printf.printf "a = %a\n" pp_array a;
