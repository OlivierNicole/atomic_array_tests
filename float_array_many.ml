let rec my_length acc = function
  | [] -> acc
  | x :: xs ->
      Printf.eprintf "my_length step, head = %f\n%!" x;
      my_length (acc+1) xs

let () =
  let a = Array.make 8 1. in
  Array.iter (fun x -> Printf.eprintf "%f\n%!" x) a;
  let snoc xs x = x :: xs in
  let xs = [] in
  let xs = snoc xs @@ Atomic.Array.unsafe_exchange a 6 4.2 in
  (*
  let xs = snoc xs @@ Atomic.Array.get a 4 in
  let xs = snoc xs @@ Atomic.Array.unsafe_exchange a 4 ~-.9.44357030529e-07 in
  let xs = snoc xs @@ Atomic.Array.get a 1 in
  let xs = snoc xs @@ Atomic.Array.get a 5 in
  let xs = snoc xs @@ Atomic.Array.get a 7 in
  let xs = snoc xs @@ Atomic.Array.get a 5 in
  let xs = snoc xs @@ Atomic.Array.unsafe_exchange a 4 ~-.0.000125648518813 in
  let xs = snoc xs @@ Atomic.Array.get a 7 in
  let xs = snoc xs @@ Atomic.Array.get a 5 in
  let xs = snoc xs @@ Atomic.Array.get a 0 in
  let xs = snoc xs @@ Atomic.Array.get a 4 in
  let xs = snoc xs @@ Atomic.Array.unsafe_exchange a 7 4464.16791698 in
  let xs = snoc xs @@ Atomic.Array.get a 7 in
  let xs = snoc xs @@ Atomic.Array.unsafe_exchange a 5 ~-.4676.40386647 in
  let xs = snoc xs @@ Atomic.Array.get a 5 in
  let xs = snoc xs @@ Atomic.Array.get a 2 in
  let xs = snoc xs @@ Atomic.Array.get a 5 in
  let xs = snoc xs @@ Atomic.Array.get a 5 in
  let xs = snoc xs @@ Atomic.Array.unsafe_exchange a 4 0.0135874055988 in
  let xs = snoc xs @@ Atomic.Array.get a 3 in
  let xs = snoc xs @@ Atomic.Array.unsafe_exchange a 1 ~-.0.00292710502148 in
  let xs = snoc xs @@ Atomic.Array.unsafe_exchange a 3 ~-.0.39677959096 in
  let xs = snoc xs @@ Atomic.Array.get a 3 in
  let xs = snoc xs @@ Atomic.Array.get a 7 in
  let xs = snoc xs @@ Atomic.Array.get a 3 in
  let xs = snoc xs @@ Atomic.Array.get a 5 in
  let xs = snoc xs @@ Atomic.Array.unsafe_exchange a 1 ~-.0.00118059511759 in
  let xs = snoc xs @@ Atomic.Array.get a 5 in
  let xs = snoc xs @@ Atomic.Array.unsafe_exchange a 2 ~-.4.29502018843e-07 in
*)
  Printf.printf "%i\n" @@ my_length 0 xs
