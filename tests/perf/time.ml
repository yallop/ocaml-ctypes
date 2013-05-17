let time f =
  let t1 = Unix.gettimeofday () in
  Gc.compact ();
  for i = 0 to 30000000 do
    ignore(f i)
  done;
  let t2 = Unix.gettimeofday () in
  t2 -. t1
