module I = Z

let pr ch x =
  output_string ch (I.to_string x);
  flush ch

let chk_extract x o l =
  let expected =
    I.logand (I.shift_right x o) (I.pred (I.shift_left (I.of_int 1) l))
  and actual =
    I.extract x o l in
  if actual <> expected then (Printf.printf "extract %a %d %d = %a found %a\n" pr x o l pr expected pr actual; failwith "test failed")

let doit () =
  let max = 128 in
  for l = 1 to max do
    if l mod 16 == 0 then Printf.printf "%i/%i\n%!" l max;
    for o = 0 to 256 do
      for n = 0 to 256 do
        let x = I.shift_left I.one n in
        chk_extract x o l;
        chk_extract (I.mul x x) o l;
        chk_extract (I.mul x (I.mul x x)) o l;
        chk_extract (I.succ x) o l;
        chk_extract (I.pred x) o l;
        chk_extract (I.neg (I.mul x x)) o l;
        chk_extract (I.neg (I.mul x (I.mul x x))) o l;
        chk_extract (I.neg x) o l;
        chk_extract (I.neg (I.succ x)) o l;
        chk_extract (I.neg (I.pred x)) o l;
      done
    done
  done

let _ = doit ()
