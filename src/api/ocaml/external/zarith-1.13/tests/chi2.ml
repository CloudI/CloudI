(* Accumulate [n] samples from function [f] and check the chi-square.
   Only the low 8 bits of the result of [f] are sampled. *)

let chisquare n f =
  let r = 256 in
  let freq = Array.make r 0 in
  for i = 0 to n - 1 do
    let t = Z.to_int (Z.logand (f ()) (Z.of_int 0xFF)) in
    freq.(t) <- freq.(t) + 1
  done;
  let expected = float n /. float r in
  let t =
    Array.fold_left
      (fun s x -> let d = float x -. expected in d *. d +. s)
      0.0 freq in
  let chi2 = t /. expected in
  let degfree = float r -. 1.0 in
  (* The degree of freedom is high, so we approximate as a normal
     distribution with mean equal to degfree and variance 2 * degfree.
     Four sigmas correspond to a 99.9968% confidence interval.
     (Without the approximation, the confidence interval seems to be 99.986%.)
  *)
  chi2 <= degfree +. 4.0 *. sqrt (2.0 *. degfree)

let test name f =
  if not (chisquare 100_000 f)
  then Printf.printf "%s: suspicious result\n%!" name

let _ =
  test "random_bits 15 (bits 0-7)"
       (fun () -> Z.random_bits 15);
  test "random_bits 32 (bits 12-19)"
       (fun () -> Z.(shift_right (random_bits 32) 12));
  test "random_bits 31 (bits 23-30)"
       (fun () -> Z.(shift_right (random_bits 31) 23));
  test "random_int 2^30 (bits 0-7)"
       (fun () -> Z.(random_int (shift_left one 30)));
  test "random_int 2^30 (bits 21-28)"
       (fun () -> Z.(shift_right (random_int (shift_left one 30)) 21));
  test "random_int (256 * p) / p"
       (let p = Z.of_string "35742549198872617291353508656626642567" in
        let bound = Z.shift_left p 8 in
        fun () -> Z.(div (random_int bound) p))
