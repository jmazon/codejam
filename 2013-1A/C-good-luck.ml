(* So, I kind of ported this from my Haskell code… *)
let replicate n e =
  let rec go a = function
    | 0 -> a
    | n -> go (e::a) (n-1)
  in go [] n;;
let lookup h k =
  try Hashtbl.find h k
  with Not_found -> 0.0;;
let next_int () = Scanf.scanf " %d" (fun x -> x);;

(* …and I already didn't really want to bother with memoization *)
let choose = function
  |  0 -> (fun _ -> 1)
  |  1 -> (fun _ -> 1)
  |  2 -> List.nth [1;2;1]
  |  3 -> List.nth [1;3;3;1]
  |  4 -> List.nth [1;4;6;4;1]
  |  5 -> List.nth [1;5;10;10;5;1]
  |  6 -> List.nth [1;6;15;20;15;6;1]
  |  7 -> List.nth [1;7;21;35;35;21;7;1]
  |  8 -> List.nth [1;8;28;56;70;56;28;8;1]
  |  9 -> List.nth [1;9;36;84;126;126;84;36;9;1]
  | 10 -> List.nth [1;10;45;120;210;252;210;120;45;10;1]
  | 11 -> List.nth [1;11;55;165;330;462;462;330;165;55;11;1]
  | 12 -> List.nth [1;12;66;220;495;792;924;792;495;220;66;12;1]
  |  n -> invalid_arg (string_of_int n);;
let multichoose n k = choose (n+k-1) k;;

(* Anyway, main starts here. *)
let t = next_int() in if t <> 1 then raise Exit; print_endline "Case #1:";
let r = next_int() and
    n = next_int() and
    m = next_int() and
    k = next_int() in
let nss = Array.make_matrix r k 0 and
    pbs = Array.make r 0.0 and
    fss = Array.make r [] in
for i = 0 to r-1 do for j = 0 to k-1 do nss.(i).(j) <- next_int() done done;
let rec draw w s nn fs =       (* This is where I wish I remembered *)
  if nn <> 0 then              (* how to elegantly use streams.     *)
    for d = s to m do
      for c = 1 to nn do
        draw (w * multichoose (nn-c+1) c) (d+1) (nn-c) (replicate c d @ fs)
      done
    done
  else begin
    let products =
      let rec h = Hashtbl.create 256 and
              go a = function
                | (f :: fs) -> go (f*a) fs; go a fs
                | [] -> Hashtbl.replace h a 
                          (lookup h a +. 2.0 ** float_of_int (-n))
      in go 1 fs; h            (* If you were thinking my OCaml *)
    in for i = 0 to r-1 do     (* is indented like Haskell…     *)
         let pb = float_of_int w                            *.
                  Array.fold_left ( *. ) 1.0 
                    (Array.map (lookup products) nss.(i))   /.
                  float_of_int (m-1) ** float_of_int n
         in if pb > pbs.(i) then begin
           pbs.(i) <- pb; 
           fss.(i) <- fs;
         end                   (* …well, you couldn't   *)
      done                     (* really be more right. *)
  end
in draw 1 2 n [];
for i = 0 to r-1 do
  List.iter print_int fss.(i);
  print_newline();
done;;
