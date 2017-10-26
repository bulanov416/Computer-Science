 
(* -------------------------------------------------------------------------- *)
(* ----- Problem 1: Compute the Complementary Helix ------------------------- *)
(* -------------------------------------------------------------------------- *)
 
(* DNA has two complementary helices, each a
  sequence of the nucleotides adenine, thymine, guanine, and cytosine.
  Adenine always appears opposite thymine; same for guanine and cytosine. *)
 
type nucleotide = G | C | A | T
type helix = nucleotide list
 
let rec append lst1 lst2 =
   match lst1 with
   | [] -> lst2
   | h::t -> h::(append t lst2)
;;
let opposite n : nucleotide =
	
	if n = G then C 
	else if n = C then G 
	else if n = T then A 
	else if n = A then T else G
;;

(* Given a single helix, compute the other half of the double helix *)
let rec complementary_helix (x: helix): helix = 
	match x with 
	| [] -> []
	| h::t -> opposite (h)::complementary_helix (t)
;;
complementary_helix [T;A;G;C];;

 (* Above Works *)

let lar_gibbon: helix = [T; A; T; A; A; A; G; A; G; A; G; T; A; A; A; A; A; G; T; G; T; A; A; A; C; C; C; C; A; T; A; G; T; T; G; G; C; C; T; A; A; A; A; G; C; A; G; C; C; A; C; C; A; A; T; T; A; A; G; A; A; A; G; C; G; T; T; C; A; A; G; C; T; C; A; A; C; A; C; C; A; C; C; T; A; T; C; C; A; A; C; A; A; A; T; C; C; C; A; A; A; C; A; C; A; C; A; A; C; T; G; A; A; C; T; C; C; T; T; C; C; A; C; C; A; C; A; T; T; G; G; A; C; C; A; A; T; C; T; A; T; C; A; T; T; T; T; A; T; A; G; A; A; G; A; A; A; T; A; A; T; G; T; T; A; G; T; A; T; A; A; G; T; A; A; C; A; T; G; A; A; T; A; A; C; A; T; T; C; T; C; C; C; C; C; G; C; A; T; A; A; A; C; C; T; A; T; A; T; C; A; G; A; C; C; A; A; A; A; A; A; C; T; T; C; G; C; T; G; A; C; A; G; T; T; A; A; C; A; G; C; C; C; A; A; T; A; T; C; T; A; A; A; A; C; C; A; A; C; T; G; A; T; A; A; A; C; C; A; T; T; A; T; T; G; C; C; C; A; C; A; C; T; G; T; C; A; A; C; C; C; A; A; C; A; T; A; G; G; C; A; T; G; C; C; C; A; C; A; A; G; G; A; A; A; G; G; T; T; A; A; A; A; A; A; A; G; T; A; A; A; A; G; G; A; A; C; T; C; G; G; C; A; A; A; C; A; C; T; A; C; C; C; C; G; C; C; T; G; T; T; T; A; C; C; A; A; A; A; A; C; A; T; C; A; C; C; T; C; T; A; G; C; A; T; T; A; C; C; A; G; T; A; T; T; A; G; A; G; G; C; A; C; C; G; C; C; T; G; C; C; C; A; G; T; G; A; C; A; C; A; T; G; T; T; C; A; A; C; G; G; C; C; G; C; G; G; T; A; C; C; C; T; A; A; C; C; G; T; G; C; A; A; A; G; G; T; A; G; C; A; T; A; A; T; C; A; C; T; T; G; T; T; C; C; T; T; A; A; A; T; G; G; G; G; A; C; T; T; G; T; A; T; G; A; A; T; G; G; C; T; C; C; A; C; G; A; G; G; G; T; T; C; A; G; C; T; G; T; C; T; C; T; T; A; C; T; T; T; C; A; A; C; C; A; G; T; G; A; A; A; T; T; G; A; C; C; T; G; T; C; C; G; T; G; A; A; G; A; G; G; C; G; G; A; C; A; T; A; A; C; C; T; A; A; C; A; A; G; A; C; G; A; G; A; A; G; A; C; C; C; T; A; T; G; G; A; G; C; T; T; T; A; G; T; C; T; A; T; C; A; A; T; G; C; A; A; A; C; A; A; C; A; T; T; C; A; A; T; A; A; A; C; C; A; A; C; A; G; G; T; C; A; T; A; A; A; T; T; A; C; C; A; A; A; C; C; T; G; C; A; T; C; G; A; A; G; A; C; T; T; C; G; G; T; T; G; G; G; G; C; G; A; C; C; T; C; G; G; A; G; C; A; T; A; G; A; C; T; A; A; C; C; T; C; C; G; A; G; C; A; G; T; A; T; A; T; G; C; T; A; A; G; A; C; C; A; C; A; C; C; A; G; T; C; A; A; A; A; C; G; A; A; A; C; T; C; C; A; T; G; T; G; C; A; A; T; T; G; A; C; C; C; A; A; T; A; A; C; T; T; G; A; T; C; A; A; C; G; G; A; A; C; A; A; G; T; T; A; C; C; C; T; A; G; G; G; A; T; A; A; C; A; G; C; G; C; A; A; T; C; C; T; A; T; T; C; T; A; G; A; G; T; C; C; A; T; A; T; C; A; A; C; A; A; T; A; G; G; G; T; T; T; A; C; G; A; C; C; T; C; G; A; T; G; T; T; G; G; A; T; C; A; G; G; A; C; A; T; C; C; C; G; A; T; G; G; T; G; C; A; G; C; C; G; C; T; A; T; C; A; A; A; G; G; T; T; C; G; T; T; T; G; T; T; C; A; A; C; G; A; T; T; A; A; A; G; T; C; C; T; A; C; G; T; G; A; T; C; T; G; A; G; T; T; C; A; G; A; C; C; G; G; A; G; T; A; A; T; C; C; A; G; G; T; C; G; G; T; T; T; C; T; A; T; C; T; G; T; T; C; T; A; T; A; T; T; T; C; T; C; C; C; T; G; T; A; C; G; A; A; A; G; G; A; C; A; A; G; A; G; A; A; A; T; A; G; G; G; C; C; C; A; C; T; T; C; G; C; A]
 
let pileated_gibbon: helix = [T; A; T; A; A; A; G; A; G; A; G; T; A; A; A; A; A; G; T; G; T; A; A; A; C; C; C; C; A; T; A; G; T; T; G; G; C; C; T; A; A; A; A; G; C; A; G; C; C; A; C; C; A; A; T; T; A; A; G; A; A; A; G; C; G; T; T; C; A; A; G; C; T; C; A; A; C; A; C; C; A; C; C; C; A; C; C; C; A; A; T; A; A; A; T; C; C; C; A; A; A; C; A; T; A; T; A; A; C; T; G; A; A; C; T; C; C; T; T; C; C; A; C; C; A; C; A; T; T; G; G; A; C; C; A; A; T; C; T; A; T; C; A; T; T; C; T; A; T; A; G; A; A; G; A; A; A; T; A; A; T; G; T; T; A; A; T; A; T; G; A; G; T; A; A; C; A; C; G; A; A; A; A; G; A; A; T; T; C; T; C; C; T; C; C; G; C; A; T; A; A; G; C; C; T; A; T; A; T; C; A; G; A; C; C; A; A; A; A; A; G; A; C; T; T; C; G; C; T; G; A; C; A; G; T; T; A; A; C; A; G; C; T; C; A; A; T; A; T; C; T; A; A; A; A; C; C; A; A; C; T; G; A; T; A; G; A; C; C; A; T; T; A; T; T; A; C; C; C; A; C; A; C; T; G; T; C; A; A; C; C; C; A; A; C; A; T; A; G; G; C; A; T; G; C; C; C; A; C; A; A; G; G; A; A; A; G; G; T; T; A; A; A; A; A; A; A; G; T; A; A; A; A; G; G; A; A; C; T; C; G; G; C; A; A; A; C; A; C; T; A; C; C; C; C; G; C; C; T; G; T; T; T; A; C; C; A; A; A; A; A; C; A; T; C; A; C; C; T; C; T; A; G; C; A; T; T; A; C; C; A; G; T; A; T; T; A; G; A; G; G; C; A; C; C; G; C; C; T; G; C; C; C; A; G; T; G; A; C; A; C; A; T; G; T; T; C; A; A; C; G; G; C; C; G; C; G; G; T; A; C; C; C; T; A; A; C; C; G; T; G; C; A; A; A; G; G; T; A; G; C; A; T; A; A; T; C; A; C; T; T; G; T; T; C; C; T; T; A; A; A; T; G; G; G; G; A; C; T; T; G; T; A; T; G; A; A; T; G; G; C; T; C; C; A; C; G; A; G; G; G; T; C; C; A; G; C; T; G; T; C; T; C; T; T; A; C; T; T; T; C; A; A; C; C; A; G; T; G; A; A; A; T; T; G; A; C; T; T; G; T; C; C; G; T; G; A; A; G; A; G; G; C; G; G; A; C; A; T; A; G; C; C; T; A; A; C; A; A; G; A; C; G; A; G; A; A; G; A; C; C; C; T; A; T; G; G; A; G; C; T; T; T; A; G; C; C; T; A; T; C; A; A; T; G; C; A; A; A; C; A; A; T; A; T; T; C; A; A; C; A; A; A; C; C; A; A; C; A; G; G; C; C; G; T; A; A; A; C; T; A; C; C; A; A; A; T; C; T; G; C; A; T; C; G; A; A; G; A; C; T; T; C; G; G; T; T; G; G; G; G; C; G; A; C; C; T; C; G; G; A; G; C; A; T; A; A; A; C; T; A; A; C; C; T; C; C; G; A; G; C; A; G; T; A; C; A; T; G; C; T; A; A; G; A; C; C; A; C; A; C; C; A; G; T; C; A; A; A; A; C; A; A; A; A; T; T; C; C; A; T; A; T; G; C; A; A; T; T; G; A; C; C; C; A; A; T; A; A; T; T; T; G; A; T; C; A; A; C; G; G; A; A; C; A; A; G; T; T; A; C; C; C; T; A; G; G; G; A; T; A; A; C; A; G; C; G; C; A; A; T; C; C; T; A; T; T; C; T; A; G; A; G; T; C; C; A; T; A; T; C; A; A; C; A; A; T; A; G; G; G; T; T; T; A; C; G; A; C; C; T; C; G; A; T; G; T; T; G; G; A; T; C; A; G; G; A; C; A; T; C; C; C; G; A; T; G; G; T; G; C; A; G; C; C; G; C; T; A; T; C; A; A; A; G; G; T; T; C; G; T; T; T; G; T; T; C; A; A; C; G; A; T; T; A; A; A; G; T; C; C; T; A; C; G; T; G; A; T; C; T; G; A; G; T; T; C; A; G; A; C; C; G; G; A; G; T; A; A; T; C; C; A; G; G; T; C; G; G; T; T; T; C; T; A; T; C; T; G; T; T; C; T; A; T; A; T; T; T; C; T; C; C; C; T; G; T; A; C; G; A; A; A; G; G; A; C; A; A; G; A; G; A; A; A; T; G; G; G; G; C; C; C; A; C; T; T; C; A; C]
 
let siamang: helix = [T; A; T; A; A; A; A; C; C; T; A; G; A; C; A; A; A; A; A; A; G; G; A; A; G; G; A; A; T; C; G; A; A; C; C; C; T; C; T; A; A; A; A; C; C; G; G; T; T; T; C; A; A; G; C; C; A; G; C; C; C; C; A; T; A; A; C; C; T; T; T; A; T; G; A; C; T; T; T; T; T; C; A; A; A; A; A; G; A; T; A; T; T; A; G; A; A; A; A; A; C; T; A; T; T; T; C; A; T; A; A; C; T; T; T; G; T; C; A; A; A; G; T; T; A; A; A; T; C; A; C; A; G; G; T; T; C; A; A; A; C; C; C; C; G; T; A; T; A; T; C; T; T; A; A; T; G; G; C; A; C; A; T; G; C; A; G; C; T; C; A; A; G; T; A; G; G; C; C; T; A; C; A; A; G; A; C; G; C; T; A; C; A; T; C; C; C; C; T; A; T; C; A; T; A; G; A; A; G; A; A; C; T; A; A; T; C; T; C; T; T; T; C; C; A; C; G; A; C; C; A; T; G; C; C; C; T; C; A; T; A; A; T; C; A; T; T; T; T; C; C; T; T; A; T; C; A; G; C; T; T; C; C; T; A; G; T; T; C; T; A; T; A; T; G; C; C; C; T; C; T; T; C; C; T; A; A; C; A; C; T; C; A; C; A; A; C; A; A; A; A; C; T; A; A; C; C; A; A; C; A; C; T; A; A; T; A; T; T; A; C; G; G; A; C; G; C; C; C; A; A; G; A; G; A; T; A; G; A; A; A; C; C; G; T; C; T; G; A; A; C; A; A; T; C; C; T; A; C; C; T; G; C; T; A; T; T; A; T; T; C; T; A; G; T; T; C; T; A; A; T; C; G; C; C; C; T; C; C; C; A; T; C; C; C; T; C; C; G; C; A; T; C; C; T; T; T; A; T; T; T; G; A; C; A; G; A; C; G; A; A; A; T; C; A; A; C; G; A; C; C; C; C; T; C; C; T; T; T; A; C; C; A; T; C; A; A; G; G; C; A; A; T; C; G; G; T; C; A; T; C; A; G; T; G; A; T; A; C; T; G; A; G; C; C; T; A; C; G; A; A; T; A; T; A; C; A; G; A; C; T; A; C; G; G; T; G; G; G; C; T; A; A; T; C; T; T; C; A; A; T; T; C; T; T; A; C; A; T; A; T; T; A; C; C; A; C; C; A; T; T; A; T; T; T; C; T; A; G; A; A; C; C; A; G; G; G; G; A; C; C; T; T; C; G; A; C; T; C; C; T; T; G; A; A; G; T; C; G; A; C; A; A; C; C; G; A; G; T; A; G; T; T; C; T; T; C; C; A; A; T; T; G; A; A; G; C; C; C; C; T; G; T; C; C; G; T; A; T; A; A; T; A; A; T; T; A; C; A; T; C; A; C; A; A; G; A; C; G; T; C; C; T; A; C; A; C; T; C; A; T; G; A; A; C; T; G; T; C; C; C; C; T; C; C; C; T; A; G; G; T; C; T; G; A; A; A; A; C; G; G; A; T; G; C; T; A; T; C; C; C; C; G; G; A; C; G; C; C; T; A; A; A; C; C; A; A; A; C; C; A; C; A; T; T; C; A; C; C; G; C; C; A; C; A; C; G; C; C; C; A; G; G; A; G; T; A; T; A; C; T; A; C; G; G; C; C; A; A; T; G; C; T; C; A; G; A; A; A; T; C; T; G; T; G; G; A; G; C; C; A; A; C; C; A; T; A; G; C; T; T; T; A; T; A; C; C; G; A; T; T; G; T; T; C; T; A; G; A; A; C; T; A; A; T; T; C; C; C; T; T; A; A; A; A; A; T; C; T; T; C; G; A; A; A; T; A; G; G; G; C; C; T; G; T; G; T; T; T; A; C; C; C; T; A; T; A; A; C; C; C; C; A; C; C; C; T; C; T; G; C; C; C; C; C; C; G; T; A; A; A; T; C; T; C; A; C; T; G; T; A; G; A; G; C; T; A; G; A; C; C; A; G; C; A; T; T; A; A; C; C; T; T; T; T; A; A; G; T; T; A; A; A; G; A; C; T; A; A; G; A; G; A; A; C; T; A; C; C; A; C; C; T; C; T; T; T; A; C; A; G; T; G; A; A; A; T; G; C; C; C; C; A; A; T; T; A; A; A; C; A; C; C; A; C; C; G; T; G; T; G; A; C; C; T; A; T; A; A; T; C; A; T; C; A; C; A; T; C; A; A; T; A; C; T; T; C; T; C; A; C; A; C; T; A; T; T; C; C; T; C; C; T; C; A; T; A; C; A; A; C; T; A; A; A; A; A; C; A; C]
 
let white_cheeked_gibbon: helix = [T; A; T; A; A; A; A; C; C; T; A; G; A; C; A; A; A; A; A; A; G; G; A; A; G; G; A; A; T; C; G; A; A; C; C; C; C; C; T; A; A; A; A; C; T; G; G; T; T; T; C; A; A; G; C; C; A; G; C; C; C; C; A; T; A; A; C; C; T; C; T; A; T; G; A; C; T; T; T; T; T; C; A; A; A; A; G; G; T; A; T; T; A; G; A; A; A; A; G; C; T; A; T; T; T; C; A; T; A; A; C; T; T; T; G; T; C; A; A; A; G; T; T; A; A; A; T; C; A; C; A; G; G; T; T; C; A; A; G; C; C; C; C; G; T; A; T; A; T; C; T; T; A; A; T; G; G; C; A; C; A; T; G; C; A; A; C; T; C; A; A; G; T; A; G; G; C; C; T; A; C; A; A; G; A; C; G; C; T; A; C; A; T; C; C; C; C; C; A; T; C; A; T; A; G; A; A; G; A; A; C; T; A; A; T; C; T; C; T; T; T; C; C; A; C; G; A; C; C; A; C; G; C; C; C; T; C; A; T; A; A; T; C; A; T; T; T; T; C; C; T; T; A; T; C; A; G; C; T; T; C; C; T; G; G; T; C; C; T; A; T; A; T; G; C; C; C; T; C; T; T; C; C; T; A; A; C; A; C; T; C; A; C; A; A; C; A; A; A; A; C; T; A; A; C; C; A; A; C; A; C; T; A; A; C; A; T; T; A; C; A; G; A; C; G; C; C; C; A; A; G; A; G; A; T; A; G; A; A; A; C; C; A; T; C; T; G; A; A; C; A; A; T; C; C; T; A; C; C; T; G; C; C; A; T; T; A; T; C; C; T; A; G; T; C; C; T; A; A; T; C; G; C; T; C; T; C; C; C; A; T; C; C; C; T; C; C; G; T; A; T; T; C; T; C; T; A; T; T; T; A; A; C; A; G; A; C; G; A; A; A; T; C; A; A; C; G; A; C; C; C; T; T; C; C; C; T; C; A; C; C; A; T; C; A; A; A; G; C; A; A; T; C; G; G; C; C; A; T; C; A; A; T; G; A; T; A; C; T; G; A; G; C; C; T; A; C; G; A; A; T; A; T; A; C; A; G; A; C; T; A; C; G; G; C; G; G; A; C; T; G; G; T; C; T; T; C; A; A; T; T; C; C; T; A; C; A; T; G; C; T; C; C; C; A; C; C; A; T; T; A; T; T; T; C; T; A; G; A; A; C; C; C; G; G; A; G; A; C; C; T; T; C; G; A; C; T; C; C; T; T; G; A; A; G; T; C; G; A; C; A; A; T; C; G; A; G; T; A; G; T; T; C; T; C; C; C; A; A; T; T; G; A; A; G; C; C; C; C; T; G; T; T; C; G; T; A; T; A; A; T; A; A; T; T; A; C; A; T; C; A; C; A; A; G; A; C; G; T; C; C; T; A; C; A; C; T; C; A; T; G; A; A; C; T; G; T; C; C; C; C; T; C; C; C; T; C; G; G; C; C; T; A; A; A; A; A; C; A; G; A; C; G; C; C; A; T; C; C; C; T; G; G; A; C; G; C; C; T; A; A; A; C; C; A; A; A; C; C; A; C; A; T; T; T; A; C; C; G; C; C; A; C; A; C; G; C; C; C; A; G; G; A; G; T; A; T; A; T; T; A; C; G; G; C; C; A; A; T; G; C; T; C; A; G; A; A; A; T; C; T; G; C; G; G; A; G; C; C; A; A; C; C; A; T; A; G; C; T; T; T; A; T; A; C; C; A; A; T; C; G; T; C; C; T; A; G; A; G; C; T; A; A; T; T; C; C; C; T; T; A; A; A; A; A; T; C; T; T; C; G; A; A; A; T; A; G; G; G; C; C; C; G; T; A; T; T; C; A; C; C; C; T; A; T; A; A; T; T; A; G; C; C; C; C; T; C; C; C; C; A; C; C; C; T; A; C; G; C; A; A; A; T; T; T; C; A; C; T; G; T; A; G; A; G; C; T; A; G; A; C; T; A; G; C; A; T; T; A; A; C; C; T; T; T; T; A; A; G; T; T; A; A; A; G; A; C; T; A; A; G; A; G; A; A; G; C; A; T; T; A; C; C; T; C; T; T; T; A; C; A; G; T; G; A; A; A; T; G; C; C; C; C; A; A; T; T; A; A; A; C; A; C; C; A; C; C; G; T; A; T; G; A; C; C; C; A; C; A; A; T; T; A; T; C; A; T; G; T; C; A; A; T; A; C; T; T; C; T; C; G; C; G; C; T; A; T; T; C; C; T; C; C; T; A; A; T; C; C; A; A; C; T; A; A; A; A; A; C; A]
 
let orangutan: helix = [T; A; T; A; A; A; A; C; C; C; C; C; T; G; C; A; A; T; A; T; C; C; C; A; A; T; A; C; C; A; A; A; C; C; C; C; C; C; T; C; T; T; C; G; T; C; T; G; A; T; C; A; G; T; C; T; T; G; A; T; C; A; C; A; G; C; A; G; T; C; C; T; A; C; T; T; C; T; C; C; T; T; T; C; C; C; T; C; C; C; A; G; T; C; C; T; A; G; C; C; G; C; T; G; G; C; A; T; C; A; C; C; A; T; A; C; T; A; C; T; A; A; C; A; G; A; T; C; G; C; A; A; C; C; T; A; A; A; C; A; C; C; A; C; A; T; T; C; T; T; T; G; A; C; C; C; A; G; C; C; G; G; A; G; G; T; G; G; A; G; A; T; C; C; C; A; T; C; C; T; A; T; A; T; C; A; G; C; A; C; C; T; A; T; T; C; T; G; A; T; T; T; T; T; T; G; G; C; C; A; C; C; C; T; G; A; A; G; T; C; T; A; C; A; T; T; C; T; C; A; T; C; C; T; G; C; C; G; G; G; T; T; T; C; G; G; C; A; T; A; A; T; C; T; C; C; C; A; C; A; T; C; G; T; A; A; C; A; C; A; C; T; A; T; T; C; C; G; G; A; A; A; A; G; A; A; G; A; G; C; C; A; T; T; T; G; G; G; T; A; C; A; T; A; G; G; C; A; T; A; G; T; C; T; G; A; G; C; C; A; T; A; G; T; C; T; C; A; A; T; T; G; G; C; T; T; C; C; T; G; G; G; C; T; T; T; A; T; C; G; T; A; T; G; G; G; C; C; C; A; C; C; A; C; A; T; A; T; T; C; A; C; A; G; T; A; G; G; A; A; T; A; G; A; C; G; T; G; G; A; C; A; C; A; C; G; A; G; C; C; T; A; C; T; T; C; A; C; C; T; C; C; G; C; T; A; C; C; A; T; A; A; T; C; A; T; T; G; C; C; A; T; C; C; C; C; A; C; C; G; G; C; G; T; C; A; A; A; G; T; A; T; T; T; A; G; C; T; G; A; C; T; C; G; C; T; A; C; A; C; T; C; C; A; C; G; G; A; A; G; C; A; A; C; A; C; T; A; A; A; T; G; A; T; C; T; G; C; C; G; C; A; A; T; C; C; T; C; T; G; A; G; C; C; T; T; A; G; G; A; T; T; C; A; T; T; T; T; C; C; T; C; T; T; C; A; C; C; G; T; A; G; G; C; G; G; C; C; T; A; A; C; A; G; G; C; A; T; C; G; T; A; C; T; A; G; C; A; A; A; C; T; C; A; T; C; A; C; T; A; G; A; C; A; T; T; G; T; A; T; T; A; C; A; C; G; A; T; A; C; A; T; A; C; T; A; C; G; T; T; G; T; A; G; C; C; C; A; C; T; T; T; C; A; T; T; A; C; G; T; C; C; T; A; T; C; A; A; T; A; G; G; A; G; C; T; G; T; A; T; T; C; G; C; C; A; T; C; A; T; G; G; G; A; G; G; C; T; T; C; A; T; C; C; A; C; T; G; G; T; T; C; C; C; A; C; T; A; T; T; C; T; C; A; G; G; C; T; A; C; A; C; C; T; T; A; G; A; C; C; A; G; A; C; C; T; A; T; G; C; T; A; A; A; A; T; T; C; A; C; T; T; C; A; T; C; A; C; C; A; T; A; T; T; T; A; T; C; G; G; C; G; T; A; A; A; T; T; T; A; A; C; T; T; T; C; T; T; C; C; C; A; C; A; A; C; A; T; T; T; C; C; T; C; G; G; C; C; T; G; T; C; A; G; G; C; A; T; A; C; C; C; C; G; A; C; G; C; T; A; C; T; C; C; G; A; C; T; A; C; C; C; C; G; A; C; G; C; G; T; A; C; A; C; C; A; C; C; T; G; A; A; A; T; A; T; T; T; T; A; T; C; A; T; C; C; G; C; A; G; G; C; T; C; A; T; T; T; A; T; C; T; C; C; C; T; A; A; C; A; G; C; A; G; T; C; A; T; A; C; T; A; A; T; A; A; T; T; T; T; C; A; T; A; A; T; T; T; G; A; G; A; A; G; C; C; T; T; C; G; C; C; T; C; A; A; A; A; C; G; A; A; A; A; G; T; C; C; C; A; A; T; A; G; T; T; G; A; A; C; A; A; C; C; C; T; C; C; A; C; A; A; G; C; C; T; A; G; A; G; T; G; A; T; T; G; T; A; C; G; G; A; T; G; C; C; C; C; C; C; A; C; C; C; T; A; C; C; A; C; A; C; A; T; T; T; G; A; A; G; A; A; C; C; C; G; T; C; T; A]
 
let gorilla: helix = [T; A; T; A; A; A; A; C; C; C; C; C; C; G; C; C; A; T; A; A; C; C; C; A; A; T; A; C; C; A; A; A; C; A; C; C; C; C; T; T; T; T; C; G; T; C; T; G; A; T; C; C; G; T; C; C; T; A; A; T; C; A; C; A; G; C; A; G; T; C; T; T; A; C; T; T; C; T; T; C; T; A; T; C; T; C; T; C; C; C; A; G; T; A; C; T; A; G; C; T; G; C; T; G; G; A; A; T; T; A; C; C; A; T; A; T; T; A; T; T; A; A; C; A; G; A; C; C; G; T; A; A; C; C; T; C; A; A; C; A; C; C; A; C; C; T; T; T; T; T; C; G; A; C; C; C; A; G; C; C; G; G; A; G; G; A; G; G; A; G; A; T; C; C; T; A; T; C; C; T; A; T; A; C; C; A; A; C; A; C; T; T; A; T; T; C; T; G; A; T; T; T; T; T; T; G; G; A; C; A; C; C; C; C; G; A; A; G; T; T; T; A; C; A; T; T; C; T; A; A; T; C; C; T; A; C; C; A; G; G; C; T; T; C; G; G; A; A; T; A; A; T; C; T; C; C; C; A; C; A; T; T; G; T; A; A; C; T; T; A; T; T; A; C; T; C; C; G; G; A; A; A; A; A; A; A; G; A; A; C; C; A; T; T; C; G; G; A; T; A; T; A; T; A; G; G; T; A; T; A; G; T; C; T; G; A; G; C; T; A; T; A; A; T; A; T; C; A; A; T; T; G; G; T; T; T; C; C; T; G; G; G; A; T; T; T; A; T; T; G; T; G; T; G; A; G; C; C; C; A; C; C; A; C; A; T; A; T; T; T; A; C; A; G; T; A; G; G; A; A; T; A; G; A; C; G; T; A; G; A; T; A; C; A; C; G; A; G; C; C; T; A; C; T; T; C; A; C; C; T; C; C; G; C; T; A; C; C; A; T; A; A; T; C; A; T; C; G; C; T; A; T; C; C; C; C; A; C; C; G; G; C; G; T; C; A; A; A; G; T; A; T; T; C; A; G; C; T; G; A; C; T; C; G; C; T; A; C; A; C; T; C; C; A; T; G; G; A; A; G; T; A; A; T; A; C; C; A; A; A; T; G; A; T; C; T; G; C; C; G; C; A; A; T; G; C; T; C; T; G; A; G; C; C; C; T; A; G; G; G; T; T; C; A; T; T; T; T; T; C; T; C; T; T; C; A; C; T; G; T; A; G; G; C; G; G; C; C; T; A; A; C; C; G; G; C; A; T; C; G; T; A; C; T; A; G; C; A; A; A; C; T; C; G; T; C; A; T; T; A; G; A; T; A; T; C; G; T; G; C; T; G; C; A; C; G; A; C; A; C; A; T; A; T; T; A; C; G; T; C; G; T; A; G; C; T; C; A; C; T; T; C; C; A; C; T; A; T; G; T; C; C; T; A; T; C; T; A; T; A; G; G; A; G; C; T; G; T; G; T; T; C; G; C; C; A; T; C; A; T; A; G; G; G; G; G; C; T; T; T; A; T; T; C; A; C; T; G; A; T; T; T; C; C; C; C; T; A; T; T; C; T; C; A; G; G; C; T; A; C; A; C; T; C; T; A; G; A; T; C; A; A; A; C; C; T; A; C; G; C; C; A; A; A; A; T; C; C; A; C; T; T; T; G; C; C; A; T; C; A; T; A; T; T; C; A; T; T; G; G; C; G; T; T; A; A; T; C; T; A; A; C; C; T; T; C; T; T; C; C; C; A; C; A; A; C; A; C; T; T; T; C; T; T; G; G; C; C; T; A; T; C; T; G; G; A; A; T; A; C; C; C; C; G; A; C; A; T; T; A; C; T; C; G; G; A; C; T; A; C; C; C; C; G; A; T; G; C; A; T; A; T; A; C; T; A; C; A; T; G; A; A; A; T; A; T; C; C; T; G; T; C; A; T; C; C; G; T; G; G; G; C; T; C; A; T; T; C; A; T; T; T; C; C; C; T; A; A; C; A; G; C; A; G; T; A; A; T; A; T; T; A; A; T; A; A; T; T; T; T; T; A; T; A; A; T; C; T; G; A; G; A; A; G; C; C; T; T; C; G; C; C; T; C; A; A; A; A; C; G; A; A; A; A; G; T; C; C; T; A; A; T; A; A; T; C; G; A; A; G; A; A; C; C; C; T; C; C; A; C; A; A; A; T; C; T; G; G; A; G; T; G; A; C; T; G; T; A; T; G; G; A; T; G; C; C; C; T; C; C; A; C; C; C; T; A; T; C; A; T; A; C; A; T; T; T; G; A; A; G; A; G; T; C; T; G; T; A; T; A]
 
let chimpanzee: helix = [T; A; T; A; A; A; A; C; C; T; C; C; T; G; C; C; A; T; G; A; C; C; C; A; A; T; A; C; C; A; A; A; C; A; C; C; C; C; T; C; T; T; C; G; T; C; T; G; A; T; C; C; G; T; C; C; T; A; A; T; C; A; C; A; G; C; A; G; T; C; T; T; A; C; T; T; C; T; C; C; T; A; T; C; C; C; T; C; C; C; A; G; T; C; C; T; A; G; C; T; G; C; T; G; G; C; A; T; C; A; C; C; A; T; A; C; T; A; T; T; G; A; C; A; G; A; T; C; G; T; A; A; C; C; T; C; A; A; C; A; C; T; A; C; C; T; T; C; T; T; C; G; A; C; C; C; A; G; C; C; G; G; G; G; G; A; G; G; A; G; A; C; C; C; T; A; T; T; C; T; A; T; A; T; C; A; A; C; A; C; T; T; A; T; T; C; T; G; A; T; T; T; T; T; T; G; G; C; C; A; C; C; C; C; G; A; A; G; T; T; T; A; T; A; T; T; C; T; T; A; T; C; C; T; A; C; C; A; G; G; C; T; T; C; G; G; A; A; T; A; A; T; T; T; C; C; C; A; C; A; T; T; G; T; A; A; C; T; T; A; T; T; A; C; T; C; C; G; G; A; A; A; A; A; A; A; G; A; A; C; C; A; T; T; T; G; G; A; T; A; T; A; T; A; G; G; C; A; T; G; G; T; T; T; G; A; G; C; T; A; T; A; A; T; A; T; C; A; A; T; T; G; G; C; T; T; C; C; T; A; G; G; G; T; T; T; A; T; C; G; T; G; T; G; A; G; C; A; C; A; C; C; A; T; A; T; A; T; T; T; A; C; A; G; T; A; G; G; G; A; T; A; G; A; C; G; T; A; G; A; C; A; C; C; C; G; A; G; C; C; T; A; T; T; T; C; A; C; C; T; C; C; G; C; T; A; C; C; A; T; A; A; T; C; A; T; T; G; C; T; A; T; T; C; C; T; A; C; C; G; G; C; G; T; C; A; A; A; G; T; A; T; T; C; A; G; C; T; G; A; C; T; C; G; C; T; A; C; A; C; T; T; C; A; C; G; G; A; A; G; C; A; A; T; A; T; G; A; A; A; T; G; A; T; C; T; G; C; C; G; C; A; G; T; A; C; T; C; T; G; A; G; C; C; C; T; A; G; G; G; T; T; T; A; T; C; T; T; T; C; T; C; T; T; C; A; C; C; G; T; A; G; G; T; G; G; C; C; T; A; A; C; C; G; G; C; A; T; T; G; T; A; C; T; A; G; C; A; A; A; C; T; C; A; T; C; A; T; T; A; G; A; C; A; T; C; G; T; G; C; T; A; C; A; C; G; A; C; A; C; A; T; A; C; T; A; C; G; T; C; G; T; A; G; C; C; C; A; C; T; T; C; C; A; C; T; A; C; G; T; T; C; T; A; T; C; A; A; T; A; G; G; A; G; C; T; G; T; A; T; T; C; G; C; C; A; T; C; A; T; A; G; G; A; G; G; C; T; T; C; A; T; T; C; A; C; T; G; A; T; T; C; C; C; C; C; T; A; T; T; C; T; C; A; G; G; C; T; A; T; A; C; C; C; T; A; G; A; C; C; A; A; A; C; C; T; A; T; G; C; C; A; A; A; A; T; C; C; A; A; T; T; T; G; C; C; A; T; C; A; T; G; T; T; C; A; T; T; G; G; C; G; T; A; A; A; C; C; T; A; A; C; C; T; T; C; T; T; C; C; C; A; C; A; G; C; A; C; T; T; C; C; T; T; G; G; C; C; T; A; T; C; T; G; G; G; A; T; G; C; C; C; C; G; A; C; G; T; T; A; C; T; C; G; G; A; C; T; A; C; C; C; C; G; A; T; G; C; A; T; A; C; A; C; C; A; C; A; T; G; A; A; A; T; G; T; C; C; T; A; T; C; A; T; C; C; G; T; A; G; G; C; T; C; A; T; T; T; A; T; C; T; C; C; C; T; G; A; C; A; G; C; A; G; T; A; A; T; A; T; T; A; A; T; A; A; T; T; T; T; C; A; T; G; A; T; T; T; G; A; G; A; A; G; C; C; T; T; T; G; C; T; T; C; A; A; A; A; C; G; A; A; A; A; G; T; C; C; T; A; A; T; A; G; T; A; G; A; A; G; A; G; C; C; C; T; C; C; G; C; A; A; A; C; C; T; G; G; A; A; T; G; A; C; T; A; T; A; T; G; G; A; T; G; C; C; C; C; C; C; A; C; C; C; T; A; C; C; A; C; A; C; A; T; T; C; G; A; A; G; A; A; C; C; C; G; T; A; T; A]
 
let human: helix = [T; A; T; A; A; A; A; C; C; C; C; C; T; G; C; C; A; T; A; A; C; C; C; A; A; T; A; C; C; A; A; A; C; G; C; C; C; C; T; T; T; T; C; G; T; C; T; G; A; T; C; C; G; T; C; C; T; A; A; T; C; A; C; A; G; C; A; G; T; C; C; T; A; C; T; T; C; T; C; C; T; A; T; C; T; C; T; C; C; C; A; G; T; C; C; T; A; G; C; T; G; C; T; G; G; C; A; T; C; A; C; T; A; T; A; C; T; A; C; T; A; A; C; A; G; A; C; C; G; C; A; A; C; C; T; C; A; A; C; A; C; C; A; C; C; T; T; C; T; T; C; G; A; C; C; C; C; G; C; C; G; G; A; G; G; A; G; G; A; G; A; C; C; C; C; A; T; T; C; T; A; T; A; C; C; A; A; C; A; C; C; T; A; T; T; C; T; G; A; T; T; T; T; T; C; G; G; C; C; A; C; C; C; T; G; A; A; G; T; T; T; A; T; A; T; T; C; T; C; A; T; C; C; T; A; C; C; A; G; G; C; T; T; C; G; G; A; A; T; A; A; T; C; T; C; C; C; A; T; A; T; T; G; T; A; A; C; T; T; A; C; T; A; C; T; C; C; G; G; A; A; A; A; A; A; A; G; A; A; C; C; A; T; T; T; G; G; A; T; A; C; A; T; A; G; G; T; A; T; G; G; T; C; T; G; A; G; C; T; A; T; G; A; T; A; T; C; A; A; T; T; G; G; C; T; T; C; C; T; A; G; G; G; T; T; T; A; T; C; G; T; G; T; G; A; G; C; A; C; A; C; C; A; T; A; T; A; T; T; T; A; C; A; G; T; A; G; G; A; A; T; A; G; A; C; G; T; A; G; A; C; A; C; A; C; G; A; G; C; A; T; A; T; T; T; C; A; C; C; T; C; C; G; C; T; A; C; C; A; T; A; A; T; C; A; T; C; G; C; T; A; T; C; C; C; C; A; C; C; G; G; C; G; T; C; A; A; A; G; T; A; T; T; T; A; G; C; T; G; A; C; T; C; G; C; C; A; C; A; C; T; C; C; A; C; G; G; A; A; G; C; A; A; T; A; T; G; A; A; A; T; G; A; T; C; T; G; C; T; G; C; A; G; T; G; C; T; C; T; G; A; G; C; C; C; T; A; G; G; A; T; T; C; A; T; C; T; T; T; C; T; T; T; T; C; A; C; C; G; T; A; G; G; T; G; G; C; C; T; G; A; C; T; G; G; C; A; T; T; G; T; A; T; T; A; G; C; A; A; A; C; T; C; A; T; C; A; C; T; A; G; A; C; A; T; C; G; T; A; C; T; A; C; A; C; G; A; C; A; C; G; T; A; C; T; A; C; G; T; T; G; T; A; G; C; T; C; A; C; T; T; C; C; A; C; T; A; T; G; T; C; C; T; A; T; C; A; A; T; A; G; G; A; G; C; T; G; T; A; T; T; T; G; C; C; A; T; C; A; T; A; G; G; A; G; G; C; T; T; C; A; T; T; C; A; C; T; G; A; T; T; T; C; C; C; C; T; A; T; T; C; T; C; C; G; G; C; T; A; C; A; C; C; C; T; A; G; A; C; C; A; A; A; C; T; T; A; C; G; C; C; A; A; A; A; T; C; C; A; T; T; T; C; A; C; T; A; T; C; A; T; A; T; T; C; A; T; C; G; G; C; G; T; A; A; A; T; C; T; A; A; C; T; T; T; C; T; T; C; C; C; A; C; A; A; C; A; C; T; T; T; C; T; C; G; G; C; C; T; A; T; C; C; G; G; A; A; T; G; C; C; C; C; G; A; C; G; T; T; A; C; T; C; G; G; A; C; T; A; C; C; C; C; G; A; T; G; C; A; T; A; C; A; C; C; A; C; A; T; G; A; A; A; T; A; T; C; C; T; A; T; C; A; T; C; T; G; T; A; G; G; C; T; C; A; T; T; C; A; T; T; T; C; T; C; T; A; A; C; A; G; C; A; G; T; A; A; T; A; T; T; A; A; T; A; A; T; T; T; T; C; A; T; G; A; T; T; T; G; A; G; A; A; G; C; C; T; T; C; G; C; T; T; C; G; A; A; G; C; G; A; A; A; A; G; T; C; C; T; A; A; T; A; G; T; A; G; A; A; G; A; A; C; C; C; T; C; C; A; T; A; A; A; C; C; T; G; G; A; G; T; G; A; C; T; A; T; A; T; G; G; A; T; G; C; C; C; C; C; C; A; C; C; C; T; A; C; C; A; C; A; C; A; T; T; C; G; A; A; G; A; G; C; C; C; G; T; A; T; A]
 
(* -------------------------------------------------------------------------- *)
(* ----- Problem 2: Compute the Hamming Distance between Two Helices -------- *)
(* -------------------------------------------------------------------------- *)
(* Given two equal-length helices, compute the number of nucleotide replacements
   needed to convert the first helix into the second. This is equal to how many
   DNA mutations are needed for one helix to evolve and become the other. *)

let equalNucleotide (n1 : nucleotide) (n2 : nucleotide) : int = 
	if n1  = n2 then 0
	else 1;;

equalNucleotide G G;;

let rec hamming_distance (x1: helix) (x2: helix): int =
  	match x1 with 
  	| [] -> 0
  	| h::t -> match x2 with
  		| [] -> 0
  		| h1::t1 -> (equalNucleotide h h1) + (hamming_distance t t1);;

 hamming_distance human orangutan;;


(* -------------------------------------------------------------------------- *)
(* ----- Problem 3: Decode the Helix to Get an Acid Chain ------------------- *)
(* -------------------------------------------------------------------------- *)
 
(*
  First, some background information and provided functions:
 
  DNA describes how to build organisms by encoding instructions for making amino
  acid chains, which enable basic cellular functions in organisms. Every
  nucleotide triplet encodes one of 20 acids, or indicates the end of the chain.
*)
 
type acid =
    Ala | Arg | Asn | Asp | Cys | Glu | Gln | Gly | His | Ile | Leu | Lys | Met
  | Phe | Pro | Ser | Thr | Trp | Tyr | Val | END
 ;;
(* Given a nucleotide triplet, output the decoded acid or END if the triplet
   signifies that the acid chain should come to an end. *)
let rec acid_of_triplet (n1: nucleotide) (n2: nucleotide) (n3: nucleotide): acid =
  begin match (n1, n2, n3) with
  | (A, A, A) -> Phe | (A, A, G) -> Phe | (A, A, T) -> Leu | (A, A, C) -> Leu
  | (G, A, A) -> Leu | (G, A, G) -> Leu | (G, A, T) -> Leu | (G, A, C) -> Leu
  | (T, A, A) -> Ile | (T, A, G) -> Ile | (T, A, T) -> Ile | (T, A, C) -> Met
  | (C, A, A) -> Val | (C, A, G) -> Val | (C, A, T) -> Val | (C, A, C) -> Val
  | (A, G, A) -> Ser | (A, G, G) -> Ser | (A, G, T) -> Ser | (A, G, C) -> Ser
  | (G, G, A) -> Pro | (G, G, G) -> Pro | (G, G, T) -> Pro | (G, G, C) -> Pro
  | (T, G, A) -> Thr | (T, G, G) -> Thr | (T, G, T) -> Thr | (T, G, C) -> Thr
  | (C, G, A) -> Ala | (C, G, G) -> Ala | (C, G, T) -> Ala | (C, G, C) -> Ala
  | (A, T, A) -> Tyr | (A, T, G) -> Tyr | (A, T, T) -> END | (A, T, C) -> END
  | (G, T, A) -> His | (G, T, G) -> His | (G, T, T) -> Gln | (G, T, C) -> Gln
  | (T, T, A) -> Asn | (T, T, G) -> Asn | (T, T, T) -> Lys | (T, T, C) -> Lys
  | (C, T, A) -> Asp | (C, T, G) -> Asp | (C, T, T) -> Glu | (C, T, C) -> Glu
  | (A, C, A) -> Cys | (A, C, G) -> Cys | (A, C, T) -> END | (A, C, C) -> Trp
  | (G, C, A) -> Arg | (G, C, G) -> Arg | (G, C, T) -> Arg | (G, C, C) -> Arg
  | (T, C, A) -> Ser | (T, C, G) -> Ser | (T, C, T) -> Arg | (T, C, C) -> Arg
  | (C, C, A) -> Gly | (C, C, G) -> Gly | (C, C, T) -> Gly | (C, C, C) -> Gly
  end
 ;;
(*
  DNA is decoded by scanning the helix for the first occurrence of the sequence
  [T; A; C]. This starts the acid chain with Met as the head element. After the
  first occurrence of [T; A; C], every triplet contributes another acid to the
  list. Finally, either the helix runs out of triplets or one of the triplets is
  [A; T; T], [A; C; T], or [A; T; C]. When any of these things happen, the acid
  chain comes to an end.
 
  Assuming that longer chains indicate more complexity, being able to decode the
  acid chain gives us some sense of helix complexity...
*)
 
(* Given a helix, decode its first acid chain. The first chain starts at the
   first occurrence of [T; A; C] and decodes triplets from that point. It stops
   once there are no more triplets to read or once a decoded triplet reads END.
   (The END acid value should not be included in the output chain.) *)


let rec acids_of_helix (x: helix): acid list =
	match x
	 with
	| [] -> []
	| h1::h2::h3::t -> if (acid_of_triplet h1 h2 h3) <> END then (acid_of_triplet h1 h2 h3)::acids_of_helix (t) else []
;;


let rec acids_of_helix_MET (x: helix): acid list =
	match x with
	| [] -> []
	| h1::h2::h3::t -> if (acid_of_triplet h1 h2 h3 = Met) then (Met::acids_of_helix t) else (acids_of_helix_MET (h2::h3):: t)
;;


acids_of_helix_MET human;;

 
(* -------------------------------------------------------------------------- *)
(* ----- Problem 4: Generate All Possible Trees ----------------------------- *)
(* -------------------------------------------------------------------------- *)
 
(*
  An evolutionary tree is a binary tree whose nodes are labeled with helices.
  We provide the code for modeling a tree:
*)
 
type tree = Leaf of helix | Node of tree * helix * tree
 
(* sample tree for testing *)
let sample_tree = Leaf human
let sample_tree2 = Node ( Leaf gorilla, lar_gibbon, Leaf orangutan)
 
(*
  Before we generate all possible trees, let's write a useful helper function:
*)
 
(* Finds the helix of the root node in the tree. *)
let helix_of_tree (r: tree): helix =
  match r with
  | Leaf helix -> helix
  | Node (left, helix, right) -> helix
 
 
(*
  We start the process of generating all possible trees by listing all distinct
  tree structures with the input helices at the leaves. These trees are
  "unlabeled," meaning that every internal node will contain [] as its helix.
 
  Since internal nodes correspond to ancestor species, we will (later) guess the
  internal labels before evaluating functions that compute tree complexity.
 
  We provide the code for generating all possible unlabeled trees...
*)
 
(* Given four helices that should appear on the leaves of each output tree,
   list all distinct tree structures with the input helices at their leaves.
   Every internal node should have [] as its helix. *)
let rec all_unlabeled_trees
    (x1: helix) (x2: helix) (x3: helix) (x4: helix): (tree list) =
  let tree1 (x1: helix) (x2: helix) (x3: helix) (x4: helix): tree =
    Node (Node (Leaf x1, [], Leaf x2),  [], Node (Leaf x3, [], Leaf x4)) in
  let tree2  (x1: helix) (x2: helix) (x3: helix) (x4: helix): tree =
    Node (Leaf x1, [], Node (Leaf x2, [], Node (Leaf x3, [], Leaf x4))) in
  tree1 x1 x2 x3 x4 :: tree1 x1 x3 x2 x4 :: tree1 x1 x4 x2 x3 ::
  tree2 x1 x2 x3 x4 :: tree2 x1 x3 x2 x4 :: tree2 x1 x4 x2 x3 ::
  tree2 x2 x1 x3 x4 :: tree2 x2 x3 x1 x4 :: tree2 x2 x4 x1 x3 ::
  tree2 x3 x1 x2 x4 :: tree2 x3 x2 x1 x4 :: tree2 x3 x4 x1 x2 ::
  tree2 x4 x1 x2 x3 :: tree2 x4 x2 x1 x3 :: tree2 x4 x3 x1 x2 :: []
 
(*
  The unlabeled trees generated above use [] as helices for the internal nodes.
  The next step in generating all possible trees is to label the internal nodes.
  See Figure C for a completely labeled candidate tree whose helices are of
  length four. Figure C is sample output for labeled_tree, which you'll write
  below.
 
  Before you write labeled_tree, read through this helper function...
*)
 
(* Assuming x1 and x2 are valid helices, guess the valid helix that labels their
   parent node in an evolutionary tree. The guessed helix is computed using
   nucleotide-by-nucleotide comparison of x1 and x2:
   If the nucleotides match, the corresponding parent nucleotide is the same.
   If they don't match, the corresponding parent nucleotide is arbitrarily A. *)
let rec guess_parent_helix (x1: helix) (x2: helix): helix =
  begin match x1, x2 with
  | h1 :: t1, h2 :: t2 ->
      (if h1 = h2 then h1 else A) :: guess_parent_helix t1 t2
  | [], [] -> []
  | _ -> failwith "invalid input: x1 and x2 have unequal lengths"
  end 
                                                                 
(* Given an unlabeled tree, replicate its structure but with valid helices at
   internal nodes. You can pass two valid child helices to guess_parent_helix to
   get the valid helix that should label their parent in the output tree.
  
   In other words, this function should guess the DNA of ancestor species in the
   possible evolutionary tree. *)
let test1: helix = [T;C;G;T;A;G;C;T;A]
let test2: helix = [T;C;A;G;A;G;C;A;T]
let test3: helix = [C;G;A;T;G;C;G;T;A]
let test4: helix = [C;C;A;G;G;A;C;T;A]

let rec labeled_tree (r: tree): tree =
  match r with
  | Node (left, helix, right) -> Node (labeled_tree left, guess_parent_helix(helix_of_tree left) (helix_of_tree right), labeled_tree right)
  | Leaf l -> Leaf l
 

(*
  Now we have a way of generating all possible trees -- i.e., all possible
  hypotheses about possible evolutionary relations between the species at the
  leaves.  What we need is a way of comparing hypotheses to see which is more
  plausible.
 
  In the following problems, we will encode three kinds complexity scores
  for trees.  Then, using a (provided) function for finding the simplest tree,
  we can hypothesize relationships between the apes.

# use "EvolutionPart1.ml";;
*)

(* -------------------------------------------------------------------------- *)
(* ----- Problem 5: Sum of Acid Chain Lengths ------------------------------- *)
(* -------------------------------------------------------------------------- *)
 
(*
  The first score approximates the complexity of the tree by summing the lengths
  of acid chains obtained by decoding the helices of internal nodes.
*)
 
let rec acids_length (acids: acid list): int =
  begin match acids with
  | [] -> 0
  | h :: t -> 1 + acids_length t
  end
 
(* Given a labeled tree, find all acid chains obtained by decoding the helices
   of internal nodes, and output the sum of their lengths. Don't add the length
   of acid chains from leaves. *)
let rec sum_of_acids_length (r: tree): int =
  match r with
  | Leaf l -> 0
  | Node (left, helix, right) -> (sum_of_acids_length left) + (acids_length (acids_of_helix_MET helix)) + (sum_of_acids_length right)
;;

sum_of_acids_length sample_tree2;;
(* -------------------------------------------------------------------------- *)
(* ----- Problem 6: Parent-Child Hamming ------------------------------------ *)
(* -------------------------------------------------------------------------- *)
 
(*
  The second complexity score approximates the complexity of a tree by
  summing the number of mutations needed to convert between parent and
  child DNA sequences.
*)
 
(* Given a labeled tree, compute the sum of Hamming distances between all
   parent-child pairs. We define the children of an internal node to be  the two
   trees it contains, and we say that leaves have no children. *)
let rec parent_child_hamming (r: tree): int =
  -1 (* STUB *)
 
 
(* -------------------------------------------------------------------------- *)
(* ----- Problem 7: Ancestor-Descendant Hamming ----------------------------- *)
(* -------------------------------------------------------------------------- *)
 
(*
  The third score approximates the complexity of the tree by summing the number
  of mutations needed to convert any ancestor DNA into any descendant DNA.
 
  This one is a bit challenging, so we've made it worth 0 points - kudos only.
  If you do this problem, be sure to add your own tests.
*)
 
(* Given a labeled tree, compute the sum of Hamming distances between all
   ancestor-descendant pairs. We define the ancestors of a node to be its parent
   as well as all the ancestors of its parent. *)
let rec ancestor_descendant_hamming (r: tree): int =
  -1 (* STUB *)
 
 
(* -------------------------------------------------------------------------- *)
(* ----- Provided Code ------------------------------------------------------ *)
(* -------------------------------------------------------------------------- *)
 
type complexity_score = PCHamming | AcidLength | ADHamming
 
(* Given a complexity score and a list of labeled candidate trees, find the
   least complex tree and its complexity score. If multiple trees are least
   complex, output the first such tree in the list.
  
   Assumes that complexities are always non-negative. *)
(*let rec simplest_tree (score: complexity_score) (rs: tree list) : tree * int =
  begin match rs with
  | [] -> (Leaf [], max_int)
  | h :: t ->
      let complexity =
          begin match score with
          | PCHamming -> parent_child_hamming h
          | AcidLength -> sum_of_acids_length h
          | ADHamming -> ancestor_descendant_hamming h
          end in
      let (h', complexity') = simplest_tree score t in
      if complexity <= complexity' then (h, complexity) else (h', complexity')
  end
 *)
(* Given a helix, output the ape name (e.g. "Orangutan") or "Non-Ape" if the
   helix doesn't match any of the ape helices. *)
let rec string_of_ape (ape: helix): string =
  if ape = lar_gibbon then "Lar Gibbon"
  else if ape = pileated_gibbon then "Pileated Gibbon"
  else if ape = siamang then "Siamang"
  else if ape = white_cheeked_gibbon then "White Cheeked Gibbon"
  else if ape = orangutan then "Orangutan"
  else if ape = gorilla then "Gorilla"
  else if ape = chimpanzee then "Chimpanzee"
  else if ape = human then "Human"
  else "Non-Ape"
 
(* Given a tree whose leaves are labeled with the ape helices, output a string
   representation of the tree. *)
let rec string_of_ape_tree (r: tree): string =
  let rec with_indent (indent: string) (r: tree): string =
    begin match r with
    | Leaf x ->
          indent ^ " -> " ^ (string_of_ape x) ^ "\n"
    | Node (r1, _, r2) ->
          let s1 = with_indent (indent ^ "    ") r1 in
          let s2 = with_indent (indent ^ "    ") r2 in
        indent ^ " -> " ^ "Split!" ^ "\n" ^ s1 ^ s2
    end in
  with_indent "" r
 
(* Given a choice of the complexity score to use and four ape helices to use as
   leaves in the tree, print the simplest ape tree and its complexity. *)
(*let rec print_simplest_ape_tree
    (score: complexity_score)
    (x1: helix)  (x2: helix)  (x3: helix)  (x4: helix): unit =
  let rec labeled_trees (rs: tree list): tree list =
    begin match rs with
    | [] -> []
    | h :: t -> labeled_tree h :: labeled_trees t
    end in
  let rs' = labeled_trees (all_unlabeled_trees x1 x2 x3 x4) in
  let (r, c) = simplest_tree score rs' in
  print_string ("complexity = " ^ (string_of_int c) ^ "\n");
  print_string (string_of_ape_tree r)
 *)
(* -------------------------------------------------------------------------- *)
(* ----- Problem 8: Questions ----------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(*
  Now it's time to use the code! Using the toplevel, code against the functions
  in this file to answer the following questions. Make sure your code is well
  tested before you begin this part.
*)
 
(*let question_complementary_dna_strand_of_ctaatgt: helix =
  (* What is the complementary DNA strand of CTAATGT? *)

 
let question_mutations_between_human_and_gorilla: int =
  (* How many mutations must happen to convert the human DNA sample into the
     gorilla DNA sample? *)

 
let question_mutations_between_human_and_chimp: int =
  (* How many mutations must happen to convert the human DNA sample into the
     chimpanzee DNA sample? *)

 
let question_most_like_human: helix list =
  (* Basing your answer on the number of mutations it takes to convert between
     the DNA samples, order the seven non-human ape helices by their similarity
     to humans, with the most human-like helix appearing first. *)

 
let question_most_complex_ape: helix list =
  (* Basing your answer on decoded acid chains, order the following ape helices
     by their complexity, with the most complex helix appearing first:
     - lar gibbon
     - orangutan
     - pileated gibbon
     - siamang *)

 
let question_better_complexity_score: complexity_score =
  (* Basing your answer on the correct evolutionary tree in Figure A and on the
     outputs of print_simplest_ape_tree for humans, orangutans, white-cheeked
     gibbons, and pileated gibbons, determine whether parent-child Hamming
     distance or acid length is a better complexity score. *)

 
let question_complexity_of_simplest_lesser_ape_tree: int =
  (* What is the complexity of the simplest lesser ape tree as measured by the
     AcidLength complexity score? The lesser apes are siamangs, lar gibbons,
     white-cheeked gibbons, and pileated gibbons.
    
     Though this question doesn't test it, you should also check how closely the
     simplest tree matches the tree in Figure A. *)

 
let question_complexity_of_simplest_greater_ape_tree: int =
  (* What is the complexity of the simplest greater ape tree as measured by the
     PCHamming complexity score? The greater apes are humans, orangutans,
     gorillas, and chimpanzees.
    
     Though this question doesn't test it, you should also check how closely the
     simplest tree matches the tree in Figure A. *)

*)
