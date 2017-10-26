(* -------------------------------------------------------------------------- *)
(* ----- Problem 4: Generate All Possible Trees ----------------------------- *)
(* -------------------------------------------------------------------------- *)

(*
  An evolutionary tree is a binary tree whose nodes are labeled with helices.
  We provide the code for modeling a tree:
*)

type nucleotide = G | C | A | T;;
type helix = nucleotide list;;
type tree = Leaf of helix | Node of tree * helix * tree

let lar_gibbon: helix = [T; A; T; A; A; A; G; A; G; A; G; T; A; A; A; A; A; G; T; G; T; A; A; A; C; C; C; C; A; T; A; G; T; T; G; G; C; C; T; A; A; A; A; G; C; A; G; C; C; A; C; C; A; A; T; T; A; A; G; A; A; A; G; C; G; T; T; C; A; A; G; C; T; C; A; A; C; A; C; C; A; C; C; T; A; T; C; C; A; A; C; A; A; A; T; C; C; C; A; A; A; C; A; C; A; C; A; A; C; T; G; A; A; C; T; C; C; T; T; C; C; A; C; C; A; C; A; T; T; G; G; A; C; C; A; A; T; C; T; A; T; C; A; T; T; T; T; A; T; A; G; A; A; G; A; A; A; T; A; A; T; G; T; T; A; G; T; A; T; A; A; G; T; A; A; C; A; T; G; A; A; T; A; A; C; A; T; T; C; T; C; C; C; C; C; G; C; A; T; A; A; A; C; C; T; A; T; A; T; C; A; G; A; C; C; A; A; A; A; A; A; C; T; T; C; G; C; T; G; A; C; A; G; T; T; A; A; C; A; G; C; C; C; A; A; T; A; T; C; T; A; A; A; A; C; C; A; A; C; T; G; A; T; A; A; A; C; C; A; T; T; A; T; T; G; C; C; C; A; C; A; C; T; G; T; C; A; A; C; C; C; A; A; C; A; T; A; G; G; C; A; T; G; C; C; C; A; C; A; A; G; G; A; A; A; G; G; T; T; A; A; A; A; A; A; A; G; T; A; A; A; A; G; G; A; A; C; T; C; G; G; C; A; A; A; C; A; C; T; A; C; C; C; C; G; C; C; T; G; T; T; T; A; C; C; A; A; A; A; A; C; A; T; C; A; C; C; T; C; T; A; G; C; A; T; T; A; C; C; A; G; T; A; T; T; A; G; A; G; G; C; A; C; C; G; C; C; T; G; C; C; C; A; G; T; G; A; C; A; C; A; T; G; T; T; C; A; A; C; G; G; C; C; G; C; G; G; T; A; C; C; C; T; A; A; C; C; G; T; G; C; A; A; A; G; G; T; A; G; C; A; T; A; A; T; C; A; C; T; T; G; T; T; C; C; T; T; A; A; A; T; G; G; G; G; A; C; T; T; G; T; A; T; G; A; A; T; G; G; C; T; C; C; A; C; G; A; G; G; G; T; T; C; A; G; C; T; G; T; C; T; C; T; T; A; C; T; T; T; C; A; A; C; C; A; G; T; G; A; A; A; T; T; G; A; C; C; T; G; T; C; C; G; T; G; A; A; G; A; G; G; C; G; G; A; C; A; T; A; A; C; C; T; A; A; C; A; A; G; A; C; G; A; G; A; A; G; A; C; C; C; T; A; T; G; G; A; G; C; T; T; T; A; G; T; C; T; A; T; C; A; A; T; G; C; A; A; A; C; A; A; C; A; T; T; C; A; A; T; A; A; A; C; C; A; A; C; A; G; G; T; C; A; T; A; A; A; T; T; A; C; C; A; A; A; C; C; T; G; C; A; T; C; G; A; A; G; A; C; T; T; C; G; G; T; T; G; G; G; G; C; G; A; C; C; T; C; G; G; A; G; C; A; T; A; G; A; C; T; A; A; C; C; T; C; C; G; A; G; C; A; G; T; A; T; A; T; G; C; T; A; A; G; A; C; C; A; C; A; C; C; A; G; T; C; A; A; A; A; C; G; A; A; A; C; T; C; C; A; T; G; T; G; C; A; A; T; T; G; A; C; C; C; A; A; T; A; A; C; T; T; G; A; T; C; A; A; C; G; G; A; A; C; A; A; G; T; T; A; C; C; C; T; A; G; G; G; A; T; A; A; C; A; G; C; G; C; A; A; T; C; C; T; A; T; T; C; T; A; G; A; G; T; C; C; A; T; A; T; C; A; A; C; A; A; T; A; G; G; G; T; T; T; A; C; G; A; C; C; T; C; G; A; T; G; T; T; G; G; A; T; C; A; G; G; A; C; A; T; C; C; C; G; A; T; G; G; T; G; C; A; G; C; C; G; C; T; A; T; C; A; A; A; G; G; T; T; C; G; T; T; T; G; T; T; C; A; A; C; G; A; T; T; A; A; A; G; T; C; C; T; A; C; G; T; G; A; T; C; T; G; A; G; T; T; C; A; G; A; C; C; G; G; A; G; T; A; A; T; C; C; A; G; G; T; C; G; G; T; T; T; C; T; A; T; C; T; G; T; T; C; T; A; T; A; T; T; T; C; T; C; C; C; T; G; T; A; C; G; A; A; A; G; G; A; C; A; A; G; A; G; A; A; A; T; A; G; G; G; C; C; C; A; C; T; T; C; G; C; A];;

let pileated_gibbon: helix = [T; A; T; A; A; A; G; A; G; A; G; T; A; A; A; A; A; G; T; G; T; A; A; A; C; C; C; C; A; T; A; G; T; T; G; G; C; C; T; A; A; A; A; G; C; A; G; C; C; A; C; C; A; A; T; T; A; A; G; A; A; A; G; C; G; T; T; C; A; A; G; C; T; C; A; A; C; A; C; C; A; C; C; C; A; C; C; C; A; A; T; A; A; A; T; C; C; C; A; A; A; C; A; T; A; T; A; A; C; T; G; A; A; C; T; C; C; T; T; C; C; A; C; C; A; C; A; T; T; G; G; A; C; C; A; A; T; C; T; A; T; C; A; T; T; C; T; A; T; A; G; A; A; G; A; A; A; T; A; A; T; G; T; T; A; A; T; A; T; G; A; G; T; A; A; C; A; C; G; A; A; A; A; G; A; A; T; T; C; T; C; C; T; C; C; G; C; A; T; A; A; G; C; C; T; A; T; A; T; C; A; G; A; C; C; A; A; A; A; A; G; A; C; T; T; C; G; C; T; G; A; C; A; G; T; T; A; A; C; A; G; C; T; C; A; A; T; A; T; C; T; A; A; A; A; C; C; A; A; C; T; G; A; T; A; G; A; C; C; A; T; T; A; T; T; A; C; C; C; A; C; A; C; T; G; T; C; A; A; C; C; C; A; A; C; A; T; A; G; G; C; A; T; G; C; C; C; A; C; A; A; G; G; A; A; A; G; G; T; T; A; A; A; A; A; A; A; G; T; A; A; A; A; G; G; A; A; C; T; C; G; G; C; A; A; A; C; A; C; T; A; C; C; C; C; G; C; C; T; G; T; T; T; A; C; C; A; A; A; A; A; C; A; T; C; A; C; C; T; C; T; A; G; C; A; T; T; A; C; C; A; G; T; A; T; T; A; G; A; G; G; C; A; C; C; G; C; C; T; G; C; C; C; A; G; T; G; A; C; A; C; A; T; G; T; T; C; A; A; C; G; G; C; C; G; C; G; G; T; A; C; C; C; T; A; A; C; C; G; T; G; C; A; A; A; G; G; T; A; G; C; A; T; A; A; T; C; A; C; T; T; G; T; T; C; C; T; T; A; A; A; T; G; G; G; G; A; C; T; T; G; T; A; T; G; A; A; T; G; G; C; T; C; C; A; C; G; A; G; G; G; T; C; C; A; G; C; T; G; T; C; T; C; T; T; A; C; T; T; T; C; A; A; C; C; A; G; T; G; A; A; A; T; T; G; A; C; T; T; G; T; C; C; G; T; G; A; A; G; A; G; G; C; G; G; A; C; A; T; A; G; C; C; T; A; A; C; A; A; G; A; C; G; A; G; A; A; G; A; C; C; C; T; A; T; G; G; A; G; C; T; T; T; A; G; C; C; T; A; T; C; A; A; T; G; C; A; A; A; C; A; A; T; A; T; T; C; A; A; C; A; A; A; C; C; A; A; C; A; G; G; C; C; G; T; A; A; A; C; T; A; C; C; A; A; A; T; C; T; G; C; A; T; C; G; A; A; G; A; C; T; T; C; G; G; T; T; G; G; G; G; C; G; A; C; C; T; C; G; G; A; G; C; A; T; A; A; A; C; T; A; A; C; C; T; C; C; G; A; G; C; A; G; T; A; C; A; T; G; C; T; A; A; G; A; C; C; A; C; A; C; C; A; G; T; C; A; A; A; A; C; A; A; A; A; T; T; C; C; A; T; A; T; G; C; A; A; T; T; G; A; C; C; C; A; A; T; A; A; T; T; T; G; A; T; C; A; A; C; G; G; A; A; C; A; A; G; T; T; A; C; C; C; T; A; G; G; G; A; T; A; A; C; A; G; C; G; C; A; A; T; C; C; T; A; T; T; C; T; A; G; A; G; T; C; C; A; T; A; T; C; A; A; C; A; A; T; A; G; G; G; T; T; T; A; C; G; A; C; C; T; C; G; A; T; G; T; T; G; G; A; T; C; A; G; G; A; C; A; T; C; C; C; G; A; T; G; G; T; G; C; A; G; C; C; G; C; T; A; T; C; A; A; A; G; G; T; T; C; G; T; T; T; G; T; T; C; A; A; C; G; A; T; T; A; A; A; G; T; C; C; T; A; C; G; T; G; A; T; C; T; G; A; G; T; T; C; A; G; A; C; C; G; G; A; G; T; A; A; T; C; C; A; G; G; T; C; G; G; T; T; T; C; T; A; T; C; T; G; T; T; C; T; A; T; A; T; T; T; C; T; C; C; C; T; G; T; A; C; G; A; A; A; G; G; A; C; A; A; G; A; G; A; A; A; T; G; G; G; G; C; C; C; A; C; T; T; C; A; C];;

let siamang: helix = [T; A; T; A; A; A; A; C; C; T; A; G; A; C; A; A; A; A; A; A; G; G; A; A; G; G; A; A; T; C; G; A; A; C; C; C; T; C; T; A; A; A; A; C; C; G; G; T; T; T; C; A; A; G; C; C; A; G; C; C; C; C; A; T; A; A; C; C; T; T; T; A; T; G; A; C; T; T; T; T; T; C; A; A; A; A; A; G; A; T; A; T; T; A; G; A; A; A; A; A; C; T; A; T; T; T; C; A; T; A; A; C; T; T; T; G; T; C; A; A; A; G; T; T; A; A; A; T; C; A; C; A; G; G; T; T; C; A; A; A; C; C; C; C; G; T; A; T; A; T; C; T; T; A; A; T; G; G; C; A; C; A; T; G; C; A; G; C; T; C; A; A; G; T; A; G; G; C; C; T; A; C; A; A; G; A; C; G; C; T; A; C; A; T; C; C; C; C; T; A; T; C; A; T; A; G; A; A; G; A; A; C; T; A; A; T; C; T; C; T; T; T; C; C; A; C; G; A; C; C; A; T; G; C; C; C; T; C; A; T; A; A; T; C; A; T; T; T; T; C; C; T; T; A; T; C; A; G; C; T; T; C; C; T; A; G; T; T; C; T; A; T; A; T; G; C; C; C; T; C; T; T; C; C; T; A; A; C; A; C; T; C; A; C; A; A; C; A; A; A; A; C; T; A; A; C; C; A; A; C; A; C; T; A; A; T; A; T; T; A; C; G; G; A; C; G; C; C; C; A; A; G; A; G; A; T; A; G; A; A; A; C; C; G; T; C; T; G; A; A; C; A; A; T; C; C; T; A; C; C; T; G; C; T; A; T; T; A; T; T; C; T; A; G; T; T; C; T; A; A; T; C; G; C; C; C; T; C; C; C; A; T; C; C; C; T; C; C; G; C; A; T; C; C; T; T; T; A; T; T; T; G; A; C; A; G; A; C; G; A; A; A; T; C; A; A; C; G; A; C; C; C; C; T; C; C; T; T; T; A; C; C; A; T; C; A; A; G; G; C; A; A; T; C; G; G; T; C; A; T; C; A; G; T; G; A; T; A; C; T; G; A; G; C; C; T; A; C; G; A; A; T; A; T; A; C; A; G; A; C; T; A; C; G; G; T; G; G; G; C; T; A; A; T; C; T; T; C; A; A; T; T; C; T; T; A; C; A; T; A; T; T; A; C; C; A; C; C; A; T; T; A; T; T; T; C; T; A; G; A; A; C; C; A; G; G; G; G; A; C; C; T; T; C; G; A; C; T; C; C; T; T; G; A; A; G; T; C; G; A; C; A; A; C; C; G; A; G; T; A; G; T; T; C; T; T; C; C; A; A; T; T; G; A; A; G; C; C; C; C; T; G; T; C; C; G; T; A; T; A; A; T; A; A; T; T; A; C; A; T; C; A; C; A; A; G; A; C; G; T; C; C; T; A; C; A; C; T; C; A; T; G; A; A; C; T; G; T; C; C; C; C; T; C; C; C; T; A; G; G; T; C; T; G; A; A; A; A; C; G; G; A; T; G; C; T; A; T; C; C; C; C; G; G; A; C; G; C; C; T; A; A; A; C; C; A; A; A; C; C; A; C; A; T; T; C; A; C; C; G; C; C; A; C; A; C; G; C; C; C; A; G; G; A; G; T; A; T; A; C; T; A; C; G; G; C; C; A; A; T; G; C; T; C; A; G; A; A; A; T; C; T; G; T; G; G; A; G; C; C; A; A; C; C; A; T; A; G; C; T; T; T; A; T; A; C; C; G; A; T; T; G; T; T; C; T; A; G; A; A; C; T; A; A; T; T; C; C; C; T; T; A; A; A; A; A; T; C; T; T; C; G; A; A; A; T; A; G; G; G; C; C; T; G; T; G; T; T; T; A; C; C; C; T; A; T; A; A; C; C; C; C; A; C; C; C; T; C; T; G; C; C; C; C; C; C; G; T; A; A; A; T; C; T; C; A; C; T; G; T; A; G; A; G; C; T; A; G; A; C; C; A; G; C; A; T; T; A; A; C; C; T; T; T; T; A; A; G; T; T; A; A; A; G; A; C; T; A; A; G; A; G; A; A; C; T; A; C; C; A; C; C; T; C; T; T; T; A; C; A; G; T; G; A; A; A; T; G; C; C; C; C; A; A; T; T; A; A; A; C; A; C; C; A; C; C; G; T; G; T; G; A; C; C; T; A; T; A; A; T; C; A; T; C; A; C; A; T; C; A; A; T; A; C; T; T; C; T; C; A; C; A; C; T; A; T; T; C; C; T; C; C; T; C; A; T; A; C; A; A; C; T; A; A; A; A; A; C; A; C];;

let white_cheeked_gibbon: helix = [T; A; T; A; A; A; A; C; C; T; A; G; A; C; A; A; A; A; A; A; G; G; A; A; G; G; A; A; T; C; G; A; A; C; C; C; C; C; T; A; A; A; A; C; T; G; G; T; T; T; C; A; A; G; C; C; A; G; C; C; C; C; A; T; A; A; C; C; T; C; T; A; T; G; A; C; T; T; T; T; T; C; A; A; A; A; G; G; T; A; T; T; A; G; A; A; A; A; G; C; T; A; T; T; T; C; A; T; A; A; C; T; T; T; G; T; C; A; A; A; G; T; T; A; A; A; T; C; A; C; A; G; G; T; T; C; A; A; G; C; C; C; C; G; T; A; T; A; T; C; T; T; A; A; T; G; G; C; A; C; A; T; G; C; A; A; C; T; C; A; A; G; T; A; G; G; C; C; T; A; C; A; A; G; A; C; G; C; T; A; C; A; T; C; C; C; C; C; A; T; C; A; T; A; G; A; A; G; A; A; C; T; A; A; T; C; T; C; T; T; T; C; C; A; C; G; A; C; C; A; C; G; C; C; C; T; C; A; T; A; A; T; C; A; T; T; T; T; C; C; T; T; A; T; C; A; G; C; T; T; C; C; T; G; G; T; C; C; T; A; T; A; T; G; C; C; C; T; C; T; T; C; C; T; A; A; C; A; C; T; C; A; C; A; A; C; A; A; A; A; C; T; A; A; C; C; A; A; C; A; C; T; A; A; C; A; T; T; A; C; A; G; A; C; G; C; C; C; A; A; G; A; G; A; T; A; G; A; A; A; C; C; A; T; C; T; G; A; A; C; A; A; T; C; C; T; A; C; C; T; G; C; C; A; T; T; A; T; C; C; T; A; G; T; C; C; T; A; A; T; C; G; C; T; C; T; C; C; C; A; T; C; C; C; T; C; C; G; T; A; T; T; C; T; C; T; A; T; T; T; A; A; C; A; G; A; C; G; A; A; A; T; C; A; A; C; G; A; C; C; C; T; T; C; C; C; T; C; A; C; C; A; T; C; A; A; A; G; C; A; A; T; C; G; G; C; C; A; T; C; A; A; T; G; A; T; A; C; T; G; A; G; C; C; T; A; C; G; A; A; T; A; T; A; C; A; G; A; C; T; A; C; G; G; C; G; G; A; C; T; G; G; T; C; T; T; C; A; A; T; T; C; C; T; A; C; A; T; G; C; T; C; C; C; A; C; C; A; T; T; A; T; T; T; C; T; A; G; A; A; C; C; C; G; G; A; G; A; C; C; T; T; C; G; A; C; T; C; C; T; T; G; A; A; G; T; C; G; A; C; A; A; T; C; G; A; G; T; A; G; T; T; C; T; C; C; C; A; A; T; T; G; A; A; G; C; C; C; C; T; G; T; T; C; G; T; A; T; A; A; T; A; A; T; T; A; C; A; T; C; A; C; A; A; G; A; C; G; T; C; C; T; A; C; A; C; T; C; A; T; G; A; A; C; T; G; T; C; C; C; C; T; C; C; C; T; C; G; G; C; C; T; A; A; A; A; A; C; A; G; A; C; G; C; C; A; T; C; C; C; T; G; G; A; C; G; C; C; T; A; A; A; C; C; A; A; A; C; C; A; C; A; T; T; T; A; C; C; G; C; C; A; C; A; C; G; C; C; C; A; G; G; A; G; T; A; T; A; T; T; A; C; G; G; C; C; A; A; T; G; C; T; C; A; G; A; A; A; T; C; T; G; C; G; G; A; G; C; C; A; A; C; C; A; T; A; G; C; T; T; T; A; T; A; C; C; A; A; T; C; G; T; C; C; T; A; G; A; G; C; T; A; A; T; T; C; C; C; T; T; A; A; A; A; A; T; C; T; T; C; G; A; A; A; T; A; G; G; G; C; C; C; G; T; A; T; T; C; A; C; C; C; T; A; T; A; A; T; T; A; G; C; C; C; C; T; C; C; C; C; A; C; C; C; T; A; C; G; C; A; A; A; T; T; T; C; A; C; T; G; T; A; G; A; G; C; T; A; G; A; C; T; A; G; C; A; T; T; A; A; C; C; T; T; T; T; A; A; G; T; T; A; A; A; G; A; C; T; A; A; G; A; G; A; A; G; C; A; T; T; A; C; C; T; C; T; T; T; A; C; A; G; T; G; A; A; A; T; G; C; C; C; C; A; A; T; T; A; A; A; C; A; C; C; A; C; C; G; T; A; T; G; A; C; C; C; A; C; A; A; T; T; A; T; C; A; T; G; T; C; A; A; T; A; C; T; T; C; T; C; G; C; G; C; T; A; T; T; C; C; T; C; C; T; A; A; T; C; C; A; A; C; T; A; A; A; A; A; C; A];;

let orangutan: helix = [T; A; T; A; A; A; A; C; C; C; C; C; T; G; C; A; A; T; A; T; C; C; C; A; A; T; A; C; C; A; A; A; C; C; C; C; C; C; T; C; T; T; C; G; T; C; T; G; A; T; C; A; G; T; C; T; T; G; A; T; C; A; C; A; G; C; A; G; T; C; C; T; A; C; T; T; C; T; C; C; T; T; T; C; C; C; T; C; C; C; A; G; T; C; C; T; A; G; C; C; G; C; T; G; G; C; A; T; C; A; C; C; A; T; A; C; T; A; C; T; A; A; C; A; G; A; T; C; G; C; A; A; C; C; T; A; A; A; C; A; C; C; A; C; A; T; T; C; T; T; T; G; A; C; C; C; A; G; C; C; G; G; A; G; G; T; G; G; A; G; A; T; C; C; C; A; T; C; C; T; A; T; A; T; C; A; G; C; A; C; C; T; A; T; T; C; T; G; A; T; T; T; T; T; T; G; G; C; C; A; C; C; C; T; G; A; A; G; T; C; T; A; C; A; T; T; C; T; C; A; T; C; C; T; G; C; C; G; G; G; T; T; T; C; G; G; C; A; T; A; A; T; C; T; C; C; C; A; C; A; T; C; G; T; A; A; C; A; C; A; C; T; A; T; T; C; C; G; G; A; A; A; A; G; A; A; G; A; G; C; C; A; T; T; T; G; G; G; T; A; C; A; T; A; G; G; C; A; T; A; G; T; C; T; G; A; G; C; C; A; T; A; G; T; C; T; C; A; A; T; T; G; G; C; T; T; C; C; T; G; G; G; C; T; T; T; A; T; C; G; T; A; T; G; G; G; C; C; C; A; C; C; A; C; A; T; A; T; T; C; A; C; A; G; T; A; G; G; A; A; T; A; G; A; C; G; T; G; G; A; C; A; C; A; C; G; A; G; C; C; T; A; C; T; T; C; A; C; C; T; C; C; G; C; T; A; C; C; A; T; A; A; T; C; A; T; T; G; C; C; A; T; C; C; C; C; A; C; C; G; G; C; G; T; C; A; A; A; G; T; A; T; T; T; A; G; C; T; G; A; C; T; C; G; C; T; A; C; A; C; T; C; C; A; C; G; G; A; A; G; C; A; A; C; A; C; T; A; A; A; T; G; A; T; C; T; G; C; C; G; C; A; A; T; C; C; T; C; T; G; A; G; C; C; T; T; A; G; G; A; T; T; C; A; T; T; T; T; C; C; T; C; T; T; C; A; C; C; G; T; A; G; G; C; G; G; C; C; T; A; A; C; A; G; G; C; A; T; C; G; T; A; C; T; A; G; C; A; A; A; C; T; C; A; T; C; A; C; T; A; G; A; C; A; T; T; G; T; A; T; T; A; C; A; C; G; A; T; A; C; A; T; A; C; T; A; C; G; T; T; G; T; A; G; C; C; C; A; C; T; T; T; C; A; T; T; A; C; G; T; C; C; T; A; T; C; A; A; T; A; G; G; A; G; C; T; G; T; A; T; T; C; G; C; C; A; T; C; A; T; G; G; G; A; G; G; C; T; T; C; A; T; C; C; A; C; T; G; G; T; T; C; C; C; A; C; T; A; T; T; C; T; C; A; G; G; C; T; A; C; A; C; C; T; T; A; G; A; C; C; A; G; A; C; C; T; A; T; G; C; T; A; A; A; A; T; T; C; A; C; T; T; C; A; T; C; A; C; C; A; T; A; T; T; T; A; T; C; G; G; C; G; T; A; A; A; T; T; T; A; A; C; T; T; T; C; T; T; C; C; C; A; C; A; A; C; A; T; T; T; C; C; T; C; G; G; C; C; T; G; T; C; A; G; G; C; A; T; A; C; C; C; C; G; A; C; G; C; T; A; C; T; C; C; G; A; C; T; A; C; C; C; C; G; A; C; G; C; G; T; A; C; A; C; C; A; C; C; T; G; A; A; A; T; A; T; T; T; T; A; T; C; A; T; C; C; G; C; A; G; G; C; T; C; A; T; T; T; A; T; C; T; C; C; C; T; A; A; C; A; G; C; A; G; T; C; A; T; A; C; T; A; A; T; A; A; T; T; T; T; C; A; T; A; A; T; T; T; G; A; G; A; A; G; C; C; T; T; C; G; C; C; T; C; A; A; A; A; C; G; A; A; A; A; G; T; C; C; C; A; A; T; A; G; T; T; G; A; A; C; A; A; C; C; C; T; C; C; A; C; A; A; G; C; C; T; A; G; A; G; T; G; A; T; T; G; T; A; C; G; G; A; T; G; C; C; C; C; C; C; A; C; C; C; T; A; C; C; A; C; A; C; A; T; T; T; G; A; A; G; A; A; C; C; C; G; T; C; T; A];;

let gorilla: helix = [T; A; T; A; A; A; A; C; C; C; C; C; C; G; C; C; A; T; A; A; C; C; C; A; A; T; A; C; C; A; A; A; C; A; C; C; C; C; T; T; T; T; C; G; T; C; T; G; A; T; C; C; G; T; C; C; T; A; A; T; C; A; C; A; G; C; A; G; T; C; T; T; A; C; T; T; C; T; T; C; T; A; T; C; T; C; T; C; C; C; A; G; T; A; C; T; A; G; C; T; G; C; T; G; G; A; A; T; T; A; C; C; A; T; A; T; T; A; T; T; A; A; C; A; G; A; C; C; G; T; A; A; C; C; T; C; A; A; C; A; C; C; A; C; C; T; T; T; T; T; C; G; A; C; C; C; A; G; C; C; G; G; A; G; G; A; G; G; A; G; A; T; C; C; T; A; T; C; C; T; A; T; A; C; C; A; A; C; A; C; T; T; A; T; T; C; T; G; A; T; T; T; T; T; T; G; G; A; C; A; C; C; C; C; G; A; A; G; T; T; T; A; C; A; T; T; C; T; A; A; T; C; C; T; A; C; C; A; G; G; C; T; T; C; G; G; A; A; T; A; A; T; C; T; C; C; C; A; C; A; T; T; G; T; A; A; C; T; T; A; T; T; A; C; T; C; C; G; G; A; A; A; A; A; A; A; G; A; A; C; C; A; T; T; C; G; G; A; T; A; T; A; T; A; G; G; T; A; T; A; G; T; C; T; G; A; G; C; T; A; T; A; A; T; A; T; C; A; A; T; T; G; G; T; T; T; C; C; T; G; G; G; A; T; T; T; A; T; T; G; T; G; T; G; A; G; C; C; C; A; C; C; A; C; A; T; A; T; T; T; A; C; A; G; T; A; G; G; A; A; T; A; G; A; C; G; T; A; G; A; T; A; C; A; C; G; A; G; C; C; T; A; C; T; T; C; A; C; C; T; C; C; G; C; T; A; C; C; A; T; A; A; T; C; A; T; C; G; C; T; A; T; C; C; C; C; A; C; C; G; G; C; G; T; C; A; A; A; G; T; A; T; T; C; A; G; C; T; G; A; C; T; C; G; C; T; A; C; A; C; T; C; C; A; T; G; G; A; A; G; T; A; A; T; A; C; C; A; A; A; T; G; A; T; C; T; G; C; C; G; C; A; A; T; G; C; T; C; T; G; A; G; C; C; C; T; A; G; G; G; T; T; C; A; T; T; T; T; T; C; T; C; T; T; C; A; C; T; G; T; A; G; G; C; G; G; C; C; T; A; A; C; C; G; G; C; A; T; C; G; T; A; C; T; A; G; C; A; A; A; C; T; C; G; T; C; A; T; T; A; G; A; T; A; T; C; G; T; G; C; T; G; C; A; C; G; A; C; A; C; A; T; A; T; T; A; C; G; T; C; G; T; A; G; C; T; C; A; C; T; T; C; C; A; C; T; A; T; G; T; C; C; T; A; T; C; T; A; T; A; G; G; A; G; C; T; G; T; G; T; T; C; G; C; C; A; T; C; A; T; A; G; G; G; G; G; C; T; T; T; A; T; T; C; A; C; T; G; A; T; T; T; C; C; C; C; T; A; T; T; C; T; C; A; G; G; C; T; A; C; A; C; T; C; T; A; G; A; T; C; A; A; A; C; C; T; A; C; G; C; C; A; A; A; A; T; C; C; A; C; T; T; T; G; C; C; A; T; C; A; T; A; T; T; C; A; T; T; G; G; C; G; T; T; A; A; T; C; T; A; A; C; C; T; T; C; T; T; C; C; C; A; C; A; A; C; A; C; T; T; T; C; T; T; G; G; C; C; T; A; T; C; T; G; G; A; A; T; A; C; C; C; C; G; A; C; A; T; T; A; C; T; C; G; G; A; C; T; A; C; C; C; C; G; A; T; G; C; A; T; A; T; A; C; T; A; C; A; T; G; A; A; A; T; A; T; C; C; T; G; T; C; A; T; C; C; G; T; G; G; G; C; T; C; A; T; T; C; A; T; T; T; C; C; C; T; A; A; C; A; G; C; A; G; T; A; A; T; A; T; T; A; A; T; A; A; T; T; T; T; T; A; T; A; A; T; C; T; G; A; G; A; A; G; C; C; T; T; C; G; C; C; T; C; A; A; A; A; C; G; A; A; A; A; G; T; C; C; T; A; A; T; A; A; T; C; G; A; A; G; A; A; C; C; C; T; C; C; A; C; A; A; A; T; C; T; G; G; A; G; T; G; A; C; T; G; T; A; T; G; G; A; T; G; C; C; C; T; C; C; A; C; C; C; T; A; T; C; A; T; A; C; A; T; T; T; G; A; A; G; A; G; T; C; T; G; T; A; T; A];;

let chimpanzee: helix = [T; A; T; A; A; A; A; C; C; T; C; C; T; G; C; C; A; T; G; A; C; C; C; A; A; T; A; C; C; A; A; A; C; A; C; C; C; C; T; C; T; T; C; G; T; C; T; G; A; T; C; C; G; T; C; C; T; A; A; T; C; A; C; A; G; C; A; G; T; C; T; T; A; C; T; T; C; T; C; C; T; A; T; C; C; C; T; C; C; C; A; G; T; C; C; T; A; G; C; T; G; C; T; G; G; C; A; T; C; A; C; C; A; T; A; C; T; A; T; T; G; A; C; A; G; A; T; C; G; T; A; A; C; C; T; C; A; A; C; A; C; T; A; C; C; T; T; C; T; T; C; G; A; C; C; C; A; G; C; C; G; G; G; G; G; A; G; G; A; G; A; C; C; C; T; A; T; T; C; T; A; T; A; T; C; A; A; C; A; C; T; T; A; T; T; C; T; G; A; T; T; T; T; T; T; G; G; C; C; A; C; C; C; C; G; A; A; G; T; T; T; A; T; A; T; T; C; T; T; A; T; C; C; T; A; C; C; A; G; G; C; T; T; C; G; G; A; A; T; A; A; T; T; T; C; C; C; A; C; A; T; T; G; T; A; A; C; T; T; A; T; T; A; C; T; C; C; G; G; A; A; A; A; A; A; A; G; A; A; C; C; A; T; T; T; G; G; A; T; A; T; A; T; A; G; G; C; A; T; G; G; T; T; T; G; A; G; C; T; A; T; A; A; T; A; T; C; A; A; T; T; G; G; C; T; T; C; C; T; A; G; G; G; T; T; T; A; T; C; G; T; G; T; G; A; G; C; A; C; A; C; C; A; T; A; T; A; T; T; T; A; C; A; G; T; A; G; G; G; A; T; A; G; A; C; G; T; A; G; A; C; A; C; C; C; G; A; G; C; C; T; A; T; T; T; C; A; C; C; T; C; C; G; C; T; A; C; C; A; T; A; A; T; C; A; T; T; G; C; T; A; T; T; C; C; T; A; C; C; G; G; C; G; T; C; A; A; A; G; T; A; T; T; C; A; G; C; T; G; A; C; T; C; G; C; T; A; C; A; C; T; T; C; A; C; G; G; A; A; G; C; A; A; T; A; T; G; A; A; A; T; G; A; T; C; T; G; C; C; G; C; A; G; T; A; C; T; C; T; G; A; G; C; C; C; T; A; G; G; G; T; T; T; A; T; C; T; T; T; C; T; C; T; T; C; A; C; C; G; T; A; G; G; T; G; G; C; C; T; A; A; C; C; G; G; C; A; T; T; G; T; A; C; T; A; G; C; A; A; A; C; T; C; A; T; C; A; T; T; A; G; A; C; A; T; C; G; T; G; C; T; A; C; A; C; G; A; C; A; C; A; T; A; C; T; A; C; G; T; C; G; T; A; G; C; C; C; A; C; T; T; C; C; A; C; T; A; C; G; T; T; C; T; A; T; C; A; A; T; A; G; G; A; G; C; T; G; T; A; T; T; C; G; C; C; A; T; C; A; T; A; G; G; A; G; G; C; T; T; C; A; T; T; C; A; C; T; G; A; T; T; C; C; C; C; C; T; A; T; T; C; T; C; A; G; G; C; T; A; T; A; C; C; C; T; A; G; A; C; C; A; A; A; C; C; T; A; T; G; C; C; A; A; A; A; T; C; C; A; A; T; T; T; G; C; C; A; T; C; A; T; G; T; T; C; A; T; T; G; G; C; G; T; A; A; A; C; C; T; A; A; C; C; T; T; C; T; T; C; C; C; A; C; A; G; C; A; C; T; T; C; C; T; T; G; G; C; C; T; A; T; C; T; G; G; G; A; T; G; C; C; C; C; G; A; C; G; T; T; A; C; T; C; G; G; A; C; T; A; C; C; C; C; G; A; T; G; C; A; T; A; C; A; C; C; A; C; A; T; G; A; A; A; T; G; T; C; C; T; A; T; C; A; T; C; C; G; T; A; G; G; C; T; C; A; T; T; T; A; T; C; T; C; C; C; T; G; A; C; A; G; C; A; G; T; A; A; T; A; T; T; A; A; T; A; A; T; T; T; T; C; A; T; G; A; T; T; T; G; A; G; A; A; G; C; C; T; T; T; G; C; T; T; C; A; A; A; A; C; G; A; A; A; A; G; T; C; C; T; A; A; T; A; G; T; A; G; A; A; G; A; G; C; C; C; T; C; C; G; C; A; A; A; C; C; T; G; G; A; A; T; G; A; C; T; A; T; A; T; G; G; A; T; G; C; C; C; C; C; C; A; C; C; C; T; A; C; C; A; C; A; C; A; T; T; C; G; A; A; G; A; A; C; C; C; G; T; A; T; A];;

let human: helix = [T; A; T; A; A; A; A; C; C; C; C; C; T; G; C; C; A; T; A; A; C; C; C; A; A; T; A; C; C; A; A; A; C; G; C; C; C; C; T; T; T; T; C; G; T; C; T; G; A; T; C; C; G; T; C; C; T; A; A; T; C; A; C; A; G; C; A; G; T; C; C; T; A; C; T; T; C; T; C; C; T; A; T; C; T; C; T; C; C; C; A; G; T; C; C; T; A; G; C; T; G; C; T; G; G; C; A; T; C; A; C; T; A; T; A; C; T; A; C; T; A; A; C; A; G; A; C; C; G; C; A; A; C; C; T; C; A; A; C; A; C; C; A; C; C; T; T; C; T; T; C; G; A; C; C; C; C; G; C; C; G; G; A; G; G; A; G; G; A; G; A; C; C; C; C; A; T; T; C; T; A; T; A; C; C; A; A; C; A; C; C; T; A; T; T; C; T; G; A; T; T; T; T; T; C; G; G; C; C; A; C; C; C; T; G; A; A; G; T; T; T; A; T; A; T; T; C; T; C; A; T; C; C; T; A; C; C; A; G; G; C; T; T; C; G; G; A; A; T; A; A; T; C; T; C; C; C; A; T; A; T; T; G; T; A; A; C; T; T; A; C; T; A; C; T; C; C; G; G; A; A; A; A; A; A; A; G; A; A; C; C; A; T; T; T; G; G; A; T; A; C; A; T; A; G; G; T; A; T; G; G; T; C; T; G; A; G; C; T; A; T; G; A; T; A; T; C; A; A; T; T; G; G; C; T; T; C; C; T; A; G; G; G; T; T; T; A; T; C; G; T; G; T; G; A; G; C; A; C; A; C; C; A; T; A; T; A; T; T; T; A; C; A; G; T; A; G; G; A; A; T; A; G; A; C; G; T; A; G; A; C; A; C; A; C; G; A; G; C; A; T; A; T; T; T; C; A; C; C; T; C; C; G; C; T; A; C; C; A; T; A; A; T; C; A; T; C; G; C; T; A; T; C; C; C; C; A; C; C; G; G; C; G; T; C; A; A; A; G; T; A; T; T; T; A; G; C; T; G; A; C; T; C; G; C; C; A; C; A; C; T; C; C; A; C; G; G; A; A; G; C; A; A; T; A; T; G; A; A; A; T; G; A; T; C; T; G; C; T; G; C; A; G; T; G; C; T; C; T; G; A; G; C; C; C; T; A; G; G; A; T; T; C; A; T; C; T; T; T; C; T; T; T; T; C; A; C; C; G; T; A; G; G; T; G; G; C; C; T; G; A; C; T; G; G; C; A; T; T; G; T; A; T; T; A; G; C; A; A; A; C; T; C; A; T; C; A; C; T; A; G; A; C; A; T; C; G; T; A; C; T; A; C; A; C; G; A; C; A; C; G; T; A; C; T; A; C; G; T; T; G; T; A; G; C; T; C; A; C; T; T; C; C; A; C; T; A; T; G; T; C; C; T; A; T; C; A; A; T; A; G; G; A; G; C; T; G; T; A; T; T; T; G; C; C; A; T; C; A; T; A; G; G; A; G; G; C; T; T; C; A; T; T; C; A; C; T; G; A; T; T; T; C; C; C; C; T; A; T; T; C; T; C; C; G; G; C; T; A; C; A; C; C; C; T; A; G; A; C; C; A; A; A; C; T; T; A; C; G; C; C; A; A; A; A; T; C; C; A; T; T; T; C; A; C; T; A; T; C; A; T; A; T; T; C; A; T; C; G; G; C; G; T; A; A; A; T; C; T; A; A; C; T; T; T; C; T; T; C; C; C; A; C; A; A; C; A; C; T; T; T; C; T; C; G; G; C; C; T; A; T; C; C; G; G; A; A; T; G; C; C; C; C; G; A; C; G; T; T; A; C; T; C; G; G; A; C; T; A; C; C; C; C; G; A; T; G; C; A; T; A; C; A; C; C; A; C; A; T; G; A; A; A; T; A; T; C; C; T; A; T; C; A; T; C; T; G; T; A; G; G; C; T; C; A; T; T; C; A; T; T; T; C; T; C; T; A; A; C; A; G; C; A; G; T; A; A; T; A; T; T; A; A; T; A; A; T; T; T; T; C; A; T; G; A; T; T; T; G; A; G; A; A; G; C; C; T; T; C; G; C; T; T; C; G; A; A; G; C; G; A; A; A; A; G; T; C; C; T; A; A; T; A; G; T; A; G; A; A; G; A; A; C; C; C; T; C; C; A; T; A; A; A; C; C; T; G; G; A; G; T; G; A; C; T; A; T; A; T; G; G; A; T; G; C; C; C; C; C; C; A; C; C; C; T; A; C; C; A; C; A; C; A; T; T; C; G; A; A; G; A; G; C; C; C; G; T; A; T; A];;

let test1: helix = [T; C; G; C; T; A; A; C];;
let test2: helix = [T; G; T; A; C; A; G; C];;
let test3: helix = [G; T; A; C; T; G; C; A];;
let test4: helix = [A; C; T; C; G; A; C; T];;

type acid =
    Ala | Arg | Asn | Asp | Cys | Glu | Gln | Gly | His | Ile | Leu | Lys | Met
  | Phe | Pro | Ser | Thr | Trp | Tyr | Val | END;;

(* sample tree for testing *)
let sample_tree = Leaf human
let sample_tree2 = Node ( Leaf gorilla, lar_gibbon, Leaf orangutan)
let sample_tree3 = Node ( Leaf test1, test2, Leaf test3)

(*
  Before we generate all possible trees, let's write a useful helper function:
*)

(* Finds the helix of the root node in the tree. *)
let helix_of_tree (r: tree): helix =
  match r with
  | Leaf helix -> helix
  | Node (left, helix, right) -> helix
;;

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
  match x1, x2 with
  | h1 :: t1, h2 :: t2 ->
      (if h1 = h2 then h1 else A) :: guess_parent_helix t1 t2
  | [], [] -> []
  | _ -> failwith "invalid input: x1 and x2 have unequal lengths"
;;
                                                                  
(* Given an unlabeled tree, replicate its structure but with valid helices at
   internal nodes. You can pass two valid child helices to guess_parent_helix to
   get the valid helix that should label their parent in the output tree.
   
   In other words, this function should guess the DNA of ancestor species in the
   possible evolutionary tree. *)
let rec labeled_tree (r: tree): tree =
  match r with 
 	| Leaf l -> Leaf l
 	| Node (left, helix, right) ->
 		let left = (labeled_tree left) in
 		let right = (labeled_tree right) in
 		Node (left, (guess_parent_helix (helix_of_tree left)(helix_of_tree right)), right)
;;

labeled_tree sample_tree3
(*
  Now we have a way of generating all possible trees -- i.e., all possible
  hypotheses about possible evolutionary relations between the species at the
  leaves.  What we need is a way of comparing hypotheses to see which is more
  plausible.

  In the following problems, we will encode three kinds complexity scores
  for trees.  Then, using a (provided) function for finding the simplest tree,
  we can hypothesize relationships between the apes.
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
  |[] -> 0
  | h :: t -> 1 + acids_length t
  end

(* Given a labeled tree, find all acid chains obtained by decoding the helices
   of internal nodes, and output the sum of their lengths. Don't add the length
   of acid chains from leaves. *)
let rec sum_of_acids_length (r: tree): int =
  -1 (* STUB *)
;;


