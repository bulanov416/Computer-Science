let rec powerset list = 
	match list with
	| [] -> [[]]
	| h::t -> let list = powerset t in 
		list @ (List.map 
					(fun a -> h :: a) list)
;;

powerset [2;4;6;8]