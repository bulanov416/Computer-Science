let rec nth (lst) (n: int) : int =
	match lst with
	| [] -> 0
	| h::t -> if n = 0 then h else (nth t (n-1))

;;

nth [1;2;3;4;5;6] 3;;