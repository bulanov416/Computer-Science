let rec fib n =
	if n = 1 then
		1
	else
		n + fib(n-1)
;;

fib(15);;
