open Core.Std

Random.self_init()
let number = Random.int 37

let roulette _ =

	if number = 0 then
		"Green"
	else 
		if (number > 0 && number < 11) || (number > 18 && number < 29) then
			if number mod 2 = 0 then
				"Black"
			else
				"Red"
		else 
			if number mod 2 = 0 then
				"Red"
			else
				"Black"
;;
let answer = string_of_int(number) ^ " " ^ roulette()
