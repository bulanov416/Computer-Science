type date = {month:int; day:int}
let birthday : date = {month=4; day=16}
let birthday2 : date = {month=6; day=20}

module type Stuff = sig
	type t
	val compare : t -> t -> int
end

module Date : Stuff = struct
	type t = date
	let compare (date1 : date)(date2: date) : int =
	if (date1.month < date2.month) then
		-1
	else if (date1.month = date2.month) then
		if (date1.day < date2.day) then
			-1
		else if (date1.day = date2.day) then
			0
		else
			1
	else
		1
end