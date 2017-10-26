(*type months = January | Febuary | March | April | May | June | July | August | September | October | November | December*)

let months = ["January"; "Febuary"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"]

type date = {year: int; month: int; day: int;}

let myBirthday : date = {year= 2001; day= 16; month= 4}

let sisterBirthday : date = {year= 2004; day= 10; month= 2}

let momBirthday : date = {year= 1960; day= 17; month= 7}

let dadBirthday : date = {year= 1962; day= 9; month = 6}

let dateList : date list = [myBirthday; sisterBirthday; momBirthday; dadBirthday]

let is_before (date1 : date)(date2: date) : bool =
	if (date1.year < date2.year) then
		true
	else if (date1.year = date2.year) then
		if (date1.month < date2.month) then
			true
		else if (date1.month = date2.month) then
			if (date1.day < date2.day) then
				true
			else
				false
		else
			false
	else
		false
;;

let is_before_date (date1 : date)(date2: date) : date =
	if (date1.year < date2.year) then
		date1
	else if (date1.year = date2.year) then
		if (date1.month < date2.month) then
			date1
		else if (date1.month = date2.month) then
			if (date1.day < date2.day) then
				date1
			else
				date2
		else
			date2
	else
		date2
;;

let string_of_date (date: date) : string = 
	(List.nth months (date.month - 1)) ^ " " ^ (string_of_int date.day) ^ ", " ^ (string_of_int date.year)
;;

let rec earliest (dates: date list) : date option =
	match dates with
	| [] -> None
	| h::t -> match (earliest t) with
				| None -> Some h
				| Some t -> Some (is_before_date h t) 
;;	

is_before myBirthday sisterBirthday;;
string_of_date myBirthday;;
earliest dateList;;