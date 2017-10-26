type ptypes = TNormal | TFire | TWater

type pokemon = {ptype: ptypes; name: string; health: int}

let charizard : pokemon = {ptype=TFire; name="Charizard"; health=78}

let metapod : pokemon = {ptype=TNormal; name="Metapod"; health=50}

let squirtle : pokemon = {ptype=TWater; name="Squirtle"; health=64}

let _pokemon = [charizard; metapod; squirtle]

let rec highestHealth pokeList =
	match pokeList with
	| [] -> None
	| h::t -> match (highestHealth t) with
				| None -> Some h.health
				| Some t -> Some (max h.health t)
;;

let rec avgHealth pokeList =
	match pokeList with
	| [] -> None
	| h::t -> match (avgHealth t) with
				| None -> Some h.health
				| Some m -> Some ((h.health + (avgHealth m)) / (List.length pokeList))
;;

highestHealth _pokemon;;

avgHealth _pokemon;;