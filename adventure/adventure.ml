(*
Type and important variable definitions
*)

type direction = Left | Right | Forward | Backward | Up | Down | Other;;
type verb = Go | Take | Drop | Look | Inv | Score | Turns | Other | Parse | StartGame | Continue;;
type visited = Yes | No;;
let instructions : string = "\nWelcome to Alex's Text Based Adventure Game! The main objective of this game is to navigate through rooms, collect items, and achieve the maximum possible score in the least amount of turns.\nHere is a list of all possible commands:\n  go + left, right, forward, backward (back), up, down\n  take + the name of an item in the room\n  drop + the name of an item in your inventory\n  inventory (inv)\n  look\n  score\n  turns\n  help\n  quit\n\nGood luck on your journey!\n"

type item = {
	id: int;
	name: string;
	description: string;
	points: int;
	treasureMessage: string;
}

type room = {
	id: int; 
	name: string;
	description: string;
	points: int;
	exits: exit list;
	treasure: item list;
	items: item list;
}

and exit = {
	direction: direction;
	nextRoom: int;
}

type currentPlayerState = {

	(*Room Stuff*)
	rooms: room list;
	currentRoom: int;
	visitedRooms: int list;
	startingRoom: int;

	(*Item Stuff*)
	items: item list;
	inventory: item list;
	score: int;
	turns: int;
}

let nonExistentItem : item = {
	id = 0;
	name = "";
	description = "";
	points = 0;
	treasureMessage = "";
}

let nonExistentRoom : room = {
	id = 0;
	name = "";
	description = "";
	points = 0;
	exits = [];
	treasure = [];
	items = [];
}

(*
Game Model
*)

let paper : item = {
	id= 1;
	name= "Paper";
	description= "Something to write on";
	points= 0;
	treasureMessage= "Hmmm, I wonder what is written on this wonderful piece of paper.";
}

let pen : item = {
	id= 2;
	name= "Pen";
	description= "Something to write with";
	points= 0;
	treasureMessage= "Hmmm, I wonder how much ink is left in this pen.";
}

let startingState : currentPlayerState = {
	rooms= [{id= 1; name= "Boston"; description= ""; points= 0; exits= [{direction= Forward; nextRoom= 2}]; treasure= [paper]; items= [pen]};
			{id= 2; name= "Living Room"; description= "You live in this room"; points= 3; exits= [{direction= Backward; nextRoom= 1}; {direction= Left; nextRoom= 3}]; treasure= [pen]; items= [paper]};
			{id= 3; name= "Driveway"; description= "The end of the game room"; points= 0; exits= [{direction= Backward; nextRoom= 2}]; treasure= []; items= []}];
	(*make sure to add all rooms to this list*)
	startingRoom= 1;
	currentRoom= 1;
	visitedRooms= [];
	items= [paper; pen]; (*make sure to add all items to this list*)
	inventory= [];
	score= 0;
	turns= 0;
}

(*
Helper Functions
*)

let rec getRoomFromId (id : int)(rooms : room list) =
	match rooms with
	| [] -> nonExistentRoom
	| h::t -> if (h.id = id) then h else (getRoomFromId id t)
;;

let updateScore (state : currentPlayerState)(room : room) =
	state.score + room.points
;;

let rec getMaxItemScore (items : item list) : int =
	match items with
	| [] -> 0
	| h::t -> h.points + (getMaxItemScore t)
;;

let rec getMaxRoomScore (rooms : room list) : int =
	match rooms with
	| [] -> 0
	| h::t -> h.points + (getMaxRoomScore t)
;;

let rec getMaxScore (rooms : room list)(items : item list) : int =
	getMaxRoomScore rooms + getMaxItemScore items
;;	

let directionToString (direction : direction) : string =
	match direction with
	| Forward -> "Forward"
	| Backward -> "Backward"
	| Up -> "Up"
	| Down -> "Down"
	| Left -> "Left"
	| Right -> "Right"
	| Other -> ""
;;

let rec print_items (items : item list) : string = 
	match items with
	| [] -> ""
	| h::t -> if (t = []) then h.name else h.name ^ ", " ^ (print_items t)
;;

let rec print_exits (exits : exit list) : string =
	match exits with
	| [] -> ""
	| h::t -> if (t = []) then (directionToString h.direction) else (directionToString h.direction) ^ ", " ^ (print_exits t)
;;


let rec print_inventory (state : currentPlayerState) : string = 
	match state.inventory with
	| [] -> ""
	| h::t -> if (t = []) then (Printf.printf "%s." h.name) else (Printf.printf "%s, " h.name) ; (print_items t)
;;

let rec print_rooms (rooms : room list) =
	match rooms with
	| [] -> ()
	| h::t -> print_string h.name ; print_rooms t
;;

let rec getItemTreasureRoomTreasureList (treasure : item list)(item : item) : item =
	match treasure with
	| [] -> nonExistentItem
	| h::t -> if (h.id = item.id) then h else getItemTreasureRoomTreasureList t item
;;

let rec getItemTreasureRoom (rooms : room list)(item : item) : room =
	match rooms with
	| [] -> nonExistentRoom
	| h::t -> if (match h.treasure with | [] -> nonExistentItem | m::n -> if (m.id = item.id) then m else (getItemTreasureRoomTreasureList n item)).id = item.id then h else getItemTreasureRoom t item
;;

let rec addRoom (roomId : int)(rooms : int list) =
	match rooms with
	| [] -> [roomId]
	| h::t -> h :: (addRoom roomId t)
;;

let addItem (item : item)(items : item list) : item list =
	if (items = [item]) then
		[item]
	else 
		item :: items
;;

let rec removeRoom (room : room)(rooms : room list) =
	match rooms with
	| [] -> rooms
	| h::t -> if (h.id = room.id) then t else h::(removeRoom room t)
;;

let rec removeItem1 (item : item)(items : item list) =
	match items with
	| [] -> items
	| h::t -> if (h.id = item.id) then t else h::(removeItem1 item t)
;;

let rec removeItemList (item : item)(items : item list) : item list =
	match items with
	| [] -> items
	| h::t -> if (h.id = item.id) then t else (removeItemList item t)
;;

let rec removeItem (item : item)(state : currentPlayerState) : room list =
	match (getRoomFromId state.currentRoom state.rooms).items with
	| [] -> state.rooms
	| h::t -> if (h.id = item.id) then 
				let room = (getRoomFromId state.currentRoom state.rooms) in
				let rooms = (removeRoom room state.rooms) in
				[{room with items = (removeItem1 h room.items)}]@rooms
			else 
				let room = (getRoomFromId state.currentRoom state.rooms) in
				let rooms = (removeRoom room state.rooms) in
				[{room with items = (removeItemList item t)}]@rooms
;;

let rec addItemList (item : item)(items : item list) : item list =
	item::items
;;

let rec addItem (item : item)(state : currentPlayerState) : room list =
	if ((getRoomFromId state.currentRoom state.rooms).items = []) then
		let room = (getRoomFromId state.currentRoom state.rooms) in
				let rooms = (removeRoom room state.rooms) in
				[{room with items = [item]}]@rooms
	else
		let room = (getRoomFromId state.currentRoom state.rooms) in
				let rooms = (removeRoom room state.rooms) in
				[{room with items = (item::(getRoomFromId state.currentRoom state.rooms).items)}]@rooms
;;

let rec getRoomDescription (state : currentPlayerState) : string = (*Returns the description of the current room*)
	"\n\n You are now the " ^ (getRoomFromId state.currentRoom state.rooms).name ^".\n\n" ^ (getRoomFromId state.currentRoom state.rooms).description 
;;

let rec checkItemExistence (items : item list)(itemName : string) : bool =
	match items with
	| [] -> false
	| h::t -> if ((String.lowercase_ascii h.name) = (String.lowercase_ascii itemName)) then true else (checkItemExistence t itemName)
;;

let rec findExitExistence (exits : exit list)(direction : direction) : bool =
	match exits with
	| [] -> false
	| h::t -> if (h.direction = direction) then true else (findExitExistence t direction)
;;

let rec retrieveNextRoom (exits : exit list)(direction : direction)(state : currentPlayerState) = 
	match exits with
	| [] -> nonExistentRoom
	| h::t -> if (h.direction = direction) then (getRoomFromId h.nextRoom state.rooms) else (retrieveNextRoom t direction state)
;;

let rec getRoomFromId (id : int)(rooms : room list) : room = 
	match rooms with
	| [] -> nonExistentRoom
	| h::t -> if (h.id = id) then h else (getRoomFromId id t)
;;

let rec checkIfRoomVisited (room : room)(rooms : int list) : visited =
	match rooms with
	| [] -> No
	| h::t -> if (h = room.id) then Yes else checkIfRoomVisited room t
;;

let rec checkIfItemInTreasureRoom (items : item list)(item : item) : bool= 
	match items with
	| [] -> false
	| h::t -> if (h.id = item.id) then true else checkIfItemInTreasureRoom t item
;;

(*
Command Parse Engine
*)

let firstWord (str : string) : string =
	if str = "" then
		""
	else
		String.lowercase_ascii (List.hd (Str.split (Str.regexp " ") str))
;;

let secondWord (str : string) : string = 
	if str = "" then
		""
	else
		List.nth (Str.split (Str.regexp " ") str) 1
;;

let commandToDirection (str : string) : direction =
	match (String.lowercase_ascii str) with
	| "forward" -> Forward
	| "front" -> Forward
	| "backward" -> Backward
	| "back" -> Backward
	| "backwards" -> Backward
	| "forwards" -> Forward
	| "up" -> Up
	| "down" -> Down
	| "left" -> Left
	| "right" -> Right
	| _ -> Other
;;

let commandToVerb (str : string) : verb =
	match (String.lowercase_ascii (firstWord str)) with
	| "go" -> Go
	| "take" -> Take
	| "drop" -> Drop
	| "look" -> Look
	| "inv" -> Inv
	| "inventory" -> Inv
	| "score" -> Score
	| "turns" -> Turns
	| _ -> Other
;;

let rec itemFromCommand (command : string)(items : item list) : item option =
	match items with
	| [] -> None
	| h::t -> if ((String.lowercase_ascii h.name) = (String.lowercase_ascii command)) then Some h else (itemFromCommand command t)
;;

let rec interface (state : currentPlayerState) =
	print_string "\nStart Game? (Yes or No)\n>";
	let input = read_line() in
		if (String.lowercase_ascii input = "no") then begin
			print_string "\nOkay. Goodbye!\n";
			()
		end
		else if (String.lowercase_ascii input = "yes") then begin
			print_string instructions;
			Printf.printf "\nThe maximum possible score is %i points.\n" (getMaxScore state.rooms state.items);
			Printf.printf "\nYou are now in the %s.\n\n%s.\n\n" (String.lowercase_ascii (getRoomFromId state.currentRoom state.rooms).name) (getRoomFromId state.currentRoom state.rooms).description;
			Printf.printf "There are exits in these directions:\n   %s\n\n" (print_exits (getRoomFromId state.currentRoom state.rooms).exits);
			game ({state with visitedRooms = addRoom state.startingRoom state.visitedRooms}) "Launch"
		end
		else
			print_string "\nSorry, that is not a valid command. Please restrict your answer to 'Yes' or 'No'.\n";
			interface state

and game (state : currentPlayerState)(command : string) =
	if (command = "Launch") then begin
		if ((getRoomFromId state.currentRoom state.rooms).items = []) then begin
			print_string "There are no items in this room\n\n";
		end
		else
			Printf.printf "These are the items in this room:\n  %s\n\n" (print_items (getRoomFromId state.currentRoom state.rooms).items); 
			(print_string "What is your next move?\n>");
			(game ({state with visitedRooms = addRoom state.currentRoom state.visitedRooms }) (read_line()))
	end
	else if ((firstWord (String.lowercase_ascii command) = "go")) then begin
		if (findExitExistence (getRoomFromId state.currentRoom state.rooms).exits (commandToDirection (secondWord command))) then begin
			if ((checkIfRoomVisited ((retrieveNextRoom (getRoomFromId state.currentRoom state.rooms).exits (commandToDirection (secondWord command))) state) state.visitedRooms) = Yes) then
				game ({state with currentRoom = (retrieveNextRoom (getRoomFromId state.currentRoom state.rooms).exits (commandToDirection (secondWord command)) state).id; turns = state.turns + 1;}) ""
			else if ((checkIfRoomVisited ((retrieveNextRoom (getRoomFromId state.currentRoom state.rooms).exits (commandToDirection (secondWord command))) state) state.visitedRooms) = No) then
				game ({state with currentRoom = (retrieveNextRoom (getRoomFromId state.currentRoom state.rooms).exits (commandToDirection (secondWord command)) state).id; turns = state.turns + 1; score = (updateScore (state) (retrieveNextRoom (getRoomFromId state.currentRoom state.rooms).exits (commandToDirection (secondWord command)) state))}) ""
		end
		else
			print_string "\nThere is no exit in that direction.\n\n";
			game state ""
	end
	else if ((firstWord (String.lowercase_ascii command) = "take")) then begin
		if (((String.lowercase_ascii command) = "take" || (firstWord (String.lowercase_ascii command) = "take "))) then begin
			print_string "\nPlease specify which item you would like to take.\n\n";
			game state "";
		end
		else begin
			if ((getRoomFromId state.currentRoom state.rooms).items = []) then begin
				print_string "\nSilly you, there are no items in this room! You can't pick up something that doesn't exist ;)\n\n";
				game state "";
			end
			else
				if ((checkItemExistence (getRoomFromId state.currentRoom state.rooms).items (String.lowercase_ascii (secondWord command))) = true) then begin
					Printf.printf "\nYou just picked up: %s.\n\n" (String.capitalize_ascii (secondWord command));
					match (itemFromCommand (secondWord command) ((getRoomFromId state.currentRoom state.rooms)).items) with
					| None -> print_string "\nThat item does not exist in this room.\n"; (game (state) (""))
					| Some item -> if (checkIfItemInTreasureRoom (getRoomFromId state.currentRoom state.rooms).treasure item) = false then begin 
										Printf.printf "Drop this item in the %s to recieve the points it is worth.\n\n" (String.lowercase_ascii (getItemTreasureRoom state.rooms item).name);
										(game {state with inventory = addItemList item state.inventory; rooms = removeItem item state; turns = state.turns + 1} "");
									end
									else begin
										Printf.printf "Picking this item up from this room will detract %i points from your score. This item should be dropped in this room if you want to recieve the points it is worth.\n\n" (item.points);
										(game {state with inventory = addItemList item state.inventory; rooms = removeItem item state; turns = state.turns + 1; score = state.score - item.points} "");
									end
				end
				else begin
					print_string "\nThat item does not exist in this room.\n\n";
					game state "";
				end
		end
	end
	else if ((firstWord (String.lowercase_ascii command) = "drop")) then begin
		if (String.lowercase_ascii command = "drop" || String.lowercase_ascii command = "drop") then begin
			print_string "\nPlease specify which item you would like to drop.\n\n";
			game state "";
		end
		else begin
			if (state.inventory = []) then begin
				print_string "\nSilly you, your inventory is empty! There is nothing to drop ;)\n\n";
				game state "";
			end
			else
				if ((checkItemExistence state.inventory (String.lowercase_ascii (secondWord command))) = true) then begin
					Printf.printf "\nYou just dropped: %s.\n\n" (String.capitalize_ascii (secondWord command));
					match (itemFromCommand (secondWord command) (state.inventory)) with
					| None -> print_string "\nYou don't have an item by that name in your inventory.\n"; (game (state) (""))
					| Some item -> if (checkIfItemInTreasureRoom (getRoomFromId state.currentRoom state.rooms).treasure item) = false then begin
										Printf.printf "Dropping this item here will not add any points to your score. This item should to be dropped in the %s if you want to receive the points it is worth.\n\n" (String.lowercase_ascii (getItemTreasureRoom state.rooms item).name);
										(game {state with inventory = removeItem1 item state.inventory; rooms = addItem item state; turns = state.turns + 1} "");
									end
									else begin
										Printf.printf "By dropping this item in the correct room, you have gained %i points!\n\n" item.points;
										(game {state with inventory = removeItem1 item state.inventory; rooms = addItem item state; turns = state.turns + 1; score = state.score + item.points} "");
									end
				end
				else begin
				print_string "\nThat item does not exist in this room.\n\n";
				game state "";
				end
		end
	end 
	else if ((firstWord (String.lowercase_ascii command) = "quit")) then begin
		print_string "\nPress control-c to close the game";
		game state (read_line());
	end	
	else if ((firstWord (String.lowercase_ascii command) = "look")) then begin
		Printf.printf "\nYou are now in the %s.\n\n%s.\n\n" (String.lowercase_ascii (getRoomFromId state.currentRoom state.rooms).name) ((getRoomFromId state.currentRoom state.rooms).description);
		Printf.printf "There are exits in these directions:\n  %s\n\n" (print_exits (getRoomFromId state.currentRoom state.rooms).exits);
		if ((getRoomFromId state.currentRoom state.rooms).items = []) then begin
			print_string "There are no items in this room\n\n";
		end
		else
			Printf.printf "These items are in this room:\n  %s\n\n" (print_items (getRoomFromId state.currentRoom state.rooms).items);
		game state "";
	end
	else if ((firstWord (String.lowercase_ascii command) = "inv")) || ((firstWord (String.lowercase_ascii command) = "inventory")) then begin
		if (state.inventory = []) then begin
			print_string "\nYour inventory is empty.\n\n";
			game state "";
		end
		else begin
			print_string "\n";
			(Printf.printf "These are the items in your inventory:\n  %s\n\n" (print_items state.inventory));
			game state "";
		end
	end
	else if ((String.lowercase_ascii (firstWord command)) = "help") then begin
		print_string instructions;
		game state "";
	end
	else if ((String.lowercase_ascii (firstWord command)) = "score") then begin
		Printf.printf "\nYour score is %i.\n\n" state.score;
		Printf.printf "The maximum possible score is %i.\n\n" (getMaxScore state.rooms state.items);
		game state "";
	end
	else if ((String.lowercase_ascii (firstWord command)) = "turns") then begin
		Printf.printf "\nYou have used %i turns.\n\n" state.turns;
		game state "";
	end
	else if ((firstWord command) = "") then begin
		if (state.score != getMaxScore state.rooms state.items) then begin
			if ((checkIfRoomVisited (getRoomFromId state.currentRoom state.rooms) state.visitedRooms) = Yes) then begin
				(print_string "What is your next move?\n>");
				game (state) (read_line());
			end
			else
				Printf.printf "\nYou are now in the %s.\n\n%s.\n\n" (String.lowercase_ascii (getRoomFromId state.currentRoom state.rooms).name) ((getRoomFromId state.currentRoom state.rooms).description);
				Printf.printf "There are exits in these directions:\n  %s\n\n" (print_exits (getRoomFromId state.currentRoom state.rooms).exits);
				if ((getRoomFromId state.currentRoom state.rooms).items = []) then begin
					print_string "There are no items in this room\n\n";
				end
				else
					Printf.printf "These are the items in this room:\n  %s\n\n" (print_items (getRoomFromId state.currentRoom state.rooms).items); 
					print_string "What is your next move?\n>";
					game ({state with visitedRooms = addRoom state.currentRoom state.visitedRooms}) (read_line())
		end
		else begin
			print_string "\nCongratulations! You have achieved the maximum possible score. Type \"again\" to play again or \"quit\" to close the game.\n";
			let input = read_line() in
				if (input = "again") then
					interface startingState
				else if (input = "quit") then begin
					print_string "\nPress control-c to close the game";
					game state (read_line())
				end
				else begin
					print_string "Sorry, that is not a valid command. Please restrict your command to \"again\" or \"quit\".";
					game state ""
				end
		end
	end
	else
		print_string "\nThat is not a valid command. Type \"help\" if you're stuck.\n\n";
		(print_string "What is your next move?\n>");
		game (state) (read_line());
;;


interface startingState

(*
To-Do
-Get of error when typing just go or take
*)












