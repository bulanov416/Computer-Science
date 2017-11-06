(*
Type and important variable definitions
*)
type direction = Left | Right | Forward | Backward | Up | Down | Other;; (*Different directions one can go through. Other is to exhaust all possible match cases.*)
let instructions : string = "\nWelcome to Alex's Text Based Adventure Game! The main objective of this game is to navigate through rooms, collect items, and achieve the maximum possible score in the least amount of turns. You gain points by entering a room for the first time and by dropping an item in the correct room.\nHere is a list of all possible commands:\n  go + left, right, forward, backward (back), up, down\n  take + the name of an item in the room\n  drop + the name of an item in your inventory\n  inventory (inv)\n  look\n  score\n  turns\n  help\n  quit\n\nIf you want to go to the previous room, always type \"back\". Back will always take you to the previous room, regardless of the direction you took to get to where you are now.\n\nGood luck on your journey!\n" 
let bostonDescription : string = "You stand in the middle of Fenway. There are 37,000 people watching you, cheering at the top of their lungs. The Boston skyline glows with an orange tint. On your left, you see a sign that says \"DROP THE CUBS JERSEY HERE\"."
let newYorkDescription : string = "You stand in the middle of Times Square. People rush everywhere, everyone having somewhere to go. Giant televisions show ads for designer clothing companies. Huge buildings surround you. On your left, you see a sign that says \"DROP THE COWBOY HAT HERE\"."
let chicagoDescription : string = "You stand on the 103rd floor of the Sears Tower, looking onto the city. You see the ocean in the distance, and skyscrapers poking holes in the cloudy sky. On your left, you see a sign that says \"DROP THE SOUVENIR OSCAR HERE\"."
let dallasDescription : string = "You stand in a tunnel with glass walls and ceiling. Fish swim all around you, a sight you thought you would only see in the ocean. Outside of the aquarium, you see a busy city. The air is dry and arid. On your left, you see a sign that says \"DROP THE BROADWAY TICKET HERE\"."
let losAngelesDescription : string = "You stand on the Hollywood Walk of Fame. In the distance, you can see the large, white, famous Hollywood sign. Your favorite actor's star is somewhere on this street. On your left, you see a sign that says \"DROP THE BASEBALL HERE\"."
(*^Quick overview of the game and list of all possible commands.^*)

(*Defining the item type. Items can be taken and dropped. Points gained if items dropped in correct room. Points lost if taken from current room*)
type item = {
	id: int; (*Identifying field for item*)
	name: string; (*Name of the item*)
	description: string; (*Description of the item*)
	points: int; (*Points gained if item dropped in correct room*)
}

type room = {
	id: int;  (*Identifying field for the room*)
	name: string; (*Name of the room*)
	description: string; (*Description of the room*)
	points: int; (*Points gained from entering the room*)
	exits: exit list; (*Exits in the room*)
	treasure: item list; (*Items that should be dropped in this room*)
	items: item list; (*Items that can be found in this room*)
}

and exit = {
	direction: direction; (*The direction (type direction) of an exit*)
	nextRoom: int; (*The ID of the room this exit leads to*)
}

type currentPlayerState = {

	(*Room Stuff*)
	rooms: room list; (*List of all rooms being used in the game*)
	currentRoom: int; (*ID of the room the player is currently in*)
	visitedRooms: int list; (*List of the IDs of the rooms the player has been in*)
	startingRoom: int; (*ID of the room the player starts in at the beginning of the game*)

	(*Item Stuff*)
	items: item list; (*List of all the items in the game*)
	inventory: item list; (*List of items in the player's inventory*)
	score: int; (*Player's score*)
	turns: int; (*Amount of turns the player has taken*)
}
(*Item returned as a match case in certain item matching functions. Must always exist. Part of the game engine. Do not modify*)
let nonExistentItem : item = {
	id = 0;
	name = "";
	description = "";
	points = 0;
}
(*Room returned as a match case in certain item matching functions. Must always exist. Part of the game engine. Do not modify*)
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
(*Item Declaration Begin*)
let baseball : item = {
	id= 1;
	name= "Baseball";
	description= "It is a white and red baseball with Ted Williams' signature on it.";
	points= 3;
}

let ticket : item = {
	id= 2;
	name= "Broadway Ticket";
	description= "It is a ticket to the Broadway show Hamilton, the American Musical.";
	points= 5;
}

let jersey : item = {
	id= 3;
	name= "Cubs Jersey";
	description= "It is a cubs jersey signed by Kris Bryant.";
	points= 8;
}

let hat : item = {
	id= 4;
	name= "Cowboy Hat";
	description= "It is a typical cowboy hat.";
	points= 10;
}

let oscar : item = {
	id= 5;
	name= "Souvenir Oscar";
	description= "It is a mini souvenir Oscar with your name on it.";
	points= 15;
}
(*Item Declaration End*)


let startingState : currentPlayerState = {
	rooms= [{id= 1; name= "Boston"; description= bostonDescription; points= 0; exits= [{direction= Forward; nextRoom= 2}]; treasure= [jersey]; items= [baseball]};
			{id= 2; name= "New York"; description= newYorkDescription; points= 3; exits= [{direction= Backward; nextRoom= 1}; {direction= Left; nextRoom= 3}]; treasure= [hat]; items= [ticket]};
			{id= 3; name= "Chicago"; description= chicagoDescription; points= 5; exits= [{direction= Backward; nextRoom= 2}; {direction= Right; nextRoom= 4}]; treasure= [oscar]; items= [jersey]};
			{id= 4; name= "Dallas"; description= dallasDescription; points= 8; exits= [{direction= Backward; nextRoom= 3}; {direction= Forward; nextRoom= 5}]; treasure= [ticket]; items= [hat]};
			{id= 5; name= "Los Angeles"; description = losAngelesDescription; points= 15; exits= [{direction= Backward; nextRoom= 4}]; treasure= [baseball]; items= [oscar]} 
			];
	(*Make sure to add all rooms to this list*)
	currentRoom= 1;
	startingRoom= 1; (*Should be the same as current room*)
	visitedRooms= []; (*Should be empty*)
	items= [baseball; ticket; jersey; hat; oscar]; (*Make sure to add all items to this list*)
	inventory= []; (*Should be empty at the start of the game unless you want the player to start with a certain item. If a player starts with an item, make sure it is not also in a room.*)
	score= 0; (*Should be 0*)
	turns= 0; (*Should be 0*)
}

(*
Helper Functions
*)

(*Retrieves a room from a provided ID*)
let rec getRoomFromId (id : int)(rooms : room list) =
	match rooms with
	| [] -> nonExistentRoom
	| h::t -> if (h.id = id) then h else (getRoomFromId id t)
;;

(*Updates the score according to the amount of points the room is worth*)
let updateScore (state : currentPlayerState)(room : room) =
	state.score + room.points
;;

(*Combines the score of every item in the game*)
let rec getMaxItemScore (items : item list) : int =
	match items with
	| [] -> 0
	| h::t -> h.points + (getMaxItemScore t)
;;

(*Combines the score of every room in the game*)
let rec getMaxRoomScore (rooms : room list) : int =
	match rooms with
	| [] -> 0
	| h::t -> h.points + (getMaxRoomScore t)
;;

(*Combines max item score with max room score to produce maximum possible score in the whole game*)
let rec getMaxScore (rooms : room list)(items : item list) : int =
	getMaxRoomScore rooms + getMaxItemScore items
;;	

(*Converts the type direction to a string*)
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

(*Returns a string with all items in a list. Commas are is added if there is more than one item.*)
let rec print_items (items : item list) : string = 
	match items with
	| [] -> ""
	| h::t -> if (t = []) then h.name else h.name ^ ", " ^ (print_items t)
;;

(*Returns a string with all exits in a room. Commas are is added if there is more than one exit.*)
let rec print_exits (exits : exit list) : string =
	match exits with
	| [] -> ""
	| h::t -> if (t = []) then (directionToString h.direction) else (directionToString h.direction) ^ ", " ^ (print_exits t)
;;

(*Part two of the getItemTreasureRoom function. Takes an item list as a parameter instead of room list. This makes recursion possible.*)
let rec getItemTreasureRoomTreasureList (treasure : item list)(item : item) : item =
	match treasure with
	| [] -> nonExistentItem
	| h::t -> if (h.id = item.id) then h else getItemTreasureRoomTreasureList t item
;;

(*Retrieves the treasure room for an item*)
let rec getItemTreasureRoom (rooms : room list)(item : item) : room =
	match rooms with
	| [] -> nonExistentRoom
	| h::t -> if (match h.treasure with | [] -> nonExistentItem | m::n -> if (m.id = item.id) then m else (getItemTreasureRoomTreasureList n item)).id = item.id then h else getItemTreasureRoom t item
;;

(*Adds a room ID to a room ID list*)
let rec addRoom (roomId : int)(rooms : int list) =
	match rooms with
	| [] -> [roomId]
	| h::t -> h :: (addRoom roomId t)
;;

(*Adds an item to an item list*)
let addItem (item : item)(items : item list) : item list =
	if (items = [item]) then
		[item]
	else 
		item :: items
;;

(*Removes a room from a room list*)
let rec removeRoom (room : room)(rooms : room list) =
	match rooms with
	| [] -> rooms
	| h::t -> if (h.id = room.id) then t else h::(removeRoom room t)
;;

(*Removes an item from an item list*)
let rec removeItemList (item : item)(items : item list) =
	match items with
	| [] -> items
	| h::t -> if (h.id = item.id) then t else h::(removeItemList item t)
;;

(*Removes an item from a current room*)
let rec removeItem (item : item)(state : currentPlayerState) : room list =
	match (getRoomFromId state.currentRoom state.rooms).items with
	| [] -> state.rooms
	| h::t -> if (h.id = item.id) then 
				let room = (getRoomFromId state.currentRoom state.rooms) in
				let rooms = (removeRoom room state.rooms) in
				[{room with items = (removeItemList h room.items)}]@rooms
			else 
				let room = (getRoomFromId state.currentRoom state.rooms) in
				let rooms = (removeRoom room state.rooms) in
				[{room with items = (removeItemList item t)}]@rooms
;;

(*Adds an item to a list*)
let rec addItemList (item : item)(items : item list) : item list =
	item::items
;;

(*Adds an item to a current room*)
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

(*Checks if an item is present in an item list*)
let rec checkItemExistence (items : item list)(itemName : string) : bool =
	match items with
	| [] -> false
	| h::t -> if ((String.lowercase_ascii h.name) = (String.lowercase_ascii itemName)) then true else (checkItemExistence t itemName)
;;

(*Checks if there is an exit with a certain direction in an exit list*)
let rec findExitExistence (exits : exit list)(direction : direction) : bool =
	match exits with
	| [] -> false
	| h::t -> if (h.direction = direction) then true else (findExitExistence t direction)
;;

(*Retrives the next room in a certain direction*)
let rec retrieveNextRoom (exits : exit list)(direction : direction)(state : currentPlayerState) = 
	match exits with
	| [] -> nonExistentRoom
	| h::t -> if (h.direction = direction) then (getRoomFromId h.nextRoom state.rooms) else (retrieveNextRoom t direction state)
;;

(*Retrieves a room from a room ID*)
let rec getRoomFromId (id : int)(rooms : room list) : room = 
	match rooms with
	| [] -> nonExistentRoom
	| h::t -> if (h.id = id) then h else (getRoomFromId id t)
;;

(*Checks if a room has been visited*)
let rec checkIfRoomVisited (room : room)(rooms : int list) : bool =
	match rooms with
	| [] -> false
	| h::t -> if (h = room.id) then true else checkIfRoomVisited room t
;;

(*Checks if an item is in its treasure room*)
let rec checkIfItemInTreasureRoom (items : item list)(item : item) : bool= 
	match items with
	| [] -> false
	| h::t -> if (h.id = item.id) then true else checkIfItemInTreasureRoom t item
;;

let rec stringListToString (strings : string list) : string =
	match strings with
	| [] -> ""
	| h::t -> if (t = []) then h else h ^ " " ^ (stringListToString t)
;;
(*
Command Parse Engine
All of these functions either return a non string result from a string or return a modified version of the inputted string
*)

(*Returns the first word of a string*)
let firstWord (str : string) : string =
	if str = "" then
		""
	else
		String.lowercase_ascii (List.hd (Str.split (Str.regexp " ") str))
;;

(*Returns the second word in a string*)
let rec secondWord (str : string) : string = 
	if str = "" then
		""
	else
		String.lowercase_ascii (stringListToString (List.tl (Str.split (Str.regexp " ") str)))
;;

(*Turns a string into a direction*)
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

(*Returns an item from a string which is the name of the item you are looking to return*)
let rec itemFromCommand (command : string)(items : item list) : item option =
	match items with
	| [] -> None
	| h::t -> if ((String.lowercase_ascii h.name) = (String.lowercase_ascii command)) then Some h else (itemFromCommand command t)
;;

(*Game initialization. Asks if the player wants to play. If player says yes, this triggers the game function with the startingState defined above.*)
let rec interface (state : currentPlayerState) =
	print_string "\nStart Game? (Yes or No)\n>";
	let input = read_line() in
		if (String.lowercase_ascii input = "no") || (String.lowercase_ascii input = "n") then begin
			print_string "\nOkay. Goodbye!\n";
			game state "quit"
		end
		else if (String.lowercase_ascii input = "yes") || (String.lowercase_ascii input = "y") then begin
			print_string instructions;
			Printf.printf "\nThe maximum possible score is %i points.\n" (getMaxScore state.rooms state.items); (*Prints maximum score*)
			Printf.printf "\nYou are now in %s.\n\n%s\n\n" ((getRoomFromId state.currentRoom state.rooms).name) (getRoomFromId state.currentRoom state.rooms).description; (*Prints current room and current room description*)
			Printf.printf "There are exits in these directions:\n   %s\n\n" (print_exits (getRoomFromId state.currentRoom state.rooms).exits); (*Prints all room exits*)
			game ({state with visitedRooms = addRoom state.startingRoom state.visitedRooms}) "Launch" (*Adds the starting room to visited rooms and launches the main game function*)
		end
		else
			print_string "\nSorry, that is not a valid command. Please restrict your answer to 'Yes' or 'No'.\n";
			interface state (*Tries again*)

(*
Game Engine
*)
and game (state : currentPlayerState)(command : string) =
	if (command = "Launch") then begin (*This is only ever called once from interface*)
		if ((getRoomFromId state.currentRoom state.rooms).items = []) then begin
			print_string "There are no items in this room\n\n"; (*Prints if there are no items in the room*)
		end
		else
			Printf.printf "These are the items in this room:\n  %s\n\n" (print_items (getRoomFromId state.currentRoom state.rooms).items);  (*Prints items in the room*)
			(print_string "What is your next move?\n>");
			(game ({state with visitedRooms = addRoom state.currentRoom state.visitedRooms }) (read_line())) (*Runs game again with current room added to visited rooms and using player's command*)
	end
	else if ((firstWord (String.lowercase_ascii command) = "go")) then begin (*This is run if the player types go. This takes the player to his or her requested room if it exists*)
		if (findExitExistence (getRoomFromId state.currentRoom state.rooms).exits (commandToDirection (secondWord command))) then begin (*Checks if there is an exit in the player's requested direction*)
			if ((checkIfRoomVisited ((retrieveNextRoom (getRoomFromId state.currentRoom state.rooms).exits (commandToDirection (secondWord command))) state) state.visitedRooms) = true) then (*Checks if the room has been visited. If it has, room score is not added to the player's score. If it hasn't, room score is added to the player's score.*)
				game ({state with currentRoom = (retrieveNextRoom (getRoomFromId state.currentRoom state.rooms).exits (commandToDirection (secondWord command)) state).id; turns = state.turns + 1;}) "" (*Runs game with modified state. In this modified state, currentRoom has been changed and 1 has been added to the player's turns*)
			else if ((checkIfRoomVisited ((retrieveNextRoom (getRoomFromId state.currentRoom state.rooms).exits (commandToDirection (secondWord command))) state) state.visitedRooms) = false) then (*Checks if the room has been visited. If it has, room score is not added to the player's score. If it hasn't, room score is added to the player's score.*)
				game ({state with currentRoom = (retrieveNextRoom (getRoomFromId state.currentRoom state.rooms).exits (commandToDirection (secondWord command)) state).id; turns = state.turns + 1; score = (updateScore (state) (retrieveNextRoom (getRoomFromId state.currentRoom state.rooms).exits (commandToDirection (secondWord command)) state))}) "" (*Same but score from this room is added to player's score*)
		end
		else (*This is returned if there is no exit in the player's requested direction*)
			print_string "\nThere is no exit in that direction.\n\n";
			game state ""
	end
	else if ((firstWord (String.lowercase_ascii command) = "take")) then begin (*This is run if the player uses the take command*)
		if (((String.lowercase_ascii command) = "take" || (firstWord (String.lowercase_ascii command) = "take "))) then begin
			print_string "\nPlease specify which item you would like to take.\n\n"; (*If the player doesn't specify an item, he or she is prompted to do so*)
			game state "";
		end
		else begin
			if ((getRoomFromId state.currentRoom state.rooms).items = []) then begin
				print_string "\nSilly you, there are no items in this room! You can't pick up something that doesn't exist ;)\n\n";
				game state ""; (*If there are no items in the room, the player is informed*)
			end
			else
				if ((checkItemExistence (getRoomFromId state.currentRoom state.rooms).items (String.lowercase_ascii (secondWord command))) = true) then begin (*Check to see if the item the player requested to take exists*)
					Printf.printf "\nYou just picked up: %s.\n\n" (String.capitalize_ascii (secondWord command)); (*Informs the palyer of what he or she picked up*)
					match (itemFromCommand (secondWord command) ((getRoomFromId state.currentRoom state.rooms)).items) with (*Checks to see if item is None or Some*)
					| None -> print_string "\nThat item does not exist in this room.\n"; (game (state) ("")) 
					| Some item -> if (checkIfItemInTreasureRoom (getRoomFromId state.currentRoom state.rooms).treasure item) = false then begin (*If the room the item is currently in is not the treasure room, the player is informed of where this item should be dropped*)
										Printf.printf "%s\n\n" item.description; (*Print item description*)
										Printf.printf "Drop this item in %s to recieve the points it is worth.\n\n" ((getItemTreasureRoom state.rooms item).name);
										(game {state with inventory = addItemList item state.inventory; rooms = removeItem item state; turns = state.turns + 1} ""); (*Game is run with the item added to the player's inventory and removed from the room*)
									end
									else begin (*If the item is in its treasure room, the player is informed that n points will be detracted from his or her score*)
										Printf.printf "%s\n\n" item.description; (*Print item description*)
										Printf.printf "Picking this item up from this room will detract %i points from your score. This item should be dropped in this room if you want to recieve the points it is worth.\n\n" (item.points);
										(game {state with inventory = addItemList item state.inventory; rooms = removeItem item state; turns = state.turns + 1; score = state.score - item.points} ""); (*Game is run with the item added to inventory and removed from room*)
									end
				end
				else begin (*Player is informed if the requested item does not exist in the current room*)
					print_string "\nThat item does not exist in this room.\n\n";
					game state "";
				end
		end
	end
	else if ((firstWord (String.lowercase_ascii command) = "drop")) then begin (*This is run if the player types the run command*)
		if (String.lowercase_ascii command = "drop" || String.lowercase_ascii command = "drop") then begin
			print_string "\nPlease specify which item you would like to drop.\n\n"; (*Player is notified if he or she doesn't specify the item he or she would like to drop*)
			game state "";
		end
		else begin
			if (state.inventory = []) then begin (*Player is notified if there is nothing to drop*)
				print_string "\nSilly you, your inventory is empty! There is nothing to drop ;)\n\n";
				game state "";
			end
			else
				if ((checkItemExistence state.inventory (String.lowercase_ascii (secondWord command))) = true) then begin (*If the item exists in the player's inventory, the following is run*)
					Printf.printf "\nYou just dropped: %s.\n\n" (String.capitalize_ascii (secondWord command)); (*Player is notified of the item he or she dropped*)
					match (itemFromCommand (secondWord command) (state.inventory)) with (*Check to make sure we have Some not None*)
					| None -> print_string "\nYou don't have an item by that name in your inventory.\n"; (game (state) (""))
					| Some item -> if (checkIfItemInTreasureRoom (getRoomFromId state.currentRoom state.rooms).treasure item) = false then begin (*Check if the item is being dropped in its treasure room*)
										Printf.printf "Dropping this item here will not add any points to your score. This item should to be dropped in the %s if you want to receive the points it is worth.\n\n" (String.lowercase_ascii (getItemTreasureRoom state.rooms item).name); (*Player is notified if he or she is dropping the item in the wrong room*)
										(game {state with inventory = removeItemList item state.inventory; rooms = addItem item state; turns = state.turns + 1} ""); (*Game is run with the item removed from the inventory and added to the room*)
									end
									else begin
										Printf.printf "By dropping this item in the correct room, you have gained %i points!\n\n" item.points; (*Player is notified that they have dropped the item in the correct room*)
										(game {state with inventory = removeItemList item state.inventory; rooms = addItem item state; turns = state.turns + 1; score = state.score + item.points} ""); (*Game is run with item removed from inventory, added to room, and item points added to player score*)
									end
				end
				else begin (*Player is informed if the item does not exist in this room*)
				print_string "\nThat item does not exist in this room.\n\n";
				game state "";
				end
		end
	end 
	else if ((firstWord (String.lowercase_ascii command) = "quit")) then begin (*If the player wants to quit, he or she is prompted to use the terminal kill shortcut*)
		print_string "\nPress control-c to close the game";
		game state (read_line()); (*read_line() only to prevent anything else from being run*)
	end	
	else if ((firstWord (String.lowercase_ascii command) = "look")) then begin (*Run if player types command look*)
		Printf.printf "\nYou are now in %s.\n\n%s\n\n" ((getRoomFromId state.currentRoom state.rooms).name) ((getRoomFromId state.currentRoom state.rooms).description); (*Room name and description are printed*)
		Printf.printf "There are exits in these directions:\n  %s\n\n" (print_exits (getRoomFromId state.currentRoom state.rooms).exits); (*Room exists are printed*)
		if ((getRoomFromId state.currentRoom state.rooms).items = []) then begin
			print_string "There are no items in this room\n\n"; (*Printed if there are no items in the room*)
		end
		else
			Printf.printf "These items are in this room:\n  %s\n\n" (print_items (getRoomFromId state.currentRoom state.rooms).items); (*Items in the room are printed*)
		game state "";
	end
	else if ((firstWord (String.lowercase_ascii command) = "restart")) then begin (*Run if player types command restart*)
		print_string "\nAre you sure you want to restart the game? All of your progress will be reset.\n\n";
		let input = read_line() in
			if ((String.lowercase_ascii input) = "yes" || (String.lowercase_ascii input) = "y") then (*Gets player confirmation for restart*)
				interface state (*Restarts the game from scratch*)
			else if ((String.lowercase_ascii input) = "no" || (String.lowercase_ascii input) = "n") then begin 
				print_newline();
				game state ""; (*Continues the game*)
			end
			else begin
				print_string "\nThat is not a valid command. Please restrict your input to \"Yes\" or \"No\".\n";
				game state "restart"; (*Calls restart again, hoping to get proper input from the user*)
			end
	end
	else if ((firstWord (String.lowercase_ascii command) = "inv")) || ((firstWord (String.lowercase_ascii command) = "inventory")) then begin (*Run if inv or inventory is typed*)
		if (state.inventory = []) then begin
			print_string "\nYour inventory is empty.\n\n"; (*Informs player if his or her inventory is empty*)
			game state "";
		end
		else begin
			print_string "\n";
			(Printf.printf "These are the items in your inventory:\n  %s\n\n" (print_items state.inventory)); (*Prints out items in the player's inventory*)
			game state "";
		end
	end
	else if ((String.lowercase_ascii (firstWord command)) = "help") then begin (*Prints out game instructions defined at top*)
		print_string instructions;
		game state "";
	end
	else if ((String.lowercase_ascii (firstWord command)) = "score") then begin (*Prints out the current score*)
		Printf.printf "\nYour score is %i.\n\n" state.score;
		Printf.printf "The maximum possible score is %i.\n\n" (getMaxScore state.rooms state.items);
		game state "";
	end
	else if ((String.lowercase_ascii (firstWord command)) = "turns") then begin (*Prints out the amount of used turns*)
		Printf.printf "\nYou have used %i turns.\n\n" state.turns;
		game state "";
	end
	else if ((firstWord command) = "") then begin (*Standard case run by every call of Game. Never called by player*)
		if (state.score != getMaxScore state.rooms state.items) then begin (*Checks if the maximum score has been reached*)
			if ((checkIfRoomVisited (getRoomFromId state.currentRoom state.rooms) state.visitedRooms) = true) then begin (*Checks if room visited*)
				(print_string "What is your next move?\n>"); (*Asks player for next move and runs game with the player's input*)
				game (state) (read_line());
			end
			else (*If the room has not been visited, all of its relevant details such as name, description, items, and exits are printed*)
				Printf.printf "\nYou are now in %s.\n\n%s\n\n" ((getRoomFromId state.currentRoom state.rooms).name) ((getRoomFromId state.currentRoom state.rooms).description);
				Printf.printf "There are exits in these directions:\n  %s\n\n" (print_exits (getRoomFromId state.currentRoom state.rooms).exits);
				if ((getRoomFromId state.currentRoom state.rooms).items = []) then begin 
					print_string "There are no items in this room\n\n";
					print_string "What is your next move?\n>";
					game ({state with visitedRooms = addRoom state.currentRoom state.visitedRooms}) (read_line()) (*Runs game with a modified state in which this romo has been visited*)
				end
				else
					Printf.printf "These are the items in this room:\n  %s\n\n" (print_items (getRoomFromId state.currentRoom state.rooms).items); 
					print_string "What is your next move?\n>";
					game ({state with visitedRooms = addRoom state.currentRoom state.visitedRooms}) (read_line()) (*Runs game with a modified state in which this romo has been visited*)
		end
		else begin (*This is run if maximum score has been reached*)
			print_string "Congratulations! You have achieved the maximum possible score. Type \"again\" to play again or \"quit\" to close the game.\n";
			let input = read_line() in
				if (input = "again") then
					interface startingState (*If the player wants to play again, interface is called with the starting state*)
				else if (input = "quit") then begin
					print_string "\nPress control-c to close the game\n"; (*If the player wants to quit, he or she is told how to do so using terminal keyboard shortcuts*)
					game state (read_line())
				end
				else begin
					print_string "\nSorry, that is not a valid command. Please restrict your command to \"again\" or \"quit\".\n";
					game state ""
				end
		end
	end
	else (*If none of the above circumstances are met, the player is informed that the command he or she entered is not valid*)
		print_string "\nThat is not a valid command. Type \"help\" if you're stuck.\n\n";
		(print_string "What is your next move?\n>");
		game (state) (read_line());
;;


interface startingState (*Calling the interface function. This starts the game with the initial state startingState*)

(*
To-Do

Empty :)

*)












