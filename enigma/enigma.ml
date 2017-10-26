let id       = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";;
let rotor1   = "EKMFLGDQVZNTOWYHXUSPAIBRCJ";;
let rotor2   = "AJDKSIRUXBLHWTMCQGZNPYFVOE";;
let rotor3   = "BDFHJLCPRTXVZNYEIWGAKMUSQO";;
let reflB    = "YRUHQSLDPXNGOKMIEBFZCWVJAT";;

let rotors = [rotor1; rotor2; rotor3];;
let notches = ['Q';'E';'V'];;
let starts = ['F';'U';'N'];;

let inputString = "helloworld";;
          
let plugboard : char list list = [['A'; 'E']; ['C'; 'F']; ['D'; 'R']; ['L'; 'N']; ['P'; 'Z']; ['G'; 'U']; ['O'; 'W']; ['B'; 'Q']; ['H'; 'V']; ['T'; 'Y']; ['I'; 'S']; ['J'; 'X']; ['K'; 'K']; ['M'; 'M']];;

let rec plugTransform (plugboard : char list list) : string =
	match plugboard with
	|[] -> ""
	|h :: t -> ((Char.escaped (List.hd h)) ^ (Char.escaped (List.nth h 1))) ^ plugTransform t;;

plugTransform plugboard;;

(*let rec plugCipher (plugboard: char list list)(c: char) : char = 

;;

*)
(* [cipher refl rotors notches starts s] computes the Enigma cipher, where
*  - [refl] is the wiring of the reflector,
*  - [rotors] is a list of the rotors (which must contain at least
*      one element), as they are installed from left to right on 
 *      the spindle,
*  - [notches] is a list of where the notch is on each rotor,
*      where the nth character of [notches] is the location of
*      the notch on the nth rotor in [rotors],
*  - [starts] is a list of the starting character for each rotor
*      as positioned on the spindle, where the nth character of
*      [starts] is the starting character for the nth rotor in
*      [rotors].
*  - [s] is the string to be ciphered.
*)

let rotateRotor (rotor: string) : string =
	String.sub (String.sub rotor 25 1 ^ rotor) 0 26;; (*Takes the last character, and places it at the front*)

let rotorToStartPosition (rotor: string)(start: char) : string =
	let i = String.index rotor start in (*This finds where the starting position is on the rotor right now*)
	String.sub rotor i (String.length rotor - i) ^ String.sub rotor 0 i;; (*Takes the subset before the starting character, and everything after, and swaps them*)

let rec transform (c: char)(transforms: string list)(verbose: bool) : char =
	match transforms with
	| [] -> c (*If there are no more rotors or reflectors left to pass the character through, return the final result*)
	| h :: t -> 
		let i = String.index id c in (*Find the position in the alphabet of the character*)
		transform (String.get h i) t verbose;; (*Use the position to encipher the character and pass it to the next rotor*)

let rec rev_transform (c: char)(transforms: string list)(verbose: bool) : char =
	match transforms with
	| [] -> c
	| h :: t ->
		let i = String.index h c in
		let c1 = String.get id i in
		rev_transform c1 t verbose;;

let cipher_character (c: char)(refl: string)(rotors: string list)(verbose: bool)(plugboard: string) : char =
	if verbose then begin
		Printf.printf "Enciphering %c\n" c;
		List.iteri (Printf.printf "rotor%d: [%s]\n") rotors;
		Printf.printf "reflB:  [%s]\n\n" refl;
	end;
	let cPlug1 = String.get plugboard ((String.index plugboard c) + 1) in
	let c1 = transform cPlug1 (rotors @ [refl]) verbose in
	rev_transform c1 (List.rev rotors) verbose;;			

let rec set_new_starts (rotors: string list)(starts: char list)(counter: int) : char list =
	let _rotors = List.map2 rotorToStartPosition rotors starts in
	if counter = 0 then
		let _rotor = List.hd _rotors in (*Fetch first rotor*)
		let _start = String.get (rotateRotor _rotor) 0 in (*Turn the first rotor and store the starting value*)
		[_start] @ set_new_starts rotors starts 1 (*Move on to next rotor*)
	else if counter < 3 then (*If we are working on the last two rotors*)
		let prev_rotor = List.nth _rotors (counter - 1) in (*Fetch previous rotor*)
		let _rotor = List.nth _rotors counter in (*Fetch current rotor*)
		if String.get prev_rotor 0 = List.nth notches (counter - 1) then (*If the previous rotor is at its notch*)
			let _start = String.get (rotateRotor _rotor) 0 in (*Turn the rotor and store the starting value*)
			[_start] @ set_new_starts rotors starts (counter + 1) (*Move on to next rotor*)
		else
			let _start = String.get _rotor 0 in (*Keep rotor still and store starting value*)
			[_start] @ set_new_starts rotors starts (counter + 1) (*Move on to next rotor*)
	else
		[];; (*Return the starting positions after passing through all rotors*)

let rec main (refl: string)(rotors: string list)(notches: char list)(starts: char list)(s: string)(verbose: bool) : string =
	let _rotors = List.map2 rotorToStartPosition rotors starts in
	let c = String.get s 0 in (*Get the character to be ciphered*)
	let _c = cipher_character c refl _rotors verbose in (*Cipher the character*)
	let _starts = set_new_starts rotors starts 0 in (*Store new starting values for rotors*)
	(*if verbose then Printf.printf "Okay!\n";*)
	if String.length s > 1 then (*If the string contains more than one character*)
		(String.make 1 _c) ^ main refl rotors notches _starts (String.sub s 1 ((String.length s) - 1)) verbose (*Cipher next character with new positions*)
	else
		String.make 1 _c;; (*Only return the ciphered character, and don't iterate again*)

let rec cipher (refl:string) (rotors:string list) (notches:char list) (starts:char list) (s:string) : string =
	main refl rotors notches starts s false;;
        
(* [simulate] takes the same inputs as [cipher] but prints
* a simulation of the Enigma machine at each step of the
* computation.
*)

let simulate (refl:string) (rotors:string list) (notches:char list)(starts:char list) (s:string) : unit =
	Printf.printf "Input: \"%s\"\n\n" s;
	let _s = main refl rotors notches starts s true in
	Printf.printf "Output: \"%s\"\n" _s;;

let runString = String.uppercase_ascii inputString;;

simulate reflB rotors notches starts runString;; 


(*

#1 Decode letter to index in alphebet
#2 Access index of letter in next rotor / reflector

Do opposite for 

___________________________________________________

#1 Pass letter through rotors and reflectors, then back through rotors
#2 Turn rotors
#3

*)