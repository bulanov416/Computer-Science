let id       = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";;
let rotor1   = "EKMFLGDQVZNTOWYHXUSPAIBRCJ";;
let rotor2   = "AJDKSIRUXBLHWTMCQGZNPYFVOE";;
let rotor3   = "BDFHJLCPRTXVZNYEIWGAKMUSQO";;
let reflB    = "YRUHQSLDPXNGOKMIEBFZCWVJAT";;

let rotors = [rotor1; rotor2; rotor3];;
let notches = ['Q';'E';'V'];;
let starts = ['F';'U';'N'];;

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
(* [simulate] takes the same inputs as [cipher] but prints
* a simulation of the Enigma machine at each step of the
* computation.
*)
let simulate (refl:string) (rotors:string list) (notches:char list)
             (starts:char list) (s:string) : unit
=
  print_string "Unimplemented\n";;

let rotateRotor (rotor: string) : string =
	String.sub (String.sub rotor 25 1 ^ rotor) 0 26;;

let rotorToStartPosition (rotor: string)(start: char) : string =
	let i = String.index rotor start in
	String.sub rotor i (String.length rotor - i) ^ String.sub rotor 0 i;; (* For 'abcde', and starting character b, i = 1. First sub is 'a', second sub is 'bcde'*)

let rec cipher (refl:string) (rotors:string list) (notches:char list) 
               (starts:char list) (s:string) : string =
               let c = s.[0] in 
               
  "";;
        
(*

#1 Decode letter to index in alphebet
#2 Access index of letter in next rotor / reflector

___________________________________________________

#1 Pass letter through rotors
#2 Turn rotors
#3

*)