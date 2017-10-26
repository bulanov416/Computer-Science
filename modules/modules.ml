module type Arith = sig
	type t
	val zero : t
	val one : t
	val plus : t -> t -> t
	val mult : t -> t -> t
	val neg : t -> t 
end


module Ints : Arith = struct
	type t  = int
	let zero = 0
	let one = 1	
	let plus s t = s + t
	let mult s t = s * t
	let neg s = s * -1
end

module Floats : Arith = struct
	type t = float
	let zero = 0.
	let one = 1.
	let zerof = 0.
	let onef = 1.
	let plus s t = s +. t
	let mult s t = s *. t
	let neg s = s *. -1.
	let dev s t = s /. t
end






