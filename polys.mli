module type POLY =
	sig
		type 'a poly 
		
		val eval : 'a poly -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'a -> 'a -> 'a
		val neg_infty : int
		val degre : 'a poly -> int
		val sum_p : 'a poly -> 'a poly -> ('a -> 'a -> 'a) -> 'a poly
		val prod_elem : 'a poly -> ('a -> 'a -> 'a) -> 'a -> ('a * int) -> 'a poly
		(*val prod_p : 'a poly -> 'a poly -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a poly*)
		
	end
