module Poly :
	sig
		type 'a poly 
		val eval : 'a poly -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'a -> 'a -> 'a
		val neg_infinity : int
		val degre : 'a poly -> int
		val sum : 'a list -> 'a list -> ('a -> 'a list -> 'a) -> 'a list
	end =

	struct
		(* Structure definition *)
		type 'a poly = 'a list;;

		(* Nul Polynomial *) 
		let create_zero_list zero n =
			let rec create_zero_list_rec zero n l =
				match n with
				 | 0 -> l
				 | n -> create_zero_list_rec zero (n - 1) (zero::l)
			 in
			 create_zero_list_rec zero;;


		(* Polynomial evaluation *)
		let eval p sum_field prod_field zero_field one_field x =
			let rec eval_rec p sum_field prod_field zero_field x pow_x tot =
				match p with
					| [] -> tot
					| a::p -> eval_rec p sum_field prod_field zero_field x
										(prod_field x pow_x) (sum_field (prod_field a pow_x) tot)
			in
			eval_rec p sum_field prod_field zero_field x one_field zero_field;;
			
		let neg_infinity = -1;;

		(* Polynomial degree *)
		let degre p =
			let rec degre_rec p d =
				match p with
					| [] -> d
					| _::p -> degre_rec p (d + 1)
			in
			degre_rec p neg_infinity;;

		(* Sum of polynomial *)
		let rec sum p q sum_field =
			match (p, q) with
				| ([], _) -> q
				| (_, []) -> p
				| (x::p, y::q) -> (sum_field x p)::(sum p q sum_field);;
	end
