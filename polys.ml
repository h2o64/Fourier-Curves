#include "polys.mli";;

let create_zero_list zero n =
	let rec create_zero_list_rec zero n l =
		match n with
		 | 0 -> l
		 | n -> create_zero_list_rec zero (n - 1) (zero::l)
	 in
	 create_zero_list_rec zero

module Poly : POLY =
	struct
		type 'a poly = 'a list
		
		let eval p sum_field prod_field zero_field one_field x =
			let eval_rec p sum_field prod_field zero_field x pow_x tot =
				match p with
					| [] -> tot
					| a::p -> eval_rec p sum_field prod_field zero_field x (prod_field x pow_x) (sum_field (prod_field a pow_x) tot)
			in
			eval_rec p sum_field prod_field zero_field x one_field zero_field
			
		let neg_infty = -1
			
		let degre p =
			let rec degre_rec p d =
				match p with
					| [] -> d
					| _::p -> degre_rec p (d + 1)
			in
			degre_rec p neg_infty
		
		let rec sum p q sum_field =
			match (p, q) with
				| ([], _) -> q
				| (_, []) -> p
				| (x::p, y::q) -> (sum_field x p)::(sum p q sum_field)
		
		let rec prod_elem p prod_field zero (c, i) =
			match p with
				| [] -> create_zero_list zero i
				| x::p -> (prod_field x c)::(prod_elem p prod_field zero (c, i))
	end
