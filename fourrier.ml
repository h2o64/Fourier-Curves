(* Rectangle Method for Integral Calculation *)
let rectangle f a b n =
	let ret = ref 0. in
	let p = (b -. a) /. (float_of_int n) in
	for i = 0 to (n-1) do
		ret := !ret +. (f (a +. (float_of_int i) *. p))
	done;(p *. !ret);;

(* Pi *)
let pi = 3.14159265358979312;;

(* Calculate Fourrier's Coefficients *)
let a0 f a b eps =
	let period = abs_float (a -. b) in
	(1. /. period) *. (rectangle f a b eps);;
let an f a b n eps =
	let period = abs_float (a -. b) in
	let w = (2. *. pi) /. period in
	let cos_f x = (f x) *. (cos ((float_of_int n)*.w*.x)) in
	(2. /. period) *. (rectangle cos_f a b eps);;
let bn f a b n eps =
	let period = abs_float (a -. b) in
	let w = (2. *. pi) /. period in
	let sin_f x = (f x) *. (sin ((float_of_int n)*.w*.x)) in
	(2. /. period) *. (rectangle sin_f a b eps);;

(* Get a fonction decomposition *)
let fourrier f a b order eps =
	(* Periodicity data *)
	let period = abs_float (a -. b) in
	let w = (2. *. pi) /. period in
	(* Print sinus and cosinus *)
	let print_sin x = print_string ("sin("^(string_of_float x)^"*x)") in
	let print_cos x = print_string ("cos("^(string_of_float x)^"*x)") in
	(* Print the decomposition *)
	print_string "f(x) = ";
	let tmp0 = (a0 f a b eps) in
	if tmp0 > 0.0000000000001 then (print_float tmp0);
	for i = 1 to order do
		let a_c =
			let tmp1 = (an f a b i eps) in
			if tmp1 < 0.0000000000001 then 0.
			else tmp1 in
		if not (a_c = 0.) then 
			(if a_c > 0. then print_string "+";
			print_float a_c;
			print_string "*";
			print_cos ((float_of_int i)*.w));
		let b_c =
			let tmp2 = (bn f a b i eps) in
			if tmp2 < 0.0000000000001 then 0.
			else tmp2 in
		if not (b_c = 0.) then
			(if b_c > 0. then print_string "+";
			print_float b_c;
			print_string "*";
			print_sin ((float_of_int i)*.w));
	done;;
















	
