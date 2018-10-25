module Cubic_Spline :
  sig
		val gaussJ_solve : float array array -> float array
		val getNaturalKS : float array array -> float array
		val evalSpline : float -> float array array -> float array -> float
  end =

  struct
		(* Solve a linear equation system with Gauss-Jordan Method *)
		let gaussJ_solve mat =
			(* Swap matrix rows *)
			let swap i j =
				let tmp = mat.(i) in
				mat.(i) <- mat.(j);
				mat.(j) <- tmp in
			(* Get matrix sizes *)
			let n = Array.length mat in
			(* Returned vector *)
			let ret = Array.make n 0. in
			(* Act on columns *)
			for k = 0 to (n-1) do
				(* Pivot column *)
				let i_max = ref 0 in
				let vali = ref min_float in
				for i = k to (n-1) do
					if (abs_float mat.(i).(k)) > !vali then
						i_max := i;
						vali := (abs_float mat.(i).(k));
				done;
				swap k !i_max;
				(* Apply changes to all the rows before pivot *)
				for i = (k+1) to (n-1) do
					let cf = mat.(i).(k) /. mat.(k).(k) in
					for j = k to n do
						mat.(i).(j) <- (mat.(i).(j) -. mat.(i).(j) *. cf);
					done;
				done;
			done;
			(* Act on rows *)
			for i = (n-1) downto 0 do
				let v = mat.(i).(n) /. mat.(i).(i) in
				ret.(i) <- v;
				for j = (i-1) downto 0 do
						mat.(j).(n) <- (mat.(j).(i) -. mat.(j).(i) *. v);
						mat.(j).(i) <- 0.
				done;
			done;
			(* Return the vector *)
			ret;;

		(* Get the parameters of the cubic spline *)
		let getNaturalKS points =
			let n = (Array.length points)-1 in
			let mat = Array.make_matrix (n+1) (n+2) 0. in
			(* Make the code clearer *)
			let x i = points.(i).(0) in
			let y i = points.(i).(1) in
			(* Build the linear equation matrix *)
			for i = 1 to (n-1) do
				mat.(i).(i-1) <- 1. /. ((x i) -. (x (i-1)));
				mat.(i).(i) <- 2.*. ((1. /. ((x i) -. (x (i-1)))) +. (1. /. ((x (i+1)) -. (x i))));
				mat.(i).(i+1) <- 1. /. ((x (i+1)) -. (x i));
				mat.(i).(n+1) <- 3.*.( ((y i)-.(y (i-1)))/.(((x i) -. (x (i-1)))*.((x i) -. (x (i-1))))  +.  ((y (i+1))-.(y i))/. (((x (i+1)) -. (x i))*.((x (i+1)) -. (x i))) );
			done;

			(* Initialisation values *)
			mat.(0).(0) <- 2. /. ((x 1) -. (x 0));
			mat.(0).(1) <- 1. /. ((x 1) -. (x 0));
			mat.(0).(n+1) <- 3. *.  ((y 1) -. (y 0)) /. (((x 1)-.(x 0)) *. ((x 1)-.(x 0)));

			(* Set last point as first *)
			mat.(n).(n-1) <- 1. /. ((x n) -. (x (n-1)));
			mat.(n).(n) <- 2. /. ((x n) -. (x (n-1)));
			mat.(n).(n+1) <- 3. *. ((y n) -. (y (n-1))) /. (((x n)-.(x (n-1))) *. ((x n)-.(x (n-1))));

			(* Solve it ! *)
			gaussJ_solve mat;;


		(* Evaluate the spline *)
		let evalSpline x_t points ks =
			let i = ref 1 in
			(* Make the code clearer *)
			let x a = points.(a).(0) in
			let y a = points.(a).(1) in
			(* Find the right interval *)
			while ((x !i) < x_t) do i := !i + 1; done; 
			(* Compute a,b,t,q *)
			let t = (x_t -. (x (!i-1))) /. ((x !i) -. (x (!i-1))) in
			let a = ks.(!i-1)*.((x !i)-.(x (!i-1))) -. ((y !i)-.(y (!i-1))) in
			let b = (-1.) *. ks.(!i) *. ((x !i)-.(x (!i-1))) +. ((y !i)-.(y (!i-1))) in
			let q = (1.-.t)*.(y (!i-1)) +. t*.(y !i) +. t*.(1.-.t)*.(a*.(1.-.t)+.b*.t) in
			q;;
	end
























