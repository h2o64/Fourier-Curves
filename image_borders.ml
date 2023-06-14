open Image_magick;;

(* Graphic Library *)
#load "graphics.cma";;

(* Set structure *)
module S = Set.Make(struct type t = (float array) let compare = compare end);;

module Image_Borders :
  sig
		val edge1_kernel : float array array
		val edge2_kernel : float array array
		val edge3_kernel : float array array
		val sharpen_kernel : float array array
		val sobel_hX : float array array
		val sobel_hY : float array array
		val getHeight : 'a array -> int
		val getWidth : 'a array array -> int
		val getHW : 'a array array -> int * int
		val matrixApply : ('a -> 'b) -> 'a array array -> 'b array array
		val matrixApply_d : ('a -> 'b -> 'c) -> 'a array array -> 'b array array -> 'c array array
		val matrixApply : ('a -> 'b) -> 'a array array -> 'b array array
		val import_image : string -> Graphics.color array array
		val color_of_rgbint : Graphics.color -> int * int * int
		val greyscale_of_rgb : Graphics.color -> float
		val rgb_of_greyscale : float -> int
		val imageToGreyScale : Graphics.color array array -> float array array
		val bwimageToImage : float array array -> int array array
		val import_image_as_matrix : string -> float array array
		val getFormat : int -> int -> string
		val displayAnyMatrix : float array array -> unit
		val convolve : int -> int -> float array array -> float array array -> float
		val convolve_matrix : float array array -> float array array -> float array array
		val normalisation : float array array -> float array array
		val align_matrix : float array array -> float array array
		val sobel_magn : float array array -> float array array
		val binarize : float array array -> float -> int array array
		val img_mvt : int array array -> int array array -> unit
		val areThereNonZeros_aux : int array array -> int -> bool -> bool
		val areThereNonZeros : int array array -> bool
		val absDiff : int array array -> int array array -> int array array
		val p : 'a array array -> int -> int -> int -> 'a
		val one_thining_guohall : int array array -> int -> bool
		val thinning : 'a array -> ('a array -> int -> bool) -> 'a array
		val getPoints : int array array -> float array array
		val displayBin : int array array -> unit
		val image_to_kdt : Graphics.color array array -> float -> float array array * float KDTrees.tree
		val pointsToLines : S.elt array * float KDTrees.tree -> int -> S.elt array array
  end =

  struct

		(* Couple convolution kernels *)

		let edge1_kernel = [|
			[|1.;0.;-1.|];
			[|0.;0.;0.|];
			[|-1.;0.;1.|];
			|];;
		let edge2_kernel = [|
			[|0.;1.;0.|];
			[|1.;-4.;1.|];
			[|0.;1.;0.|];
			|];;
		let edge3_kernel = [|
			[|-1.;-1.;-1.|];
			[|-1.;8.;-1.|];
			[|-1.;-1.;-1.|];
			|];;
		let sharpen_kernel = [|
			[|0.;-1.;0.|];
			[|-1.;5.;-1.|];
			[|0.;5.;-1.|];
			|];;
		let sobel_hX = [|
			[|-1.;-2.;-1.|];
			[|0.;0.;0.|];
			[|1.;2.;1.|]
			|];;
		let sobel_hY = [|
			[|-1.;0.;1.|];
			[|-2.;0.;2.|];
			[|-1.;0.;1.|]
			|];;

		(* Get height and width of an image *)
		let getHeight img = Array.length img;;
		let getWidth img = Array.length img.(0);;
		let getHW m = (getHeight m,getWidth m);;

		(* Apply a function to each element of a matrix *)
		let matrixApply f matrix =
			let (h,w) = getHW matrix in
			let ret = Array.make_matrix h w (f matrix.(0).(0)) in
			for i = 0 to (h- 1) do
				for j = 0 to (w - 1) do
					ret.(i).(j) <- f (matrix.(i).(j))
				done;
			done;ret;;

		(* Apply function on matrix *)
		let matrixApply_d f a b =
			let (h,w) = getHW a in
			if not ((h,w) =  getHW b) then failwith "matrixApply_d: Not same size";
			let ret = Array.make_matrix h w (f a.(0).(0) b.(0).(0)) in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					ret.(i).(j) <- f a.(i).(j) b.(i).(j);
				done;
			done;ret;;

		(* Apply function on matrix *)
		let matrixApply f m =
			let (h,w) = getHW m in
			let ret = Array.make_matrix h w (f m.(0).(0)) in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					ret.(i).(j) <- f m.(i).(j);
				done;
			done;ret;;

		(* Import an image *)
		let import_image file = Image_magick.lire_image file;;

		(* Convert RGB integer to color type *)
		let color_of_rgbint (num : Graphics.color) =
			(* Red/Green/Blue *)
			let b = num mod 256 in
			let g = (num/256) mod 256 in
			let r = (num/256/256) mod 256 in
			(r,g,b);;

		(* Convert RGB to greyscale *)
		let greyscale_of_rgb pix =
			let (r,g,b) = color_of_rgbint pix in
			(float_of_int (r + g + b))/.3.;;

		(* Convert greyscale to RGB *)
		let rgb_of_greyscale pix = (int_of_float pix) * 0x00010101;;

		(* Convert whole image to greyscale *)
		let imageToGreyScale image = (matrixApply greyscale_of_rgb image);;

		(* Convert whole greyscale image to RGB image *)
		let bwimageToImage image = (matrixApply rgb_of_greyscale image);;

		(* Import image as matrix *)
		let import_image_as_matrix file =
			let img = Image_magick.lire_image file in
			let bw_img = imageToGreyScale img in
			bw_img;;

		(* Get the right image format *)
		let getFormat height width =
			let s_height = string_of_int height in
			let s_width = string_of_int width in
			String.concat "" [" ";s_height;"x";s_width];;

		(* Display any matrix *)
		let displayAnyMatrix matrix =
			let (h,w) = getHW matrix in
			let last = matrixApply rgb_of_greyscale matrix in
			Graphics.open_graph (getFormat w h);
			Image_magick.dessiner_image last;;

		(* Do convolution on only one pixel *)
		let convolve i j kernel image_matrix =
			let tmp = ref 0. in
			let r = Array.length kernel in (* Kernel is square *)
			let (h,w) = getHW image_matrix in
			for m = 0 to (r - 1) do
				for n = 0 to (r - 1) do
					(* Use zero-padding to extend the image *)
					let (a,b) = ((i + m - (r/2)),(j + n - (r/2))) in
					if not((a < 0) || (b < 0) || (a > (h-1)) || (b > (w-1))) then
						tmp := !tmp +.(kernel.(m).(n)*.image_matrix.(a).(b))
				done;
			done;
			!tmp;;

		(* Convolve whole matrix *)
		let convolve_matrix kernel m =
				let (h,w) = getHW m in
				let ret = Array.make_matrix h w 0. in
				for i = 0 to (h - 1) do
					for j = 0 to (w - 1) do
						ret.(i).(j) <- (convolve i j kernel m)
					done;
				done;
				ret;;

		(* Image normalisation with histogram equalization *)
		let normalisation m =
			let (h,w) = getHW m in
			(* Get image's histogramm *)
			let occurs = Array.make 256 0 in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					let value = occurs.(int_of_float (m.(i).(j))) in
					occurs.(int_of_float (m.(i).(j))) <- value + 1;
				done;
			done;
			(* Get the transformation *)
			let transf = Array.make 256 0. in
			let size = float_of_int (h*w) in
			let tmp = (255. /. size) in
			for i = 0 to 255 do
				let sum = ref 0 in
				for j = 0 to i do
					sum := !sum + occurs.(j)
				done;
				transf.(i) <- tmp *. (float_of_int !sum)
			done;
			(* Transform the image *)
			let ret = Array.make_matrix h w 0. in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					ret.(i).(j) <- transf.(int_of_float m.(i).(j))
				done;
			done;
			ret;;

		(* Align matrix values to 255 *)
		let align_matrix m =
			(* Get max value of a matrix *)
			let get_array_max tab =
				let ret = ref tab.(0) in
				for i = 1 to ((Array.length tab)-1) do
					if tab.(i) > !ret then ret := tab.(i);
				done;!ret in
			let get_matrix_max m =
				let ret = ref (get_array_max m.(0)) in
				for i = 1 to ((Array.length m)-1) do
					let tmp = (get_array_max m.(i)) in
					if tmp > !ret then ret := tmp;
				done;!ret in
			(* Alignement *)
			let (h,w) = getHW m in
			let matrix_max = get_matrix_max m in
			let convert num = (num *. 255.) /. matrix_max in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					m.(i).(j) <- convert m.(i).(j);
				done;
			done;m;;

		(* Sobel segmentation *)
		let sobel_magn m =
			let x_cv = convolve_matrix sobel_hX m in
			let y_cv = convolve_matrix sobel_hY m in
			let f x y = x**2. +. y**2. in
			matrixApply_d f x_cv y_cv;;

		(* Binarize the image *)
		let binarize m threshold =
			let (h,w) = getHW m in
			let ret = Array.make_matrix h w 0 in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					if (m.(i).(j) > threshold) then ret.(i).(j) <- 1;
				done;
			done;ret;;

		(* Image difference *)
		(* A &= ~B in CCP *)
		let img_mvt a b =
			let (h,w) = getHW a in
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					a.(i).(j) <- ((land) a.(i).(j) (lnot b.(i).(j)));
				done;
			done;;

		(* Are there zeros ? *)
		let rec areThereNonZeros_aux m i b =
			if i < 0 then false
			else if (b = true) then true
			else (areThereNonZeros_aux m (i-1) (Array.mem 1 m.(i)));;
		let areThereNonZeros m = areThereNonZeros_aux m ((Array.length m)-1) false;;

		(* Get abs difference between two matrix *)
		let absDiff a b =
			let f a b = abs (b-a) in
			matrixApply_d f a b;;

		(* Guo-Hall thinning algorithm - 1987 *)
		(* Get 8-neighborhood bool array *)
		let p matrix i j num =
			if num = 9 then matrix.(i-1).(j-1)
			else if num = 8 then matrix.(i).(j-1)
			else if num = 7 then matrix.(i+1).(j-1)
			else if num = 6 then matrix.(i+1).(j)
			else if num = 5 then matrix.(i+1).(j+1)
			else if num = 4 then matrix.(i).(j+1)
			else if num = 3 then matrix.(i-1).(j+1)
			else if num = 2 then matrix.(i-1).(j)
			else matrix.(i).(j) (* Fallback *) ;;

		(* One thining iteration *)
		let one_thining_guohall m iter =
			(* Prepare matrix *)
			let (h,w) = getHW m in
			let marker = Array.make_matrix h w 0 in
			let m_bak = Array.copy m in
			let deleting = ref false in
			(* Actual loop *)
			for i = 2 to (h-2) do
				for j = 2 to (w-2) do
					(* Get values *)
					let p_cur num = p m i j num in
					let p2 = (p_cur 2) in
					let p3 = (p_cur 3) in
					let p4 = (p_cur 4) in
					let p5 = (p_cur 5) in
					let p6 = (p_cur 6) in
					let p7 = (p_cur 7) in
					let p8 = (p_cur 8) in
					let p9 = (p_cur 9) in
					(* Conditions *)
					let c  = ((land) (lnot p2) ((lor) p3 p4)) + ((land) (lnot p4) ((lor) p5 p6)) +
									 ((land) (lnot p6) ((lor) p7 p8)) + ((land) (lnot p8) ((lor) p9 p2)) in
					let n1 = ((lor) p9 p2) + ((lor) p3 p4) + ((lor) p5 p6) + ((lor) p7 p8) in
					let n2 = ((lor) p2 p3) + ((lor) p4 p5) + ((lor) p6 p7) + ((lor) p8 p9) in
					let n  = if n1 < n2 then n1 else n2 in
					let m_c  = if (iter = 0) then
							((land) ((lor) ((lor) p6 p7) (lnot p9)) p8)
						else
							((land) ((lor) ((lor) p2 p3) (lnot p5)) p4) in
					(* Check *)
					if (c = 1 && (n >= 2 && n <= 3) && m_c = 0) then
						marker.(i).(j) <- 1;
				done;
			done;
			img_mvt m marker;
			deleting := areThereNonZeros (absDiff m m_bak);
			!deleting;;

	(* Zhangsuen Method *)
	let one_thining_zhangsuen m iter =
		(* Prepare matrix *)
		let (h,w) = getHW m in
		let marker = Array.make_matrix h w 0 in
		let m_bak = Array.copy m in
		let deleting = ref false in
		let bool2bin value =
			if value then 1
			else 0 in
		(* Actual loop *)
		for i = 2 to (h-2) do
			for j = 2 to (w-2) do
				(* Get values *)
				let p_cur num = p m i j num in
				let p2 = (p_cur 2) in
				let p3 = (p_cur 3) in
				let p4 = (p_cur 4) in
				let p5 = (p_cur 5) in
				let p6 = (p_cur 6) in
				let p7 = (p_cur 7) in
				let p8 = (p_cur 8) in
				let p9 = (p_cur 9) in
				(* Conditions *)
				let a = (bool2bin ((p2 = 0) && (p3 = 1))) + (bool2bin ((p3 = 0 && (p4 = 1)))) + 
								 (bool2bin ((p4 = 0) && (p5 = 1))) + (bool2bin ((p5 = 0 && (p6 = 1)))) + 
								 (bool2bin ((p6 = 0) && (p7 = 1))) + (bool2bin ((p7 = 0 && (p8 = 1)))) +
								 (bool2bin ((p8 = 0) && (p9 = 1))) + (bool2bin ((p9 = 0 && (p2 = 1)))) in
				let b = p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 in
				let m1 = if (iter = 0) then (p2 * p4 * p6) else (p2 * p4 * p8) in
				let m2 = if (iter = 0) then (p4 * p6 * p8) else (p2 * p6 * p8) in
				(* Check *)
				if ((a = 1) && ((b >= 2) && (b <= 6)) && (m1 = 0) && (m2 = 0)) then
					marker.(i).(j) <- 1;
			done;
		done;
		img_mvt m marker;
		deleting := areThereNonZeros (absDiff m m_bak);
		!deleting;;

		(* Actuall thinning part *)
		let thinning m methode =
			let cur_m = Array.copy m in
			(* Actual while - Add an iter check *)
			let isDeleting = ref true in
			while !isDeleting do
				isDeleting := methode cur_m 0;
				isDeleting := methode cur_m 1;
			done;cur_m;;

		(* Make the set of points after sobel *)
		let getPoints m =
			let (h,w) = getHW m in
			let cur = ref 0 in
			(* Find the size *)
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					if (m.(i).(j) = 1) then cur := !cur + 1;
				done;
			done;
			(* Make the array *)
			let ret = Array.make !cur [|0.;0.|] in
			cur := 0;
			for i = 0 to (h-1) do
				for j = 0 to (w-1) do
					if (m.(i).(j) = 1) then
						(ret.(!cur)<-[|(float_of_int i);(float_of_int j)|];
						cur := !cur + 1);
				done;
			done;ret;;

		(* Display a binary matrix *)
		let displayBin m =
			let f x =
				if x = 0 then 255.
				else 0. in
			displayAnyMatrix (matrixApply f m);;

		(* Transform an image to a kd-tree *)
		let image_to_kdt img threshold =
			let gr_img = imageToGreyScale img in
			let sobel_img = align_matrix (sobel_magn gr_img) in
			let bin_img = binarize sobel_img threshold in
			(* let thin_img = thinning bin_img one_thining_guohall in *)
		 	let pts_img = getPoints bin_img in
			displayBin bin_img;
			(pts_img,(KDTrees.constructKDT pts_img));;

		(* DEBUG *)
		let print_arr arr =
			let print_vect_d vect =
				print_string "[|";
				print_float vect.(0);
				print_string ";";
				print_float vect.(1);
				print_string "|]" in
			for i = 0 to (Array.length arr)-1 do
				print_vect_d arr.(i);
				print_string "\n";
			done;;

		(* Transform points to lines *)
		let pointsToLines (pointList,pointTree) neighborhoodSize =
			(* Neiborhood of any tree point *)
			let nf point = KDTrees.knns pointTree point neighborhoodSize KDTrees.float_tools 2 in
			(* Various initializers *)
			let num = Array.length pointList in
			let pointSet = ref (S.empty) in
			let count = ref 0 in
			(* Algorithm variables *)
			let current_point = ref [||] in
			let current_neighborhood = ref [||] in
			let current_segment = ref [] in
			let segment_bag = ref [] in
			let couldReverse = ref true in
			(* Vector operation *)
			let sub_v a b = (Array.map2 ( -. ) a b) in
			let add_v a b = (Array.map2 ( +. ) a b) in
			let mul_v a x = [|a.(0)*.x;a.(1)*.x|] in
			let dot_v a b = (a.(0)*.b.(0) +. a.(1)*.b.(1)) in
			let norm_v a = sqrt (dot_v a a) in
			(* Updaters to global variables *)
			let add_to_segment point =
				((* Draw points live *)
				Graphics.plot (int_of_float point.(1)) (470 - (int_of_float point.(0)));
				(* print_string ("\ni = "^(string_of_float point.(0)));
				print_string (" j = "^(string_of_float point.(1))); *)
				current_segment := point::!current_segment) in
			let add_to_bag () =
				(segment_bag := (Array.of_list !current_segment)::!segment_bag;
				current_segment := []) in
			let update_neighborhood () =
				let ret = ref [] in
				(* Get the neighbors *)
				let neigh = nf !current_point in
				let size = Array.length neigh in
				for i = 0 to (size-1) do
					let (_,cur) = neigh.(i) in
					if not(S.mem cur !pointSet) then ret := cur::!ret;
				done;(current_neighborhood :=
							(Array.of_list (List.rev !ret))); in
			let update_current_point point =
				current_point := point in
			let remove_from_points point =
				pointSet := S.add point !pointSet in
			let nth_of_seg n = List.nth !current_segment n in
			(* WARNING: THIS IS BAD AF *)
			let choose_rnd_point () =
				let array_rand ary =
					let len = Array.length ary in
					ary.(Random.int len) in
				let ret = ref (array_rand pointList) in
				while (S.mem !ret !pointSet) do
					ret := (array_rand pointList);
				done;!ret in
			(* Main routine *)
			let routine point =
				update_current_point point;
				add_to_segment !current_point;
				update_neighborhood ();
				remove_from_points !current_point;
				count := !count + 1 in



			(* DEBUG *)
			let debug case =
				let interesst = [|[|39.; 13.|]; [|40.; 12.|]; [|40.; 14.|]; [|39.; 12.|]; [|39.; 14.|];
  [|41.; 12.|]; [|40.; 11.|]; [|40.; 15.|]; [|39.; 15.|]; [|41.; 11.|];
  [|40.; 16.|]; [|39.; 16.|]; [|39.; 17.|]; [|40.; 18.|]; [|41.; 18.|];
  [|40.; 19.|]; [|41.; 19.|]; [|40.; 20.|]; [|40.; 22.|]; [|38.; 0.|];[|40.;13.|]|] in
				if (Array.mem !current_point interesst) then
					(print_string ("\nCase "^case);
					print_string "\nCurrent point : ";
					print_arr [|!current_point|];
					print_string "Current neighborhood : ";
					print_arr !current_neighborhood) in
			
			(* Cover all the points *)
			while (!count < num) do
				(* Re-initialise *)
				couldReverse := true;
				routine (choose_rnd_point ());
				let seg_length = ref !count in
				(* Build the current segment *)
				let (r,g,b) = ((Random.int 255),(Random.int 255),(Random.int 255)) in
				Graphics.set_color (Graphics.rgb r g b);
				while (((Array.length !current_neighborhood) > 0) || !couldReverse) do
					(* Restart from the other end of the segment *)
					if ((Array.length !current_neighborhood) = 0) then
						(debug "Empty Neighborhood";
						couldReverse := false;
						current_segment := List.rev !current_segment;
						update_current_point (List.hd !current_segment);
						update_neighborhood ())
					else
						(* Sleep *)
						((* Unix.sleepf 0.0001; *)
						(* Update the variables *)
						if true (* ((List.length !current_segment) < 4) ||
								((Array.length !current_neighborhood) = 1) *) then
							(debug "Classic Continuation";
							routine !current_neighborhood.(0))
						else
							(* Penalize sharp edges *)
							(let third = (nth_of_seg 3) in
							let sec = (nth_of_seg 2) in
							let first = (nth_of_seg 1) in
							let d_v = add_v (sub_v first sec) (mul_v (sub_v sec third) 0.5) in
							let d = mul_v d_v (1./.(norm_v d_v)) in
							let n = [|(-1.)*.d.(1);d.(0)|] in
							(* Find the best candidate *)
							let compare_v a b =
								(* Compute the distances *)
								let dist x =
									sqrt (((dot_v d (sub_v x first))**2.) +.
											 ((dot_v n (sub_v x first))**2.)) in
								let dist_a = dist a in
								let dist_b = dist b in
								if dist_a < dist_b then (-1)
								else if dist_b > dist_a then 1
								else 0 in
							(* Sort the nearest accordingly *)
							Array.fast_sort compare_v !current_neighborhood;
							(* Do the shorten routine for the two points *)
							add_to_segment !current_neighborhood.(0);
							remove_from_points !current_neighborhood.(0);
							count := !count + 1;
							routine !current_neighborhood.(1);););						
				done;
				debug "End segment";
				(* If the segment is shorter than 10 points *)
				if (!count - !seg_length) > 10 then
					(* Add the built segment to the bag *)
					add_to_bag ();
			done;(Array.of_list !segment_bag);;

	end
		let subanything ret_a k =
			let goal_length = if (Array.length ret_a) > k then k else (Array.length ret_a) in
			Array.sub ret_a 0 goal_length;;


		let clear_arr arr =
			let n = Array.length arr in
			let ret = Array.make n [||] in
			for i = 0 to (n-1) do
				let (_,tmp) = arr.(i) in
				ret.(i)<-tmp;
			done;ret;;
