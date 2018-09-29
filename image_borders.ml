open Image_magick;;

(* Graphic Library *)
#load "graphics.cma";;

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
	if not ((h,w) =  getHW b) then failwith "applyFunctMatrix_d: Not same size";
	let ret = Array.make_matrix h w (f a.(0).(0) b.(0).(0)) in
	for i = 0 to (h-1) do
		for j = 0 to (w-1) do
			ret.(i).(j) <- f a.(i).(j) b.(i).(j);
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
