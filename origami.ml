open List

type point = float * float
type kartka = point -> int
type znak = Dod | Zero | Uje

(****************)

let odl (a , b) (c , d) =	let (x , y) = (a -. c , b -. d) in
							sqrt(x *. x +. y *. y)

let wew_kolko (s , r) p = odl s p <= r

let wew_prostokat (x , y) p = 
		(fst x <= fst p && fst p <= fst y) && (snd x <= snd p && snd p <= snd y)

let wekt (a , b) (c , d) (x , y) = 
		let s = (c -. a) *. (y -. b) -. (x -. a) *. (d -. b) in
		if s > 0. then Dod else if s = 0. then Zero else Uje

let sym (a , b) (c , d) (x , y) =	(* Dostaje punkty A , B , X i zwraca odbicie punktu X wzgledem prostej AB *)
		if a = c then (2. *. a -. x , y)
		else
		let (af , bf) = (d -. b) /. (c -. a) , (b *. c -. d *. a) /. (c -. a) in	(* Równanie prostej AB *)
			if af = 0. then (x , 2. *. bf -. y) else
			let (ag , bg) = -1. /. af , y +. x /. af in	(* Równanie prostopadłej do AB przechodzącej przez X *)
				let p = ( (bg -. bf) /. (af -. ag) , (af *. bg -. ag *. bf) /. (af -. ag) ) in (* Rzut X na AB *)
				(2. *. fst p -. x , 2. *. snd p -. y) (* Odbicie punktu X względem rzutu na AB *)

(****************)

let prostokat a b : kartka = function p ->
			if wew_prostokat (a , b) p then 1
			else 0

let kolko s r : kartka = function p ->
			if wew_kolko (s , r) p then 1
			else 0


let zloz a b (k:kartka) : kartka = function p ->
			match wekt a b p with
			| Dod -> k p + k (sym a b p)
			| Zero -> k p
			| Uje -> 0

let skladaj l k = fold_left( fun k (a , b) -> zloz a b k ) k l;;
			
