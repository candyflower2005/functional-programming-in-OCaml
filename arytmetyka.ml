type przedzial = { l : float ; p : float };;
type wartosc = przedzial list;; (* Lista parami rozlacznych przedzialow posortowanych rosnaco *)

let max x y = if x > y then x else y;;
let min x y = if x < y then x else y;;

(* FUNKCJE DO MODYFIKATOROW *)

let suma a b c d = { l = a +. c ; p = b +. d };;
let roznica a b c d = { l = a -. d ; p = b -. c };;

let mnoz a b =
match ( a , b ) with
| ( 0. , infinity ) -> 0.
| ( infinity , 0. ) -> 0.
| ( 0. , neg_infinity ) -> 0.
| ( neg_infinity , 0. ) -> 0.
| ( _ , _ ) -> a *. b;;

let iloczyn a b c d = { l = min( min( mnoz b d ) ( mnoz a c ) ) ( min( mnoz a d ) ( mnoz b c ) ) ;
			p = max( max( mnoz b d ) ( mnoz a c ) ) ( max( mnoz a d ) ( mnoz b c ) ) };;


(* POMOCNICZE FUNKCJE *)

let rec zaktualizuj_przedzialy w =	(* Jesli dwa przedzialy sie przecinaja, to zostaja polaczone w jeden przedzial *)
	match w with
	| [] -> []
	| { l = x ; p = y }::tl ->
		let listeczka = zaktualizuj_przedzialy ( tl )
		in
		match listeczka with
		| [] -> [ { l = x ; p = y } ]
		| h::t -> if y >= h.l then { l = min x h.l ; p = max y h.p }::t
			  else { l = x ; p = y }::listeczka;;

let is_nan x = compare x nan = 0;;

let rec czy_nan l =	(* Sprawdza, czy wsrod przedzialow wystepuje wartosc nan *)
	match l with
	| [] -> false
	| { l ; p }::tl -> is_nan l || is_nan p || czy_nan tl;;

(* KONSTRUKTORY *)

let wartosc_dokladnosc x p =
	let pr = max x (-.x ) *. p /. 100.
	in
	[ { l = x -. pr ; p = x +. pr } ];;

let wartosc_od_do x y = [ { l = x ; p = y } ];;

let wartosc_dokladna x = [ { l = x ; p = x } ];;

(* SELEKTORY *)


let rec in_wartosc l liczba =	(* Sprawdza rekurencyjnie, czy liczba zawarta jest w jednym z przedzialow *)
	if czy_nan l then false
	else
	match l with	
	| [] -> false
	| { l ; p }::tl -> ( liczba >= l && liczba <= p ) || in_wartosc tl liczba;;

let min_wartosc ( h::t ) =
	if czy_nan ( h::t ) then nan
	else h.l;;

let rec max_wartosc l =
	if czy_nan l then nan
	else
	match l with
	| [] -> -.infinity
	| { l ; p }::tl -> max p ( max_wartosc tl );;

let sr_wartosc l =
	if czy_nan l then nan
	else ( min_wartosc l +. max_wartosc l ) /. 2.;;

(* MODYFIKATORY *)

let dzialanie a b f =	(* Petla, ktora oblicza f( a_i , b_j ) dla kazdej takiej pary przedzialow *)
	if czy_nan a || czy_nan b then [ { l = nan ; p = nan } ]
	else
	let rec pom a b =
		let rec zrob_przedzialy { l = x ; p = y } b =	(* Dla danego a_i przechodzi po kazdym przedziale z listy b i wykonuje f( a_i , b_j ) *)
		match b with
		| [] -> []
		| h::tl ->( f x y h.l h.p )::zrob_przedzialy { l = x ; p = y } tl
		in
		match a with	(* Przechodzi po kazdym przedziale z listy a *)
		| [] -> []
		| h::tl -> ( zrob_przedzialy h b ) @ (pom tl b)
	in
	zaktualizuj_przedzialy( List.sort compare ( pom a b ) );;   (* Sortuje przedzialy, a nastepnie je aktualizuje *)
 
let plus a b = dzialanie a b suma;;

let minus a b = dzialanie a b roznica;;

let razy a b = dzialanie a b iloczyn;;

let rec podzielic a ( h::tl ) =	(* a / b zamienia w a * ( 1/b ) *)
	if czy_nan a || czy_nan ( h::tl ) then [ { l = nan ; p = nan } ]
	else	
	match tl with
	| [] ->	if h.l >= 0. || h.p <= 0. then	(* Ifowanie brzydkich przypadkow *)
			if h.p = 0. then
				if h.l = 0. then [ { l = nan ; p = nan } ]
				else razy a [ { l = neg_infinity ; p = 1. /. h.l } ]
 			else razy a [ { l = 1. /. h.p ; p = 1. /. h.l } ]
		else razy a [ { l = -.infinity ; p = 1. /. h.l } ; { l = 1. /. h.p ; p = infinity } ] 
	| hx::tx -> zaktualizuj_przedzialy( List.sort compare( 
				( podzielic a ( wartosc_od_do h.l h.p ) ) @ ( podzielic a ( wartosc_od_do hx.l hx.p ) ) ) );;



