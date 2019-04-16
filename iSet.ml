type interval = int * int;;
type t = Empty | Node of t * interval * t * int * int;;

let cmp ( a , b ) ( c , d ) =
if b < c then -1 else if a > d then 1 else 0;;

let height t =
match t with
| Empty -> 0
| Node( _ , _ , _ , h , _ ) -> h;;

let nmb t =
match t with
| Empty -> 0
| Node( _ , _ , _ , _ , n ) -> n;;

let make l ( a , b ) r = Node( l , ( a , b ) , r , max (height l) (height r) + 1 , (nmb l) + (nmb r) + b - a + 1 );;

let empty = Empty;;

let is_empty t = ( t = Empty );;

(*****************************************************)

let bal l k r =
  let hl = height l in 
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node( ll, lk, lr, _ , _ ) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node( lrl, lrk, lrr, _ , _ ) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node( rl, rk, rr, _ , _ ) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node( rll, rlk, rlr, _ , _  ) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r;;

let rec min_elt = function
  | Node( Empty, k, _, _ , _) -> k
  | Node( l, _, _, _ , _) -> min_elt l
  | Empty -> raise Not_found;;

let rec remove_min_elt = function
  | Node( Empty, _, r, _, _) -> r
  | Node( l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "iSet.remove_min_elt";;

let rec add_one x = function
  | Node (l, k, r, _ , _) ->
      let c = cmp x k in
      if c = 0 then make l x r
      else if c < 0 then
        let nl = add_one x l in
        bal nl k r
      else
        let nr = add_one x r in
        bal l k nr
  | Empty -> make Empty x Empty;;

let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add_one v r
  | (_, Empty) -> add_one v l
  | (Node(ll, lv, lr, lh , _), Node(rl, rv, rr, rh , _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r;;

let merge t1 t2 =
  match t1, t2 with
  | Empty , _ -> t2
  | _ , Empty -> t1
  | _ ->
      let k = min_elt t2 in
      join t1 k (remove_min_elt t2);;

(*****************************************************)

(* Szukanie przedziałów pokrywajacych się z przedziałem ( a , b ) w lewym i prawym poddrzewie *)

let rec find_left t ( x , y ) =
	match t with
	| Empty -> ( max_int , max_int )
	| Node( l , ( a , b ) , r , _ , _ ) ->
		if x < a then 
			let p = find_left l ( x , y ) in
			if y >= a - 1 then min p ( a , b )
			else p
		else if x > b then
			if x = b + 1 then ( a , b )
			else find_left r ( x , y )
		else ( a , b );;

let rec find_right t ( x , y ) =
	match t with
	| Empty -> ( min_int , min_int )
	| Node( l , ( a , b ) , r , _ , _ ) ->
		if y > b then 
			let p = find_right r ( x , y ) in
			if x <= b + 1 then max p ( a , b )
			else p
		else if y < a then
			if y = a - 1 then ( a , b )
			else find_right l ( x , y )
		else ( a , b );;

let rec mem x t =
	match t with
	| Empty -> false
	| Node( l , ( a , b ) , r , _ , _ ) ->
		if x < a then mem x l
		else if x > b then mem x r
		else true;;

let rec split_tree t x =
	match t with
	| Empty -> ( Empty , Empty )
	| Node( l , y , r , _ , _ ) ->
		match cmp x y with
		| -1 ->	let ( ll , lr ) = split_tree l x in
			( ll , join lr y r )
		| 0 -> ( l , r )
		| 1 ->	let ( rl , rr ) = split_tree r x in
			( join l y rl , rr )
		| _ -> assert false;;

let add x t =
	let ( a , b ) = ( find_left t x , find_right t x ) in
	if a = ( max_int , max_int ) && b = ( min_int , min_int ) then add_one x t
	else let ( lx , _ ) = split_tree t a in
	     let ( _ , rx ) = split_tree t b in
		let y = ( min ( fst a ) ( fst x ) ,
			  max ( snd b ) ( snd x ) ) in
				join lx y rx;;

let split x t =
	if mem x t then
		let ( a , b ) = ( fst ( find_left t ( x , x ) ) , 
				  snd ( find_right t ( x , x ) ) ) in
		let ( lx , rx ) = split_tree t ( a , b ) in
		let l =
			if a = x then lx
			else add ( a , x - 1 ) lx in
		let r =
			if b = x then rx 
			else add ( x + 1 , b ) rx in
		( l , true , r )
	else
		let ( l , r ) = split_tree t ( x , x ) in
		( l , false , r );;

let remove x t =
	let ( lx , _ , _ ) = split ( fst x ) t in
	let ( _ , _ , rx ) = split ( snd x ) t in
	merge lx rx;;

let iter f t =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _ , _) -> loop l; f k; loop r in
  loop t;;


let fold f t acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _ , _) ->
          loop (f k (loop acc l)) r in
  loop acc t;;

let elements t = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _ , _) -> loop (k :: loop acc r) l in
  loop [] t;;


let below x t =
  let rec pom x t =
	match t with
	| Empty -> 0
	| Node( l , ( a , b ) , r , _ , _ ) ->
		if x > b then nmb l + ( b - a + 1 ) + pom x r
		else if x < a then pom x l
		else nmb l + ( x - a + 1 )
  in
  let s = pom x t in
	if s < 0 then max_int
	else if s = 0 then
		match t with
		| Empty -> 0
		| _ -> if x >= fst( min_elt t ) then max_int else 0
	else s;;
		

		

