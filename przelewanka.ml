open PMap

exception Mam

let rec nwd a b = if b = 0 then a else nwd b (a mod b)

let sprawdz a =
	let nwdx = Array.fold_left (fun ac (x,_) -> nwd ac x) 0 a in
	Array.for_all (fun (_,y) -> y mod nwdx = 0) a &&
	Array.exists (fun (x,y) -> x <> 0 && (y = 0 || x = y)) a

let czy_rowne a b = 
	let p = ref true in
	Array.iter2 (fun ai bi -> if ai <> bi then p := false) a b; !p

let przelewanka a =
	let n = Array.length a in
	if Array.for_all (fun (x,y) -> x = 0 && y = 0) a then 0 else
	if not(sprawdz a) then -1 else
		let odl = ref empty in
		let kolejka = Queue.create () in
		let roz = Array.init n (fun i -> fst(a.(i))) in
		let szukane = Array.init n (fun i -> snd(a.(i))) in
		(* Funkcje pomocnicze *)
		let nalej t (nr,_) = t.(nr) <- roz.(nr) in
		let wylej t (nr,_) = t.(nr) <- 0 in
		let przelej t (nr1,nr2) =
			let s = t.(nr1) + t.(nr2) in 
			t.(nr2) <- min s roz.(nr2);
			t.(nr1) <- s - t.(nr2) in
		let dodaj t v =
			try if find t !odl = 42 then ()
			with Not_found ->
			odl := add t (find v !odl + 1) !odl;
			Queue.push t kolejka in
		let zrob funkcja v szkl = 
			let t = Array.copy v in
			funkcja t szkl; dodaj t v in
		(* BFS *)
		let start = Array.make n 0 in
		Queue.push start kolejka;
		odl := add start 0 !odl;
		let rec bfs kolejka =
			let v = Queue.take kolejka in
			if czy_rowne v szukane then raise Mam else
				for i = 0 to n - 1 do
					zrob nalej v (i,None);
					zrob wylej v (i,None);
					for j = i + 1 to n - 1 do
						zrob przelej v (i,j);
						zrob przelej v (j,i);
					done;
				done;
				bfs kolejka in
		try bfs kolejka with Mam -> find szukane !odl
