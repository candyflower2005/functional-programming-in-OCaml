open PMap

exception Cykliczne
 
type stan_wierzcholka = IN | OUT
 
let topol l =
        let graf = ref empty in
        let odwiedzone = ref empty in
        let wynik = ref [] in
		let sasiedzi v = try find v !graf with Not_found -> [] in
        List.iter(fun (v,lv) -> graf := add v (lv @ (sasiedzi v)) !graf) l;
            let rec dfs v =
                    odwiedzone := add v IN !odwiedzone;
                    List.iter pusc_dfs (sasiedzi v);
                    odwiedzone := add v OUT !odwiedzone;
                    wynik := v::(!wynik)
            and pusc_dfs v =
                    try if find v !odwiedzone = IN then raise Cykliczne
                    with Not_found -> dfs v in
        iter(fun v _ -> pusc_dfs v) !graf;
        !wynik
