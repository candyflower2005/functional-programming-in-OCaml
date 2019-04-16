type 'a queue = Leaf | Node of 'a queue * 'a * int * 'a queue;;
 
let empty = Leaf;;
 
let sort a b =
    match ( a , b ) with
    | ( Leaf , _ ) -> ( b , a )
    | ( _ , Leaf ) -> ( a , b )
    | ( Node( _ , a_x , _ , _ ) ,
        Node( _ , b_x , _ , _ ) ) -> if a_x < b_x then ( a , b ) else ( b , a );;

let right a =
    match a with
    | Leaf -> 0
    | Node( _ , _ , d , _ ) -> d;;
 
let rec join a b =
    let ( a , b ) = sort a b in
    match a with
    | Leaf -> Leaf
    | Node( a_l , a_x , _ , a_r ) ->
        let t = join a_r b
        in let ( l , r ) = if right a_l < right t then ( t , a_l ) else ( a_l , t )
            in Node( l , a_x , right r + 1 , r );;
 
let add a q = join ( Node( Leaf , a , 1 , Leaf ) ) q ;;
 
exception Empty;;
 
let delete_min l =
    match l with
    | Leaf -> raise Empty
    | Node( l , x , _ , r ) -> ( x , join l r );;
 
let is_empty l = ( l = empty );;
