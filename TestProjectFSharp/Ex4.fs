module Pcl3

    //Exercise 4.1

    //a
    let rec fold (f, r:int, ls : list<int>) = 
        match ls with
        | [] -> r
        | head::rest -> fold(f, (fun u -> f u r ) head, rest)
    
    //b
    let pclSumWithFold ls = 
        fold((+), 0, ls)


    //Exercise 4.2

    //a
    let foldBack (f, r : int, ls : list<int>) =
        let rec pclFoldBack (f, r : int, ls : list<int>) =
            match ls with
            | [] -> [r]
            | _ -> pclFoldBack ((f), (fun u -> f u r ) ls.Head, ls.Tail) @ [ls.Head]
        pclFoldBack((f), r, ls).Head

    //a-2
    let rec foldBack2 f l init =
        match l with
        | [] -> init
        | x::xs -> f x (foldBack2 f xs init)

    //b
    let pclSumWithFoldBack ls = 
        foldBack ((+), 0, ls)
   
    //Exercise 4.3
    let rec pclIncList ls : list<int> = 
        match ls with
        | [] -> []
        | _ -> ((fun u -> u + 1) ls.Head) :: pclIncList ls.Tail


    //Exercise 4.4

    //a
    let rec pclMap f ls = 
        match ls with
        | [] -> []
        | _ -> f ls.Head :: pclMap f ls.Tail

    //a-2
    let rec rmap f = function
        | [] -> []
        | hd::tl -> f hd :: rmap f tl

    //b
    let pclIncListWithMap ls =
        pclMap (fun r -> r + 1) ls


    //Exercise 4.5

    //a
    let rec pclFilter f ls = 
        match ls with
        | [] -> []
        | _ -> 
            if f ls.Head 
            then [ls.Head] @ pclFilter f ls.Tail
            else [] @ pclFilter f ls.Tail

    //b
    let pclEven ls = 
        pclFilter (fun n -> n % 2 = 0) ls