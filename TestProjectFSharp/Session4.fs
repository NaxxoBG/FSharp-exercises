module Session4

    // Hand-in 2

    //1
    let rec pclReverse =
        function
        | [] -> []
        | [x] -> [x]
        | head::tail -> pclReverse tail @ [head]

    //2
    let isPalindrome ls = ls = pclReverse ls

    //3
    let lastElement ls = 
        let revList = pclReverse ls
        revList.Head

    /// 4 Convert a list holding tuples [("string", 1); ...] -> ["STRING"; ...]
    let tplToStrList ls =
        let conv t = 
            match t with
                | (x:string, _) -> x.ToCharArray() |> Array.toList |> List.map System.Char.ToUpper |> System.String.Concat
        let res = ls |> List.map conv
        res