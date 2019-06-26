module Session3

    let square x = x * x
    let negate x = -x

    let firstSquaredThenNegated = (square >> negate) 10
    let firstNegatedThenSquared = (square << negate) 10;;

    let nonEmptyLists = [[1]; []; [4;5;6]; [3;4]; []; []; []; [9]] |> List.filter (not << List.isEmpty)

    type Statement = IfStmt
        and Expression = 
            Integer of int 
                | GreaterThan of Expression * Expression

    /// Append function
    let rec xappend = 
        fun list1 ->
            (fun list2 ->
              match list1 with
                | [] -> list2
                | hdlist1::tllist1 -> hdlist1 :: xappend tllist1 list2)

    /// Convert all characters in a string to upper case
    let stringToUpper (s:string) = s.ToCharArray() |> Array.toList |> List.map System.Char.ToUpper |> System.String.Concat

    let stringToUpperV2 (s:string) = (Array.toList >> List.map System.Char.ToUpper >> System.String.Concat) (s.ToCharArray())

    /// Get all primes up to a specific number
    let primesUnderMax num =
        let factorsOfN num = [
            for i in 1 .. num do
                if i % 2 > 0 then
                    yield i;
        
        ]
        factorsOfN num;

    /// Tail recursive function
    let sumListTailRec ls =
        let rec helper (ls, total) = 
            match ls with
                | [] -> total
                | hd::tl -> 
                    let ntot = hd + total
                    helper(tl, ntot)
        helper(ls, 0)
        

    /// Continuation pattern to pass the rest of the code
    let printReversedList ls =
        let rec prList ls cont =
            match ls with
               | [] -> cont() 
               | hd::tl -> 
                    prList tl (fun () -> printfn "%d" hd
                                         cont())
        prList ls (fun () -> printfn "Done!")

    /// Match tuples
    let testXor x y =
        match x, y with
            | tuple when fst tuple <> snd tuple -> true
            | _, _ -> false

    /// Replicate a list and repeat its elements n number of times
    let replicateList =
        printfn "Enter the number of times elements should be replicated"
        let n = (System.Console.ReadLine()) |> int

        printfn "Enter the size of your list"
        let lSize = (System.Console.ReadLine()) |> int

        let arr = Array.zeroCreate lSize

        for i in 0 .. lSize - 1 do
            printfn "Enter a number of your array"
            arr.[i] <- (System.Console.ReadLine()) |> int
        
        let endList = arr |> Array.toList |> List.map (fun f -> seq {for i in [1..n] do yield f} ) |> List.map Seq.toList |> List.concat
        endList

    /// Replicate a list's elements n number of times without specyfing the size of the list beforehand
    let replicateListV2 =
        printfn "Enter the number of times elements should be replicated"
        let n = (System.Console.ReadLine()) |> int

        let mutable num = System.Console.ReadLine() |> int
        let mutable ls = []
        let mutable endList = []
        try
            while true do
            ls <- num :: ls
            num <- (System.Console.ReadLine()) |> int    
        with
            | :? System.FormatException -> endList <- (ls |> List.map (fun f -> seq {for i in [1..n] do yield f} ) |> List.map Seq.toList |> List.concat |> List.rev)
        endList
        
        