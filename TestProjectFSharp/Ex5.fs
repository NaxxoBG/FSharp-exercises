module Ex5

    /// Exercise 5.1 
    /// Counts the number of vowels in a string
    let countNumOfVowels s =
        let charList = List.ofSeq(s)

        let accFunc (As, Es, Is, Os, Us) letter =
            if letter = 'a' then (As + 1, Es, Is, Os, Us)
            elif letter  = 'e' then (As, Es + 1, Is, Os, Us)
            elif letter  = 'i' then (As, Es, Is + 1, Os, Us)
            elif letter  = 'o' then (As, Es, Is, Os + 1, Us)
            elif letter  = 'u' then (As, Es, Is, Os, Us + 1)
            else (As, Es, Is, Os, Us)
    
        List.fold accFunc (0, 0, 0, 0, 0) charList
            
    /// Exercise 5.2
    /// Return a list with the primes up to a specific number
    let primesUpTo num =
        let sieve = new System.Collections.BitArray((num / 2) + 1, true)
        let result = new ResizeArray<int>(num / 10)
        let upper = int (sqrt (float num))   
    
        if num > 1 then result.Add(2) 

        let mutable m = 1
        while 2 * m + 1 <= num do
            if sieve.[m] then
                let n = 2 * m + 1
                if n <= upper then 
                    let mutable i = m
                    while 2 * i < num do sieve.[i] <- false; i <- i + n
                result.Add n
            m <- m + 1
        result

    /// Exercise 5.3
    /// Return the Nth fib number
    let rec pclFib n = 
        match n with
        | 1 | 2 -> 1
        | n -> pclFib (n-1) + pclFib (n-2)

    //Exercise 5.4

    //a
    let doubleNum x = x * 2

    let sqrNum x = x * x

    //b
    let pclQuad x = (doubleNum >> doubleNum) x

    //c
    let pclFourth x = sqrNum x |> sqrNum