module Ex3

    /// Exercise 3.1
    
    /// a
    let charToUpper (c) =
            match c with
                | 'a' -> 'A'
                | 'e' -> 'E'
                | 'i' -> 'I'
                | 'o' -> 'O'
                | 'u' -> 'U'
                | c -> c
    /// b
    let rec convChar (s:string) = 
        match s with
            | "" -> ""
            | s -> (charToUpper (s.[0])).ToString() + convChar(s.Substring(1))


    ///Exercise 3.2

    /// a
    let rec pclLength ls =
        match ls with
            | [] -> 0
            | _head::rest -> 1 + pclLength rest
    
    /// b
    let rec pclSum ls = 
        match ls with
            | [] -> 0
            | head::rest -> head + pclSum rest

    /// Exercise 3.3
    
    /// Return list of first n elements
    let rec takeSome (n : int, ls : list<int>) =
        match ls with
            | [] -> []
            | head :: rest -> match n with | 0 -> [] | _ -> head::[] @ takeSome ((n-1), rest)