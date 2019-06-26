module Session2

    // Hand-in 1

    //1
    let tupleMe = (239841, "Atanas Latinov")
    let deTupleMe tup = 
        let x, y = tup
        printf "Student number: %d\n"  x
        printf "Student name: %s\n" y
    
    //2
    let rec myStrLength1 (str : string) = 
        match str with
        | "" -> 0
        | s -> 1 + myStrLength1(s.Substring(1))
    
    //3
    let myStrLength2 str = String.length str;

    //4
    let takeSomeHe i = 
        if i < 0 then failwith "Cannot pass negative value to function"
        else printfn "%d" i