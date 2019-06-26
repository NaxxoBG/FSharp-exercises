module Ex2
    
    /// Ex 2.1
    let x = 23
    let myName = "David"
    let age = 25
    let country = "Robudk"

    let y = 4 + 2

    /// Ex 2.3
    let addNum1 x = x + 1

    let addNum10 x = x + 10

    let addNum20 x = addNum10 x |> addNum10


    //Ex 2.4
    let max2 y g = if y >= g then y else g

    let evenOrOdd y = y % 2 = 0

    let addXY x y : int = 
        printfn "Integer 1 -> %d\nInteger 2 -> %d\n" x y
        let sum x y = x + y
        sum x y

    ///Ex 2.5
    let addNum_j_k j k = j + 10 * k

    let rec addNumRec_j_k j k =
        if k = 1 then
            j + addNum10 0
        else
            let dec k = k - 1
            let c = dec k
            addNum10 0 + addNumRec_j_k j c