// Learn more about F# at http://fsharp.org
module Session1

    /// Print "Hello World" to the console n times
    let helloFunc (n : int) =
        for i = 1 to n do
            printf "%s" "Hello World\n"

    /// Read from the console and convert the input to an integer
    printf "Enter a number to calculate its factorial\n"
    let input = (System.Console.In.ReadLine ()) |> int

    /// Factorial
    let rec pmfact n = 
        match n with 
            | 0 -> 0
            | _ -> n * pmfact(n - 1)

    /// Recursive factorial
    let rec factorial x =
        if x < 0 then failwith "You cannot input a negative value to this function"
        if x <= 1 then
            1
        else 
            x * factorial(x-1)

    [<EntryPoint>]
    let main _argv =
        let r = factorial input
        printf "%d\n" r
        let pointA = (32, 42)
        let dataB = (1, "fred", 3.1415)
        printf "Point A is -> %A\n" pointA
        let swap (a, b) = (b, a)
        let pointB = swap pointA
        printf "Point B(pointA values swapped) is -> %A\n" pointB
        0;;