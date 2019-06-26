module Session5

open System
open System.Text.RegularExpressions

    ///Creating a class explicitly
    type Point =
        val m_x : float
        val m_y : float

        new (text : string) as this =
            if text = null then
                raise <| ArgumentException("text")
            let parts = text.Split([| ',' |])
            let (successX, x) = Double.TryParse(parts.[0])
            let (successY, y) = Double.TryParse(parts.[1])
            if not successX || not successY then
                raise <| ArgumentException("text")
            {m_x = x; m_y = y}
            then
                printfn "Initialized to [%f, %f]" this.m_x this.m_y
        
        new (x, y) = {m_x = x; m_y = y}
        
        new () = {m_x = 0.0; m_y = 0.0}

        member this.Length =
            let sqr x = x * x
            sqrt <| sqr this.m_x + sqr this.m_y
    // End of class definition


    /// Creating a class implicitly
    type Point2(x : float, y : float) =
        let length =
            let sqr x = x * x
            sqrt <| sqr x + sqr y
        do printfn "Initialized to [%f, %f]" x y

        member this.X = x
        member this.Y = y
        member this.Length = length

        new() = Point2(0.0, 0.0)

        new (text : string) =
            if text = null then
                raise <| ArgumentException("text")
            let parts = text.Split([| ',' |])
            let (successX, x) = Double.TryParse(parts.[0])
            let (successY, y) = Double.TryParse(parts.[1])
            if not successX || not successY then
                raise <| ArgumentException("text")
            new Point2(x, y)

    let p1 = new Point(1.0, 1.0)
    let p2 = new Point()
    let p3 = new Point("3.5, 2.5")



    /// Pattern matching against types
    let whatIs (x : obj) =
        match x with
            | :? string as s -> printfn "x is a string %s" s
            | :? int as i -> printfn "x is an int %i" i
            | :? list<int> as l -> printfn "x is a list of integers %A" l
            | _ -> printfn "x is a '%s'" <| x.GetType().Name

    
    /// Parameterized active patterns
    let (|RegexMatch3|_|) (pattern : string) (input : string) =
        let result = Regex.Match(input, pattern)

        if result.Success then
            match (List.tail [ for g in result.Groups -> g.Value ]) with
            | fst :: snd :: trd :: [] -> Some(fst, snd, trd)
            | [] -> failwith <| "Match succeeded but no groups found."
            | _ -> failwith "Match succeeded, but did not find exactly three groups."
        else
            None
        
    let parseTime input =
        match input with
            | RegexMatch3 "(\d+)/(\d+)/(\d\d\d\d)" (month, day, year)
            | RegexMatch3 "(\d\d\d\d)-(\d+)-(\d+)" (year, month, day) -> Some(new DateTime(int year, int month, int day))
            | _ -> None
