module Session6

open System
open System.Collections.Generic
    
    type Student(name : string, number : int, age: int) =
        member __.Name = name
        member __.Number = number
        member __.Age = age
    
        new() = Student("", 0, 0)

    let Val1 = new Student()

    type MyViaStudy(intStarsParam : int, strStatusParam : string) =
        member __.DreamGrade = intStarsParam
        member __.StrStatus = strStatusParam
        
        member __.IncrementStart x = x + 1

        new() = MyViaStudy(0, "")

    let Val2 = new MyViaStudy(0, "")

    let loopUntilFS =
        while (DateTime.Now.Second < 15 && DateTime.Now.Second > 20) do
            printfn "I'm still working"
        printfn "... Finally 15 seconds"
    
    let w = new System.Windows.Forms.Form(Text = "Example F# GUI", Visible = true, TopMost = true)

    let printerAgent = MailboxProcessor.Start(fun inbox -> 
        let rec messageLoop() = async {
            let! msg = inbox.Receive()

            printfn "message is %s" msg

            return! messageLoop()
        }
        messageLoop()
    )


    //Find the last element of a list
    let rec lastEl ls =
        match ls with
        | [] -> 0
        | [r] -> r
        | _::rest -> lastEl rest

    //Find the last element of a list with tail recursion
    let lastEltr ls =
        let mutable lastEl = 0
        let rec helper (ls, el) =
            match ls with
            | [] -> el
            | head::rest ->
                lastEl <- head
                helper(rest, lastEl)
        helper(ls, lastEl)

    //Find the last but one element of a list
    let rec lastButOne ls =
        match ls with
        | [] -> 0
        | [r; _] -> r
        | _::rest -> lastButOne rest
    
    //Find the kth element of a list
    let kthEl ls n =
        let mutable counter = 0
        let mutable res = 0
        let rec helper (ls, el) =
            match ls with
            | [] -> el
            | head::rest ->
                counter <- counter + 1
                if n = counter then
                    res <- head
                helper(rest, res)
        helper (ls, res)

    //Find the kth element of a list
    let rec kthElrec ls n =
        match ls, n with
        | [], _ -> 0
        | head::_, 1 -> head
        | _::rest, n -> kthElrec rest (n - 1)

    //Memoization
    let memoize (f: 'a -> 'b) =
        let dict = new Dictionary<'a, 'b>()

        let memoizedFunc (input: 'a) =
            match dict.TryGetValue(input) with
            | true, x -> x
            | false, _ ->
                let answer = f input
                dict.Add(input, answer)
                answer
        memoizedFunc
