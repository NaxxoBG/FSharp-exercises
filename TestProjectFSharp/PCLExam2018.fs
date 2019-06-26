module PCLExam2018

//Question 1
   
//a   
// Tail recursion is a special case of recursion where the calling function does no more computation after making a recursive call. In head recursion
// more computations could occur after the recursive call. With tail recursion after the recursive call there are no additional functions to execute so
// there is no need to store the instruction pointer on the stack, because the only thing left to do once the recursive call exits is restore the stack to the previous function.

//b 
// The general benefits are that functions are first-class citizens and can be passed as arguments to another functions. Furthermore
// functional languages are less verbose and functional composition and pipelining can be applied when programming. Also when considering immutability, we avoid a lot
// of side effects which are the primary cause of most bugs. The usage of higher-order function is one of the biggest benefits when working with functional programming.

//c
// Scala traits encapsulate method and field definitions which can be reused by being mixed into classes. With inheritance you can extend only
// one class whereas when dealing with mixins, one class can integrate several mixin classes.

//d
// Erlang is designed specifically in mind for distributed multicore systems and scaling is one of its huge advantages. Erlang can
// easily handle clustered architectures due to its inter-process communication using messages. In Erlang, code is executed by a light-weight process.
// They have little overhead, each one is uniquely identifiable and can use multiple cpus on multiprocessor machines. Each process has its own
// program counter, stack and variables, so several processes may use the same program code at the same time. Erlang's approach to
// concurrency is that there is non-shared memory process(each has thread-local memory only).
// Processes can be regarded as threads with a very small amount of extra state and functionality related to mailboxes.
 

//Question 2

//2a
// List comprehensions are an easy way of defining custom collections of elements which are extracted with the application of a custom algorithm
// or simply by filtering through a specified range. As in the example below, we are specifying a range from 1 to 100 and we are extracting the values
// which satisfy the boolean function for an even number.

    let lsComp = [for n in 1..100 do if n % 2 = 0 then yield n]

//b

    let displayList ls =
        let mutable counter = 0
        let rec helper ls =
            match ls with
                | [] -> ""
                | head::rest -> 
                        counter <- counter + 1
                        (sprintf ("\n%d: %A") counter head) + helper rest
        printfn "%s" (helper ls)
    displayList ["Functional"; "Imperative"; "Object-Oriented";"Concurrent"]

//c

    let rec stringToCharList (str : string) = 
       match str with
           | "" -> []
           | s -> s.[0] :: stringToCharList(s.Substring(1))
    stringToCharList "PCL exam"

//Question 3

//a
// The function f(1,3,5) accepts three tupled integer parameters. Defined this way, this function needs to have all of the parameters specified at the same time. This is the opposite of
// "currying" where applying a single parameter results in a new function value.

//b
    let incList l = List.map (fun i -> i + 1) l
    incList [0; 1; 2; 4]

//c

    let isleapYear y =
        let yearDivisibleBy n = (y % n = 0)
        yearDivisibleBy 4 && not (yearDivisibleBy 100) || yearDivisibleBy 400

    let numDaysToEndYear y = 365 * y + y/4 - y/100 + y/400

    let numOfDaysToEndMonth (m, y) =
        let numOfDaysToM c = (367 * m + 5) / 12 - c
        let res m =
            match m with
                | m when m = 1 -> numOfDaysToM 0
                | m when m > 1 && isleapYear y -> numOfDaysToM 1 
                | m when m > 1 && (not << isleapYear) y -> numOfDaysToM 2
                | m -> m
        numDaysToEndYear y - (365 - res m)
    numOfDaysToEndMonth(9, 1792) //654 424

//Question 4

//a
    type Meal = {Name:string; Serving:string; MType:string}
    
//b
    let pizzaMargherita = {Name="pizza"; Serving="warm"; MType="lunch"}
    let shrimpFriedRice = {Name="chicken soup";  Serving="warm"; MType = "lunch"}
    let chickenSoup =  {Name="rice";  Serving="warm"; MType = "dinner"}

//c 
    printfn "%s" pizzaMargherita.Name
    printfn "%s" shrimpFriedRice.Name
    printfn "%s" chickenSoup.Name

//Question 5

//a
    type sportType = Individual | Team
    type Sport = | Sport of sportName:string * sType:sportType * numOfPlayers:int

//b
    let football = Sport ("football", sportType.Team, 22)
    let iceHockey = Sport ("ice hockey", sportType.Team, 12)
    let chess = Sport ("chess", sportType.Individual, 2)

//c
    let defineSport s =
        match s with
        | Sport(n,t,c) ->
            if t = sportType.Individual then
                sprintf ("%s is an individual sport with %d players ") n c
            else
                sprintf ("%s is a team sport with %d players ") n c
    defineSport football
    defineSport iceHockey
    defineSport chess


//Question 6

//a using direct recursion
    let rec sumOnlyPositives ls =
        match ls with
            | [] -> 0
            | head::rest ->
                if head >=0 then
                    head + sumOnlyPositives rest
                else
                    0 + sumOnlyPositives rest
    sumOnlyPositives [2; -3; 4; -5; 6]
                    
//** bonus with tail recursion
    let rec sumOnlyPositivesTrec (ls:list<int>) =
        let mutable sum = 0
        let rec helper (ls) =
            match ls with
            | [] -> sum
            | head::rest ->
                if head >= 0 then
                    sum <- sum + head
                    helper(rest)
                else
                    helper(rest)
        helper(ls)
    sumOnlyPositivesTrec [2; -3; 4; -5; 6]

//b
    let sumOnlyPositivesHof (ls:list<int>) =
        List.filter (fun i -> i >= 0) ls |> List.fold (fun acc r -> acc + r) 0
    sumOnlyPositivesHof [2; -3; 4; -5; 6]

//c
    let myPartition bfun ls =
        let mutable lsTrue = []
        let mutable lsFalse = []

        for i in ls do
            if (bfun i) then
                 lsTrue <- i :: lsTrue
            else
                 lsFalse <- i :: lsFalse
        lsTrue <- List.rev lsTrue
        lsFalse <- List.rev lsFalse
        (lsTrue, lsFalse)

    myPartition (fun n -> n > 0) [2;-3; 4; -5; 6]

//Question 7

    type WeeklyActivity =
        | Dancing of int
        | Gaming of int
        | Running of int
        | SeeingMovies of int
        | Walking of int
        with
            override qty.ToString() =
                match qty with
                    | Walking rate -> Printf.sprintf "Walking %i times per week" rate
                    | Running rate -> Printf.sprintf "Running %i times per week" rate
                    | Dancing rate -> Printf.sprintf "Dancing %i times per week" rate
                    | Gaming rate -> Printf.sprintf "Gaming %i times per week" rate
                    | SeeingMovies rate -> Printf.sprintf "SeeingMovies %i times per week" rate

    //a
    let agent = MailboxProcessor<WeeklyActivity>.Start(fun handler ->

        let advise (x:WeeklyActivity) =
                match x with
                    | Dancing(q) -> 
                        if q > 10 then sprintf "%s is a bit too much." (x.ToString()) 
                        else if q < 10 && q > 4 then sprintf "%s in a day is a good amount." (x.ToString()) 
                        else sprintf "%s is not enough." (x.ToString()) 
                    | Gaming(q) -> 
                        if q >= 7 then sprintf "%s is a bit too much." (x.ToString()) 
                        else if q = 4 then sprintf "%s in a day is a good amount." (x.ToString())
                        else sprintf "%s is not enough." (x.ToString()) 
                    | Running(q) ->
                        if q > 7 then sprintf "%s is a bit too much." (x.ToString()) 
                        else if q = 4 then sprintf "%s is a good amount." (x.ToString())
                        else sprintf "%s is not enough." (x.ToString()) 
                    | SeeingMovies(q) ->
                        if q > 5 then sprintf "%s is a bit too much." (x.ToString()) 
                        else if q <= 3 && q >= 1 then sprintf "%s is a good amount." (x.ToString()) 
                        else sprintf "%s is not enough." (x.ToString()) 
                    | Walking(q) ->
                        if q > 10 then sprintf "%s is a bit too much." (x.ToString()) 
                        else if q <= 8 && q >= 6 then sprintf "%s is a good amount." (x.ToString()) 
                        else sprintf "%s is not enough." (x.ToString()) 
            

        let rec translatorLoop() = async {
            let! req = handler.Receive()
            (printfn "%s" << advise) req
            return! translatorLoop()
        }
        translatorLoop()
    )
    
    //b
    let dance = Dancing(5)
    let game = Gaming(4)
    let run = Running(7)
    let seeMovies = SeeingMovies(4)
    let walk = Walking(6)

    agent.Post dance
    agent.Post game
    agent.Post run
    agent.Post seeMovies
    agent.Post walk
