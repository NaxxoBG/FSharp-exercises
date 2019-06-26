module PCLTestExam2

//Question 1
//Statically typed languages
//A language is statically typed if the type of a variable is known
//at compile time. For some languages this means that you as the programmer must specify what type each variable is
//(e.g.: Java, C, C++); other languages offer some form of type inference, the capability of the type system to deduce the type of 
//a variable (e.g.: OCaml, Haskell, Scala, Kotlin)

//The main advantage here is that all kinds of checking can be done by the 
//compiler, and therefore a lot of trivial bugs are caught at a very early stage.

//Dynamically typed languages
//A language is dynamically typed if the type is associated with run-time values,
//and not named variables/fields/etc. This means that you as a programmer can write a little 
//quicker because you do not have to specify types every time (unless using a statically-typed 
//language with type inference). Example: Perl, Ruby, Python

//b
// Functions are first-class citizens. Chaining of functions as well as functional composition make the language less verbose.

//c
// Scala traits encapsulate method and field definitions which can be reused by being mixed into classes

//d
//Erlang is designed specifically in mind for distributed multicore systems

//e
// The server process is waiting to receive a time zone and then checks whether the hour of the current time added to that time zone
// is greater than 23. If it is not it checks if that sum is smaller than 0. If neither of those cases are true, it just sends back a 
// tuple containting that sum + 24 to the process that initially sent the time zone. After all that it continues to listen for new messages.

//Question 2
//a
    let rec sumAbsDr (ls:list<int>) =
        match ls with
            | [] -> 0
            | head::rest -> System.Math.Abs(head) + sumAbsDr rest
    let res1 = sumAbsDr [2; -4; 6; -8; 5; -10; 15; -20]

//b
    let sumAbsHof ls =
        List.fold (fun acc (i:int) -> acc + System.Math.Abs(i)) 0 ls
    let res2 = sumAbsHof [2; -4; 6; -8; 5; -10; 15; -20]

//c
    let lastElement ls =
        let rec helper ls =
            match ls with
                | [] -> 0
                | [r] -> r
                | _::rest -> helper rest
        helper ls

//Question 3

//a
    let withSnd (x, y) z =
        (x, z)

//b
    let recur n x =
        Array.create n x |> Array.toList
    
    let rec recur2 (n:int) x =
        match n with
            | 0 -> []
            | _ -> x :: recur2 (n-1) x

//c
    let displayList ls =
        let mutable counter = 0
        let rec helper ls =
            match ls with
                | [] -> ""
                | head::rest -> 
                        counter <- counter + 1
                        (sprintf ("\n%d: %A") counter head) + helper rest
        printfn "%s" (helper ls)
    displayList [12; 10; 7];;


//Question 4

    type BinaryTree =
        | Node of (int * BinaryTree * BinaryTree)
        | Empty

//a
    exception TreeEmptyException of string

    let tree1 = Node(2, Node(5,Empty,Empty), Node(3, Node(4,Empty,Empty), Node(12,Empty,Empty)))
    let tree2 = Node(4, Node(34,Empty,Empty), Empty)
    let tree3 = Empty

    let rtNodeValue bt =
        match bt with 
            | Empty -> failwith "The tree is empty"
            | Node(i, _, _) -> i

//b
    let leftSubTree bt =
        match bt with 
            | Empty -> failwith "The tree is empty"
            | Node(_, lst, _) -> lst

//c
    let rec appears bt i =
        match bt with
            | Empty -> false
            | Node(v, lt, rt) -> if v = i then true else appears lt i || appears rt i

(*
    let rec printInOrder tree =
        match tree with
            | Node(data, left, right)
                -> printInOrder left
                   printfn "Node %d" data
                   printInOrder right
            | Empty -> ()
*)

//Question 5

    type Route = int
    type Make = string
    type Model = string
    type Registration = string
    type TransportMeans = 
        | Car of Make * Model * Registration
        | Bicycle
        | Bus of Route * Registration
    
//a
    let tm1 = Car("Audi", "TT", "AR1241")
    let tm2 = Bicycle
    let tm3 = Bus(62, "TE1752")

    let tmList = [tm1; tm2]

//b
    let cityAverageSpeed tm =
        match tm with
            | Car(_, _, _) -> 40
            | Bicycle -> 18
            | Bus(_, _) -> 30

//Question 6

    type Dept = string
    type Scores = Scores of int
    type Points = Points of int
    type Fixtures = Dept * Dept
    type Result = (Dept * Scores) * (Dept * Scores)
    type Table = Map<Dept, Points>

    let competition:list<Dept> = ["ATCM"; "ISM"; "VCM"; "CEG"; "GBE"; "ICT"]

    let result1 = Result(("ATCM", Scores(3)), ("ICT", Scores(6)))
    let result2 = Result(("ATCM", Scores(2)), ("ICT", Scores(8)))
    let result3 = Result(("ISM", Scores(2)), ("CEG", Scores(5)))

//a
    let deptPoints (res:Result) =
        match res with
            | ((d1, s1), (d2, s2)) ->
                if s1 > s2 then 
                    ((d1, Points 2), (d2, Points 0)) 
                else if s1 < s2 then 
                    ((d1, Points 0), (d2, Points 2)) 
                else
                    ((d1, Points 0), (d2, Points 0))

//b
    let initializeCompetition (name:Dept) =
        (name, Points 0)
    
    let initializeTable ls =
        Map.ofList (List.map initializeCompetition ls)

    let table = initializeTable competition

    let updateTable (tabl:Table, res:Result) =
        let resPoints = deptPoints res
        match resPoints with
            | ((d1, p1), (d2, p2)) -> 
                let decPoints (p:Points) =
                    match p with
                        | Points(x) -> x
                let tabl = tabl.Add(d1, Points ((decPoints p1) + decPoints (tabl.Item(d1))))
                tabl.Add(d2, Points(decPoints p2 + decPoints (tabl.Item(d2))))
    
    let table1 = table
    let table2 = updateTable (table, result1)
    let table3 = updateTable (table2, result2)
    let table4 = updateTable (table3, result3)

//c
    let weekUpdate (tabl:Table, resls:list<Result>) =
        let rec helper (table, ls) =
            match ls with
            | [] -> table
            | head::rest -> 
                    let newTable = updateTable (table, head)
                    helper(newTable, rest)
        helper(tabl, resls)
    
    let resList = [result1; result2; result3]
    let tableWeekUpdate = weekUpdate(table1, resList)

//c** Anoter solution
    let mutable tableAfterUpdate = table1
    let tableWeekUpdateV2 = List.fold (fun tableAfterUpdate i -> updateTable(tableAfterUpdate, i)) tableAfterUpdate resList

//Question 7

    type DrinkQty =
            | Water of int
            | Cola of int
            | Coffee of int
            | Tea of int
        with
            override qty.ToString() =
                match qty with
                    | Water qty -> Printf.sprintf "%i cup(s)/glass(es) of water" qty
                    | Cola qty -> Printf.sprintf "%i can(s)/bottles(es) of cola" qty
                    | Coffee qty -> Printf.sprintf "%i cup(s) of coffee" qty
                    | Tea qty -> Printf.sprintf "%i cup(s) of tea" qty

//a
    let water = Water(6)
    let cola = Cola(1)
    let coffee = Coffee(2)
    let tea = Tea(1)

    let agent = MailboxProcessor<DrinkQty>.Start(fun handler ->

        let advise (x:DrinkQty) =
                match x with
                    | Water(q) -> 
                        if q > 15 then sprintf "%s in a day is a bit too much." (x.ToString()) 
                        else if q < 15 && q > 12 then sprintf "%s in a day is a good amount." (x.ToString()) 
                        else sprintf "%s in a day is not enough." (x.ToString()) 
                    | Cola(q) -> 
                        if q >= 2 then sprintf "%s in a day is a bit too much." (x.ToString()) 
                        else if q = 1 then sprintf "%s in a day is a good amount." (x.ToString())
                        else "You should drink some Cola from time to time."
                    | Coffee(q) ->
                        if q > 4 then sprintf "%s in a day is a bit too much." (x.ToString()) 
                        else if q < 3 && q >= 1 then sprintf "%s in a day is a good amount." (x.ToString())
                        else "Not drinking coffee is a good choice."
                    | Tea(q) ->
                        if q > 4 then sprintf "%s in a day is a bit too much." (x.ToString()) 
                        else if q <= 3 && q >= 1 then sprintf "%s in a day is a good amount." (x.ToString()) 
                        else "Drinking any amount of tea is not bad, you should find your tea."
            

        let rec translatorLoop() = async {
            let! req = handler.Receive()
            (printfn "%s" << advise) req
            return! translatorLoop()
        }
        translatorLoop()
    )

//b
    agent.Post water
    agent.Post cola
    agent.Post coffee
    agent.Post tea