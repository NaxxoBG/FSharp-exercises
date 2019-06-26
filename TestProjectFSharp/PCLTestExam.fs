module PCLTestExam
//F# is statically typed
//F# is a strongly typed, multi-paradigm programming language that encompasses functional, imperative, and object-oriented programming methods.

// Question 1
//a F# is not a pure functional language but a multiparadigm one since it encompasses object-oriented, functional and other methods

//b Scala traits encapsulate method and field definitions which can be reused by being mixed into classes

//c concurrency the parallel execution of several processes

//d static and dynamic typing
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

// Question 2
    let f x y = x + y
    let g (x, y) = x + y

//a
// The type defintions show the difference, the first function is defining a function whose result is passed to another function and then returned
// The second function is a function that accepts a tuple and returns an integer
//val f : x:int -> y:int -> int
//val g : x:int * y:int -> int

//If a mathematical function can only have one parameter, then how is it possible that an F# function can have more than one?
//The answer is quite simple: a function with multiple parameters is rewritten as a series of new functions, 
//each with only one parameter. And this is done automatically by the compiler for you. It is called “currying”,

//b
    let rec strToCharList (str : string) = 
       match str with
           | "" -> []
           | s -> s.[0] :: strToCharList(s.Substring(1))

// Question 3
//a
    type Vector =  
        | Vector2 of x:double * y:double
        | Vector3 of x:double * y:double * z:double
        | Vector4 of x:double * y:double * z:double * m:double
        | Vector5 of x:double * y:double * z:double * m:double * n:double
    
    let dec v = 
        match v with
            | Vector2(x, y) -> [x; y]
            | Vector3(x, y, z) -> [x; y; z]
            | Vector4(x, y, z, m) -> [x; y; z; m]
            | Vector5(x, y, z, m, n) -> [x; y; z; m; n]

//b
    let vecLen v =
        List.fold (fun acc a -> acc + System.Math.Pow(a, 2.0)) 0.0 (dec v) |> System.Math.Sqrt 

//c
    let vec1 = Vector2(4.1, 15.2)
    let vec2 = Vector3(1.1, 1.1, 1.1)
    let vec3 = Vector3(2.7, 3.2, 4.6)
    
    let vecAdd v1 v2 =
        try
            List.map2 (fun e1 e2 -> e1 + e2) (dec v1) (dec v2)
        with
            | :? System.ArgumentException -> printfn "Vectors must be the same size"; []

// Question 4

//a
    let rec rerun s n =
        match n with
            | 0 -> ""
            | _ -> s + rerun s (n - 1)


//b
    let rerunTailRec s n =
        let rec helper (str, n) =
            match n with
                | 0 -> str
                | _ -> helper(str + s, n - 1)
        helper ("", n)

// Question 5
    let f1 i j k =
        seq {
            for x in [0 .. i] do
                for y in [0 .. j] do
                    if x+y < k then yield (x,y)
        }

    let f2 f p sq =
        seq {
            for x in sq do
            if p x then yield f x
        }

    let f3 g sq =
        seq {
            for s in sq do
            yield! g s
        }

//a Value of List.ofSeq (f1 2 2 3).
    let res = List.ofSeq (f1 2 2 3)

//b
    let f2Alt f p sq =
        Seq.filter p sq |> Seq.map f

// Question 6
//a
    type expr =
        | Const of int
        | BinOpr of expr * string * expr
    
    let exp1 = BinOpr(Const(12), "-", Const(2))
    let exp2 = Const(23)
    let exp3 = BinOpr(BinOpr(Const(3), "*", Const(8)), "-", Const(32))

//b
    let rec toString expr = 
        match expr with
            | Const(x) -> x.ToString()
            | BinOpr(a, s, b) -> sprintf "(%s %s %s)" (toString a) s (toString b)

//c
    let rec extractOp expr =
        match expr with
            | Const(_) -> []
            | BinOpr(a, s, b) -> extractOp a @ s :: extractOp b

// Question 7

//a
// The process listens for messages and when it receives one, it first checks whether it is a number and if it is, it prints if that number is greater
// than, smaller than or equal to 0 and continues to listen for new messages.
// If the message is the string "bye", "Bye" is printed to the console and the process is stopped. If the message is neither a number, nor the string "bye",
// then "Unexpected message" is printed on the console and the process continues to wait for new messages.

//b How to start the process?
// Pid = spawn(what_am_i_doing, loop, []).

//(optional) How to send messages to the process?
// Pid ! -4.
// Pid ! 0.
// Pid ! 6.
// Pid ! "hello".
// Pid ! "bye".