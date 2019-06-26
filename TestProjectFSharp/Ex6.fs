module Ex6

    //Exercise 6.1

    //a
    type PclShape = RightTriangle of float * float * float | Rectangle of float * float

    //b
    let rtr = PclShape.RightTriangle(3.0, 4.0, 5.0)
    let rect = PclShape.Rectangle(4.0, 5.0)

    //c
    let pclArea (shape : PclShape) =
        match shape with
        | RightTriangle(a, b, _) -> (a * b) / 2.0
        | Rectangle(a, b) -> a * b

    //d
    let pclPerimeter =
        function
        | RightTriangle(a, b, c) -> a + b + c
        | Rectangle(a, b) -> 2.0 * a + 2.0 * b

    //e
    type RightTriangleR = {a:float; b:float; c:float}
    type RectangleR = {a:float; b:float}

    type PclShapeR = RightTriangleR of RightTriangleR | RectangleR of RectangleR

    let figRec1 = PclShapeR.RightTriangleR {a = 3.0; b = 4.0; c = 5.0}
    let figRec2 = PclShapeR.RectangleR {a = 4.0; b = 5.0}

    let pclAreaRec (shape : PclShapeR) =
        match shape with
        | RightTriangleR{a = a; b = b; c = c} -> (a * b) / 2.0
        | RectangleR{a = a; b = b} -> a * b

    let pclPerimeterRec =
        function
        | RightTriangleR{a = a; b = b; c = c} -> a + b + c
        | RectangleR{a = a; b = b} -> 2.0 * a + 2.0 * b

    
    //Exercise 6.2

    let sampleCharList = ['p'; 'p'; 's'; 'c'; 'a'; 'l';'a';'p';'c';'l';'y']

    /// Collects elements in lists containing consecutive elements
    let pclCollect xs =
        let collect x =
            function
            | (y::xs)::xss when x = y -> (x::y::xs)::xss
            | xss -> [x]::xss
        List.foldBack collect xs []