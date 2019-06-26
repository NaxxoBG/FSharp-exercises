module Interpreter

//A type to represent different kinds of sports
    type sportType = Individual | Team
    type Sport = Sport of sportName:string * sType:sportType * numOfPlayers:int

//A type to represent different kinds of meals
    type mealType = Breakfast | Lunch | Dinner
    type mealServing = Cold | Warm
    type Meal = Meal of name:string * mType:mealType * serving:mealServing

// Sport declarations
    let football = Sport ("Футбол", sportType.Team, 22)
    let iceHockey = Sport ("Хокей на лед", sportType.Team, 12)
    let chess = Sport ("Шах", sportType.Individual, 2)
    let volleyball = Sport ("Волейбол", sportType.Team, 12)
    let basketball = Sport ("Баскетбол", sportType.Team, 10)

// Meal declarations
    let pizzaMargherita = Meal ("Пица Маргарита", mealType.Dinner, mealServing.Warm)
    let shrimpFriedRice = Meal ("Скариди с пържен ориз", mealType.Lunch, mealServing.Warm)
    let chickenSoup = Meal ("Пилешка супа", mealType.Lunch, mealServing.Warm)
    let omelet = Meal("Омлет", mealType.Breakfast, mealServing.Warm)
    let spaghettiBolognese = Meal ("Спагети Болонезе", mealType.Dinner, mealServing.Warm)
    let roll = Meal ("Rohlik", mealType.Lunch, mealServing.Warm)
    let bacon = Meal ("Slanina", mealType.Breakfast, mealServing.Warm)

/// Phase 1
    
    //Meals
    let interpretMeal (meal:Meal) =
        let mealName = 
            match meal with
                | Meal(n,_,_) when n.Equals("Пица Маргарита") -> "Pizza Margherita"
                | Meal(n,_,_) when n.Equals("Rohlik") -> "Roll"
                | Meal(n,_,_) when n.Equals("Скариди с пържен ориз") -> "Shrimp with fried rice"
                | Meal(n,_,_) when n.Equals("Пилешка супа") -> "Chicken soup"
                | Meal(n,_,_) when n.Equals("Омлет") -> "Omelet"
                | Meal(n,_,_) when n.Equals("Спагети Болонезе") -> "Spaghetti Bolognese"
                | Meal(n,_,_) when n.Equals("Slanina") -> "Bacon"
                | _ -> "No translation available"
    
        let mealType = 
            match meal with
                | Meal(_,t,_) -> t.ToString()
        
        let mealServe =
            match meal with
                | Meal(_,_,s) -> s.ToString()

        sprintf "EN: %s is delicious %s when served %s." mealName mealType mealServe
    
    //Sports
    let interpretSport (sport:Sport) =
        let sportName = 
            match sport with
                | Sport(n,_,_) when n.Equals("Футбол") -> "Football"
                | Sport(n,_,_) when n.Equals("Хокей на лед") -> "Ice Hockey"
                | Sport(n,_,_) when n.Equals("Шах") -> "Chess"
                | Sport(n,_,_) when n.Equals("Волейбол") -> "Volleyball"
                | Sport(n,_,_) when n.Equals("Баскетбол") -> "Basketball"
                | _ -> "No translation available"
    
        let sportType = 
            match sport with
                | Sport(_, t, _) -> t.ToString()
        
        let numOfPlayers =
            match sport with
                | Sport(_, _, s) -> s

        sprintf "EN: %s is a %s sport with %d competing players." sportName sportType numOfPlayers

/// Phase 2

    let interpreterAgent = MailboxProcessor<obj>.Start(fun handler ->
        let differentiate (x : obj) =
                match x with
                    | :? Meal as m -> interpretMeal m
                    | :? Sport as s -> interpretSport s
                    | _ -> "The passed argument cannot be translated."

        let rec translatorLoop() = async {
            let! req = handler.Receive()
            (printfn "%s" << differentiate) req
            return! translatorLoop()
        }
        translatorLoop()
    )