module InterpreterV2

//A type to represent different kinds of sports
    type sportType = Individual | Team
    type Sport = Sport of sportName:string * sType:sportType * numOfPlayers:int

//A type to represent different kinds of meals

    //type mealType = Breakfast | Lunch | Dinner
    type mealServing = Cold | Warm
    type Meal = 
        |   Breakfast of name:string * serving:mealServing
        |   Lunch of name:string  * serving:mealServing
        |   Dinner of name:string  * serving:mealServing

// Sport declarations
    let football = Sport ("Футбол", sportType.Team, 22)
    let iceHockey = Sport ("Хокей на лед", sportType.Team, 12)
    let chess = Sport ("Шах", sportType.Individual, 16)
    let volleyball = Sport ("Волейбол", sportType.Team, 12)
    let basketball = Sport ("Баскетбол", sportType.Team, 10)

// Meal declarations
    let pizzaMargherita = Dinner ("Пица Маргарита",  mealServing.Warm)
    let shrimpFriedRice = Lunch ("Скариди с пържен ориз",  mealServing.Warm)
    let chickenSoup = Lunch ("Пилешка супа",  mealServing.Warm)
    let omelet = Breakfast("Омлет",  mealServing.Warm)
    let spaghettiBolognese = Dinner ("Спагети Болонезе",  mealServing.Warm)
    let roll = Lunch ("Rohlik", mealServing.Warm)

//Phase 1
    
    //Meals
    let interpretMeal (meal:Meal) =
        let mealName = 
            match meal with
                | Breakfast(n,_) | Lunch(n,_) | Dinner(n,_) when n.Equals("Пица Маргарита") -> "Pizza Margherita"
                | Breakfast(n,_) | Lunch(n,_) | Dinner(n,_) when n.Equals("Rohlik") -> "Roll"
                | Breakfast(n,_) | Lunch(n,_) | Dinner(n,_) when n.Equals("Скариди с пържен ориз") -> "Shrimp with fried rice"
                | Breakfast(n,_) | Lunch(n,_) | Dinner(n,_) when n.Equals("Пилешка супа") -> "Chicken soup"
                | Breakfast(n,_) | Lunch(n,_) | Dinner(n,_) when n.Equals("Омлет") -> "Omelet"
                | Breakfast(n,_) | Lunch(n,_) | Dinner(n,_) when n.Equals("Спагети Болонезе") -> "Spaghetti Bolognese"
                | _ -> "No translation available"
    
        let mealType = 
            match meal with
                | Breakfast(_,_) -> Breakfast.ToString()
                | Lunch(_,_) -> Lunch.ToString()
                | Dinner(_,_)  -> Dinner.ToString()
        
        let mealServe =
            match meal with
                | Breakfast(_,s) | Lunch(_,s) | Dinner(_,s) when s = Warm -> Warm.ToString()
                | Breakfast(_,s) | Lunch(_,s) | Dinner(_,s) when s = Cold -> Cold.ToString()
                | _ -> "No available translation"

        let str = sprintf "EN: %s is delicious %s when served %s." mealName mealType mealServe
        printfn "Translated meal information: %s" str
        str
    
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
                | Sport(_, t, _) when t = Individual -> Individual.ToString()
                | Sport(_,t,_) when t = Team -> Team.ToString()
                | _ -> "No available translation"
        
        let numOfPlayers =
            match sport with
                | Sport(_, _, s) -> s

        let str = sprintf "EN: %s is a %s sport with %d players." sportName sportType numOfPlayers
        printfn "Translated meal information: %s" str
        str