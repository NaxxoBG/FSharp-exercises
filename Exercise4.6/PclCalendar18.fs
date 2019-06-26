module PclCalendar18
    
    ///Exercise 4.6
    
    //1
    let isLeap x = (x % 4 = 0 && x % 100 <> 0) || x % 400 = 0

    //2
    let daysToEndYear x = 
        let mutable acc = 365 * x
        for i in 1 .. x do
            if (isLeap i) then
                acc <- acc + 1;
        acc


    ///Exercise 7.3

    //3
    let daysToEndMonth (m, y) =
        let numOfDaysToM c = (367 * m + 5) / 12 - c
        let res m =
            match m with
                | m when m = 1 -> numOfDaysToM 0
                | m when m > 1 && isLeap y -> numOfDaysToM 1 //here m should be 2 as well if we want to match the example answers from the pdf file
                | m when m > 1 && (not << isLeap) y -> numOfDaysToM 2
                | m -> m
        daysToEndYear y - (365 - res m) // daysToEndMonth (9, 1792) = 654 424, check https://www.timeanddate.com/date/durationresult.html?d1=1&m1=1&y1=1&d2=30&m2=9&y2=1792

    //4
    let eraDay (d, m, y) =
        daysToEndMonth (m, y) - 31 + d

    ///Exercise 10

    //5
    let yearOf eraDay =
        let mutable totalDays = eraDay - 1
        
        let mutable Periods400Years = totalDays / (400 * 365 + 97)
        if Periods400Years = 4 then
            Periods400Years <- 3 
        totalDays <- totalDays - Periods400Years * (400 * 365 + 97)

        let mutable Periods100Years = totalDays / (100 * 365 + 24) 
        if Periods100Years = 4 then
            Periods100Years <- 3 
        totalDays <- totalDays - Periods100Years * (100 * 365 + 24)
        
        let Periods4Years = totalDays / (4 * 365 + 1)
        totalDays <- totalDays - Periods4Years * (4 * 365 + 1)

        let mutable PeriodCompleteYears = totalDays / 365
        if PeriodCompleteYears = 4 then
            PeriodCompleteYears <- 3

        Periods400Years * 400 + Periods100Years * 100 + Periods4Years * 4 + PeriodCompleteYears + 1

    ///Exercise 11b

    //6
    let monthOf x =
        let y = yearOf x
        let z = x - eraDay(1, 1, y)
        let mutable c = 0
        match y with
            | y when isLeap y ->
                if z < 60 then c <- 0
                elif z >= 60 then c <- 1
            | y when (not << isLeap) y -> 
                if z < 59 then c <- 0
                elif z >= 59 then c <- 2
            | _ -> ()
        (12 * (z + c) + 373) / 367
    
    //7
    let dateOf x =
        (x - eraDay(0, monthOf x, yearOf x), monthOf x, yearOf x)
        // dateOf 654415 = (20,9,1792), dateOf 729825 = (10,3,1999) check https://www.timeanddate.com/date/dateadded.html?d1=1&m1=1&y1=1&type=add&ay=&am=&aw=&ad=654414&rec=
    
    //8
    let daysAfter x (d, m, y) =
        eraDay (d, m, y) |> (+) x |> dateOf
