// Define the Coach record
type Coach = {
    Name: string
    FormerPlayer: bool
}

// Define the Stats record
type Stats = {
    Wins: int
    Losses: int
}

// Define the Team record
type Team = {
    Name: string
    Coach: Coach
    Stats: Stats
}

// Function to calculate success percentage
let successPercentage (team: Team) =
    let wins = float team.Stats.Wins
    let losses = float team.Stats.Losses
    (wins / (wins + losses)) * 100.0

// Sample data provided
let coaches = [
    { Name = "Erik Spoelstra"; FormerPlayer = false }
    { Name = "Doc Rivers"; FormerPlayer = true }
    { Name = "Chris Finch"; FormerPlayer = false }
    { Name = "Willie Green"; FormerPlayer = true }
    { Name = "Tom Thibodeau"; FormerPlayer = false }
]

// Create list of 5 teams
let teams = [
    { Name = "Miami Heat"; Coach = coaches.[0]; Stats =  { Wins = 42; Losses = 33 } }
    { Name = "Milwaukee Bucks"; Coach = coaches.[1]; Stats = { Wins = 15; Losses = 14 } }
    { Name = "Minnesota Timberwolves"; Coach = coaches.[2]; Stats = { Wins = 52; Losses = 23 } }
    { Name = "New Orleans Pelicans"; Coach = coaches.[3]; Stats = { Wins = 45; Losses = 30 } }
    { Name = "New York Knicks"; Coach = coaches.[4]; Stats = { Wins = 44; Losses = 31 } }
]

let successfulTeams = teams |> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)
// Print team names

printfn "Team Names:"
teams |> List.iter (fun team -> printfn "%s" team.Name)

// Filter successful teams
let successPercentages = successfulTeams |> List.map successPercentage

// Output results
printfn "\nPercentage of Successful Teams:"
successfulTeams |> List.iter (fun team -> printfn "%s coached by %s has a success percentage of %.2f%%" team.Name team.Coach.Name (successPercentage team))
// Define the Cuisine discriminated union
// Define the Cuisine discriminated union
type Cuisine =
    | Korean
    | Turkish

// Define the MovieType discriminated union
type MovieType =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks

// Define the Activity discriminated union
type Activity =
    | BoardGame
    | Chill
    | Movie of MovieType
    | Restaurant of Cuisine
    | LongDrive of int * float

// Function to calculate the budget for each activity
let calculateActivityCost (activity: Activity) =
    match activity with
    | BoardGame | Chill -> 0.0
    | Movie movieType ->
        match movieType with
        | Regular -> 12.0
        | IMAX -> 17.0
        | DBOX -> 20.0
        | RegularWithSnacks -> 12.0 + 5.0
        | IMAXWithSnacks -> 17.0 + 5.0
        | DBOXWithSnacks -> 20.0 + 5.0
    | Restaurant cuisine ->
        match cuisine with
        | Korean -> 70.0
        | Turkish -> 65.0
    | LongDrive (distance, fuelCost) -> float distance * fuelCost

// Function to calculate the total budget for all activities
let calculateTotalBudget (activities: Activity list) =
    let activityCosts = List.map calculateActivityCost activities
    let totalBudget = List.sum activityCosts
    activityCosts, totalBudget

// Test cases
let activities = [ BoardGame; Chill; Movie Regular; Movie IMAXWithSnacks; Restaurant Korean; LongDrive (100, 0.1) ]
let activityCosts, totalBudget = calculateTotalBudget activities

// Output result
printfn "Individual activity costs:"
activities
|> List.iteri (fun i activity -> printfn "Activity %d: %.2f CAD" (i+1) (List.item i activityCosts))
printfn "Total budget for Valentine's Day evening: %.2f CAD" totalBudget
