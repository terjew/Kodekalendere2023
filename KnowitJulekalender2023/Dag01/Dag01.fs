open System.IO
open System.Text.RegularExpressions

type Bet = { GoalsBetted : int; Odds : float; }

let parseBet (str : string) =
    let parts = str.Split(", ")
    { GoalsBetted = int parts[0]; Odds = double parts[1];}

let evaluateBet goals bet =
    match goals with
        | _unused when goals >= bet.GoalsBetted -> bet.Odds
        | _ -> 0

let applyBet total winFactor =
    let betfactor = 0.175
    let amountBet = round (betfactor * total)
    match winFactor with 
        | 0.0 -> total - amountBet
        | _ -> total + round (amountBet * winFactor)
    
let bets = Regex.Matches(File.ReadAllText("bets.txt"), "\[(.*?)\]")
            |> Seq.map (fun m -> m.Groups[1].Value)
            |> Seq.map parseBet

let goals = File.ReadAllText("goals.txt").Split(",") 
            |> Seq.map int

let initial = 50_000

Seq.map2 evaluateBet goals bets
|> Seq.fold applyBet initial
|> (fun left -> initial - int(left))
|> printfn "Loss: %A" 
