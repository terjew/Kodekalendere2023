open System.IO
open System.Text.RegularExpressions

type Card = 
    {
        ID : int
        Winning : Set<int>
        Played : Set<int>
    }

let parseLine str = 
    let m = Regex.Match(str, "Card +(?<id>\d+): (?<winning>[\d ]+) \| (?<played>[\d ]+)")
    {
        ID = m.Groups["id"].Value |> int
        Winning = m.Groups["winning"].Value.Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> Seq.map int |> Set.ofSeq
        Played = m.Groups["played"].Value.Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> Seq.map int |> Set.ofSeq
    }
    
let countCorrectNumbers card =
    card.Played
    |> Seq.filter (fun p -> Set.contains p card.Winning)
    |> Seq.length

let scoreCardPart1 card =   
    let correct = countCorrectNumbers card
    match correct with
    | 0 -> 0
    | _ -> pown 2 (correct - 1)

let addToKey score map index =
    map |> Map.change index (fun v -> Some(score + Option.defaultValue 1 v)) 

let applyCardRules copies card =
    let id = card.ID
    let countOption = copies |> Map.tryFind id
    let count = countOption |> Option.defaultValue 1
    let updated = match countOption with 
                    | Some v -> copies
                    | None -> copies |> Map.change id (fun v -> Some(count))
    let score = countCorrectNumbers card
    match score with
        | s when s > 0 -> seq { id + 1 .. id + score } |> Seq.fold (addToKey count) updated
        | _ -> updated

File.ReadAllLines("input.txt")
|> Seq.map parseLine
|> Seq.map scoreCardPart1
|> Seq.sum
|> printfn "%A"

File.ReadAllLines("input.txt")
|> Seq.map parseLine
|> Seq.fold applyCardRules Map.empty
|> Map.values
|> Seq.sum 
|> printfn "%A"

