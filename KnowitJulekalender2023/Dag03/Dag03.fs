open System.IO

let rec pairs list = seq {  
    match list with 
    | head::tail -> for element in tail do
                        yield head, element
                    yield! pairs tail
    | _ -> () 
    } 

let getBestTrade (line : string) =
    line.Split(",")
    |> Seq.map int64
    |> Seq.toList
    |> pairs
    |> Seq.maxBy (fun (buy,sell) -> sell - buy)

let performTrade total (buy,sell) =
    total % buy + (total / buy) * sell

File.ReadAllLines("input.txt")
|> Seq.map getBestTrade
|> Seq.fold performTrade 200_000L
|> printfn "Total %d"
