open System.IO
open System.Text.RegularExpressions

type Reveal = { Color : string; Count : int }
type Game = { GameId : int; Rounds : seq<seq<Reveal>> }

let maximums = Map.empty
                .Add("red", 12)
                .Add("green", 13)
                .Add("blue", 14)

let isInvalidRound reveals =
    reveals |> Seq.exists (fun reveal -> reveal.Count > maximums[reveal.Color])

let parseRound str = 
    Regex.Matches(str, "(\d+) (red|blue|green)") 
        |> Seq.map (fun m -> {Count = int m.Groups[1].Value; Color = m.Groups[2].Value})

let parseGame line = 
    let m = Regex.Match(line, "Game (\d+): (.*)") 
    let gameid = m |> fun m -> int m.Groups[1].Value
    let rounds = m.Groups[2].Value.Split("; ")
                    |> Seq.map parseRound
    { GameId = gameid;Rounds = rounds }

let isValidGame game = 
    not (game.Rounds |> Seq.exists isInvalidRound)

File.ReadLines("input.txt")
|> Seq.map parseGame 
|> Seq.filter isValidGame
|> Seq.sumBy (fun game -> game.GameId)
|> printfn "Part 1: %A"

let maxByColor color reveals =
    reveals 
        |> Seq.filter (fun reveal -> reveal.Color = color) 
        |> Seq.maxBy (fun reveal -> reveal.Count)
        |> fun r -> r.Count
 
let gamePower game =
    let allReveals = game.Rounds |> Seq.collect (fun round -> round)
    let red = allReveals |> maxByColor "red"
    let green = allReveals |> maxByColor "green"
    let blue = allReveals |> maxByColor "blue"
    red * green * blue

File.ReadLines("input.txt")
|> Seq.map parseGame 
|> Seq.map gamePower
|> Seq.sum
|> printfn "Part 2: %A"

