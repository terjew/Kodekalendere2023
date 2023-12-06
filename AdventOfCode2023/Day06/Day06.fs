open System.IO
open Utilities
open System

let q (a, b, c) = Math.Sqrt(b*b - 4.0*a*c)
let quad (a,b,c) = ((-b + q(a,b,c))/(2.0*a), (-b - q(a,b,c))/(2.0*a));;

let countWinningMoves (time,distance) =
    let x2,x1 = quad (1.0, -time |> float, distance |> float) 
    int(ceil(x2)) - int(floor(x1)) - 1
    
let lines = File.ReadLines("input.txt") |> List.ofSeq
let times = lines[0] |> Regex.transformMatches "\d+" Value.asInt64
let distances = lines[1] |> Regex.transformMatches "\d+" Value.asInt64

Seq.zip times distances |> Seq.map countWinningMoves 
|> Seq.fold (fun product element ->  element * product |> int) 1 
|> printfn "Part 1: %A"

(times |> Seq.map string |> String.concat "" |> int64, distances |> Seq.map string |> String.concat "" |> int64)
|> countWinningMoves |> printfn "Part 2: %A"
