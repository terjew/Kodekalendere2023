open System.IO
open System.Text.RegularExpressions

let getInt string =
    match string with
        | ("zero" | "0")    -> 0
        | ("one" | "1")     -> 1
        | ("two" | "2")     -> 2
        | ("three" | "3")   -> 3
        | ("four" | "4")    -> 4
        | ("five" | "5")    -> 5
        | ("six" | "6")     -> 6
        | ("seven" | "7")   -> 7
        | ("eight" | "8")   -> 8
        | ("nine" | "9")    -> 9
        | _ -> failwith "unexpected token"

let getNumberFromFirstAndLastDigit pattern line = 
    let digits = Regex.Matches(line, pattern) 
                |> Seq.map (fun m -> m.Groups[1].Value |> getInt)
    let first = digits |> Seq.head
    let last = digits |> Seq.last
    first * 10 + last

let getNumberDigitsOnly = 
    getNumberFromFirstAndLastDigit "([0-9])"

let getNumberDigitsAndWords = 
    getNumberFromFirstAndLastDigit "(?=(zero|one|two|three|four|five|six|seven|eight|nine|[0-9]))"

let input = File.ReadLines("input.txt")

input
|> Seq.map getNumberDigitsOnly
|> Seq.sum
|> printfn "Part 1: %d"

input
|> Seq.map getNumberDigitsAndWords
|> Seq.sum
|> printfn "Part 2: %d"
