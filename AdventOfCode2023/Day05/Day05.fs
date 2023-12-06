open System.IO
open Utilities
open System.Text.RegularExpressions

type MappingRange = {
    SourceStart : int64
    DestinationStart : int64
    Length : int64
}

type Mapping = {
    Source : string
    Destination : string
    Ranges : List<MappingRange>
}

let createRange seq =
    let list = List.ofSeq seq
    { DestinationStart = list[0]; SourceStart = list[1]; Length = list[2] }

let createMapping source destination ranges =
    (source, {
        Source = source
        Destination = destination
        Ranges = ranges |> String.splitWithAny "\r\n"
                        |> Seq.map (Regex.transformGroups "(\d+) (\d+) (\d+)" Value.asInt64)
                        |> Seq.map createRange
                        |> List.ofSeq
    })

let mapNumber mapping input =
    match mapping.Ranges 
            |> Seq.filter (fun range -> range.SourceStart <= input && range.SourceStart + range.Length - 1L >= input)
            |> Seq.tryExactlyOne 
    with 
    | None -> input
    | Some r -> input - r.SourceStart + r.DestinationStart

let rec map maps current target number =
    let mapping = Map.find current maps
    let mappedNumber = mapNumber mapping number
    match mapping.Destination with 
    | t when t = target -> mappedNumber
    | _ -> map maps mapping.Destination target mappedNumber

let maps = File.ReadAllText("input.txt") 
        |> Regex.matchesWithOptions "(?<source>\w+)-to-(?<destination>\w+) map:(?<ranges>[\s\d]+)" RegexOptions.Singleline
        |> Seq.map (fun m -> createMapping m.Groups["source"].Value m.Groups["destination"].Value m.Groups["ranges"].Value)
        |> Map.ofSeq 

let seeds = File.ReadLines("input.txt") 
            |> Seq.head 
            |> Regex.transformMatches "\d+" Value.asInt64
            |> List.ofSeq

seeds
|> Seq.map (fun seed -> map maps "seed" "location" seed)
|> Seq.min
|> printfn "Part 1: %A"

seeds
|> List.chunkBySize 2
|> Seq.collect (fun pair -> seq {pair[0] .. pair[0] + pair[1] - 1L} 
                            |> Seq.map (fun seed -> 
                                if seed % 1000000L = 0 then printfn "%A" seed
                                map maps "seed" "location" seed                            
                            )
                )
|> Seq.min
|> printfn "Part 2: %A"