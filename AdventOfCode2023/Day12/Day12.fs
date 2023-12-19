open Utilities
open System.IO
open System.Linq
open FSharp.Collections.ParallelSeq
//open Day12CS

let fillBlanks str indicesToFill =
    str |> Seq.mapi (fun i c -> match c with 
                                | '?' -> if List.contains i indicesToFill then '#' else '.'
                                | _ -> c )
        |> Seq.toArray
        |> System.String

let generateAndTestCombinations str candidates numToPlace isValid =
    candidates |> SequenceHelper.combinations [] numToPlace
    |> PSeq.map (fillBlanks str)
    |> PSeq.filter isValid
    |> PSeq.length

let countValid str =
    let groups = str |> Regex.matches "\d+" |> Seq.map (fun m -> (m.Index, int m.Value))
    let pattern = groups 
                    |> Seq.map (fun (pos,length) -> length)
                    |> Seq.map (sprintf "#{%d}")
                    |> String.concat "\.+"
    let isValid line = Regex.isMatchPattern pattern line

    let total = groups |> Seq.map (fun (pos,length) -> length) |> Seq.sum
    let numPlaced = str |> Regex.matches "#" |> Seq.length
    let candidates = str |> Regex.matches "\?" |> PSeq.map (fun m -> m.Index) |> PSeq.toList

    generateAndTestCombinations str candidates (total - numPlaced) isValid

//File.ReadLines("input.txt")
//|> Seq.map countValid
//|> Seq.sum
//|> printfn "Part 1: %d"

let expandLine str =
    let parts = str |> String.splitWithAny " "
    let part1 = Enumerable.Repeat(parts[0], 5) |> String.concat "?"
    let part2 = Enumerable.Repeat(parts[1], 5) |> String.concat ","
    part1 + " " + part2



//File.ReadLines("sample.txt")
//|> Seq.map expandLine
//|> Seq.toArray
//|> Seq.map Day12Solver.CountSolutions
//|> Seq.sum
//|> Any.print
