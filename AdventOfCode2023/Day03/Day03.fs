﻿open System.IO
open System.Text.RegularExpressions

type PartNumber =
    {
        X : int
        Y : int
        Value : int
    }

type Part = 
    {
        X : int
        Y : int
        PartType : char
    }

let getPartsInRow rowIndex str =
    Regex.Matches(str, "[^\.^\d]")
    |> Seq.map (fun m -> 
        {
            X = m.Index
            Y = rowIndex
            PartType = m.Value[0]
        })

let getParts matrix =
    matrix 
    |> Matrix.rows
    |> Seq.mapi getPartsInRow
    |> Seq.collect (fun pn -> pn)

let isDigit x y matrix = 
    match Matrix.get x y matrix with 
    | d when '0' <= d && d <= '9' -> true
    | _ -> false

let getNumber x y matrix = 
    let left = seq {0 .. x - 1} |> Seq.tryFindBack (fun ix -> not (isDigit ix y matrix))
    let row = Matrix.row y matrix
    let m = Regex.Match(row.Substring(left |> Option.defaultValue 0), "\d+")
    {
        X = m.Index
        Y = y
        Value = m.Value |> int
    }
        
let getPartNumbers matrix part =
    Matrix.neighborCoords part.X part.Y matrix
    |> Seq.filter (fun (x,y) -> isDigit x y matrix)
    |> Seq.map (fun (x,y) -> getNumber x y matrix)
    |> Set.ofSeq


let matrix = File.ReadLines("input.txt")
                |> Matrix.fromStrings

matrix
|> getParts
|> Seq.collect (getPartNumbers matrix)
|> Seq.sumBy (fun pn -> pn.Value)
|> printfn "Part 1: %d"

matrix
|> getParts
|> Seq.filter (fun part -> part.PartType = '*')
|> Seq.map (getPartNumbers matrix)
|> Seq.filter (fun seq -> seq.Count > 1)
|> Seq.map (fun seq -> Seq.fold (fun product next -> product * next.Value) 1 seq) 
|> Seq.sum
|> printfn "Part 2: %d"
