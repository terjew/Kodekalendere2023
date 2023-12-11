open Utilities
open System.IO

let prettify c = 
    match c with
    | '|' -> '│'
    | 'L' -> '└'
    | '-' -> '─'
    | 'F' -> '┌'
    | '7' -> '┐'
    | 'J' -> '┘'
    | _ -> c

let connections c =
    match c with
    | '│' -> [|North; South|]
    | '└' -> [|North; East|]
    | '─' -> [|West; East|]
    | '┌' -> [|South; East|]
    | '┐' -> [|South; West|]
    | '┘' -> [|North; West|]
    | 'S' -> [|North; East; South; West|]
    | _ -> [||]

let nextDir c dir =
    connections c 
    |> Seq.filter (fun d -> d <> Direction.oposite dir)
    |> Seq.exactlyOne

let hasConnectionTo matrix pos direction =
    Matrix.get matrix pos |> connections |> Array.exists ((=) direction)

let areConnectionsConnectedBack matrix value pos =
    value 
    |> connections
    |> Seq.map (Matrix.neighborInDirection pos)
    |> Seq.filter (fun (dir,pos) -> Matrix.isInside matrix pos)
    |> Seq.map (fun (dir,pos) -> (dir,hasConnectionTo matrix pos (Direction.oposite dir)))
    |> Map.ofSeq

let rec traverse matrix currentPos incomingDir tail =
    let currentValue = Matrix.get matrix currentPos
    match currentValue with
    | 'S' -> tail //loop complete
    | _ ->  let nextDir = nextDir currentValue incomingDir
            let (_,nextPos) = Matrix.neighborInDirection currentPos nextDir
            traverse matrix nextPos nextDir (nextPos :: tail)
   
let getStartDirections matrix pos =
    areConnectionsConnectedBack matrix 'S' pos
                                |> Map.filter (fun dir connectedBack -> connectedBack)
                                |> Map.keys

let getMainLoop matrix = 
    let start = matrix |> Matrix.find 'S'
    let startDirection = getStartDirections matrix start |> Seq.head
    let (_,startPos) = Matrix.neighborInDirection start startDirection
    traverse matrix startPos startDirection [startPos] 

let renderStart matrix pos =
    let directions = getStartDirections matrix pos |> Set.ofSeq
    let mapping = "│─└┌┐┘" |> Seq.map (fun c -> (c,connections c |> Set.ofSeq)) |> Map.ofSeq
    mapping 
    |> Map.findKey (fun c set -> set = directions) 
    
let renderMainLoop loopPositions matrix pos =
    match Set.contains pos loopPositions with 
    | true -> let c = Matrix.get matrix pos
              match c with 
              | 'S' -> renderStart matrix pos
              | _ -> c
    | false -> '.'

let map = File.ReadAllLines("input.txt") |> Matrix.fromStringArray |> Matrix.map (fun m pos -> Matrix.get m pos |> prettify)

let loop = map |> getMainLoop
printfn "Part 1: %d" ((Seq.length loop) / 2)

let loopPositions = Set.ofSeq loop
let onlyLoop = map |> Matrix.map (renderMainLoop loopPositions)

Matrix.print onlyLoop

let isInsideLoop matrix (x,y) =
    let row = Matrix.row matrix y
    let numVertical = row.Substring(0, x) |> Regex.matches "[│└┘]" |> Seq.length 
    (numVertical % 2) <> 0

let insideOrOut = onlyLoop 
                    |> Matrix.map (fun matrix pos -> let value = Matrix.get onlyLoop pos
                                                     match value with 
                                                     | '.' -> match isInsideLoop onlyLoop pos with
                                                                | true -> 'I'
                                                                | false -> 'O'
                                                     | _ -> value )

insideOrOut |> Matrix.print
insideOrOut |> Matrix.rows |> Seq.map (fun s -> s |> Regex.matches "I" |> Seq.length) |> Seq.sum |> printfn "Part 2: %d"