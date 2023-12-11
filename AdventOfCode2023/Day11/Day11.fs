open Utilities
open System.IO

let universe = File.ReadAllLines("input.txt")
                |> Matrix.fromStringArray

let emptyRows = universe |> Matrix.findRowsMatching "^\.+$"
let emptyCols = universe |> Matrix.findColumnsMatching "^\.+$"

let findGalaxyPairs universe =
    universe
    |> Matrix.findAll '#'
    |> Seq.toList
    |> SequenceHelper.pairs

let expandedRows = emptyRows |> Matrix.withDuplicatedRows universe
let expandedUniverse = emptyCols |> Matrix.withDuplicatedColumns expandedRows

expandedUniverse
|> findGalaxyPairs
|> Seq.sumBy (fun (s1,s2) -> Coordinate.manhattanDistance s1 s2)
|> printfn "Part 1: %d"

let manhattanDistanceWithSpaceExpansion (x1,y1) (x2,y2) expandingRows expandingColumns factor =
    let distance expandingSpace steps =
        steps |> Seq.map (fun step -> match Set.contains step expandingSpace with
                                        | true -> factor
                                        | false -> 1L)

    let (xmin,xmax) = MathHelpers.ordered x1 x2
    let (ymin,ymax) = MathHelpers.ordered y1 y2
    let xDist = seq {xmin .. xmax - 1} |> distance expandingColumns |> Seq.sum
    let yDist = seq {ymin .. ymax - 1} |> distance expandingRows |> Seq.sum
    xDist + yDist

universe
|> findGalaxyPairs
|> Seq.sumBy (fun (s1,s2) -> manhattanDistanceWithSpaceExpansion s1 s2 emptyRows emptyCols 1_000_000L)
|> printfn "Part 2: %d"
