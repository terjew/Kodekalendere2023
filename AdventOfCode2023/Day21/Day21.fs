open System.IO
open Utilities

let expand board =
    let allOs = board |> Matrix.findAllOf ['O';'S']
    let nextSteps = allOs 
                    |> Seq.map (fun coord -> Matrix.neighborCoordsWithDirection board coord)
                    |> Seq.collect (fun m -> m.Values)
                    |> Set.ofSeq
    board |> Matrix.map (fun m coord ->
        match (Matrix.get m coord) with
        | '#' -> '#'
        | _ -> 
            match nextSteps.Contains coord with
            | true -> 'O'
            | false -> '.'
    )

let board = File.ReadLines("input.txt") |> Matrix.fromStrings

seq {1..64}
|> Seq.fold (fun s a -> expand s) board
|> Matrix.findAll 'O'
|> Seq.length
|> Any.print

//Idea for part 2: 
// Find the amount of squares that can be covered in odd or even tiles respectively
// Find the maximum amount of whole "tiles" that are covered given the max steps
// Calculate the possible amounts of leftover steps and positions after getting to the edge of the whole tiles, and simulate the remainding part along the edge
