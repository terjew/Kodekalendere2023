open System.IO
open Utilities.Any
open Utilities

type State = {
    Position : float * float
    DistanceTraveled : float
}

let moveTo state (x,y) =
    let (x0,y0) = state.Position
    let (dx,dy) = (x0 - x, y0 - y)
    {
        DistanceTraveled = state.DistanceTraveled + System.Math.Sqrt(dx * dx + dy * dy)
        Position = (x,y)
    }

let track = File.ReadAllLines("rute.txt")
            |> Seq.map (Regex.transformGroups "(-?\d+),(-?\d+)" Value.asFloat)
            |> Seq.map (fun s -> (Seq.head s, Seq.last s))
let initial = track |> Seq.head 

track
|> Seq.skip 1 
|> Seq.fold moveTo {Position = initial; DistanceTraveled = 0}
|> (fun state -> ceil (state.DistanceTraveled * 9.0 / 1000.0))
|> print
