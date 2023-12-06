open System.IO
open System.Text.RegularExpressions

type State = {
    Pins : bool[];
    Solved: int;
}

type Move = {
    Index : int;
    PinState : bool;
}

let isSolved pins =
    pins |> Seq.exists (fun b -> not b)

let applyMove state move =
    let index = move.Index
    let pinState = move.PinState
    let pins = state.Pins
    pins[index - 1] <- pinState
    match isSolved pins with
        | true -> {Pins = Array.create 7 false; Solved = state.Solved + 1}
        | false -> {Pins = pins; Solved = state.Solved}

let parseMove (regexMatch : Match) =
    {
        Index = int regexMatch.Groups[2].Value;
        PinState = match regexMatch.Groups[1].Value with 
                    | "klikk" -> true
                    | "klakk" -> false
                    | _ -> failwith "Unexpected token"
    }

let initial = {
    Pins = Array.create 7 false; 
    Solved = 0
}

Regex.Matches(File.ReadAllText("log.txt"), "(kl[ia]kk) på (\d)")
|> Seq.map parseMove
|> Seq.fold applyMove initial
|> (fun state -> state.Solved)
|> printfn "%A"
