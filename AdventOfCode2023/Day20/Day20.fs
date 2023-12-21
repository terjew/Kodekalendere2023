open Utilities
open System.IO

type NodeType = Forwarder | FlipFlop | Nand

type Node = {
    Name : string
    Type : NodeType
    Connections : string array
}

let parseNode str = 
    let m = str |> Regex.matchPattern "(?<prefix>[%&])?(?<name>\w+) -> (?<connections>.*)"
    let name = m.Groups["name"].Value
    let connections = m.Groups["connections"].Value |> String.splitWithAny ", "
    let nodeType = match m.Groups["prefix"].Value with
                    | "%" -> FlipFlop
                    | "&" -> Nand
                    | _ -> Forwarder
    {
        Name = name
        Type = nodeType
        Connections = connections
    }

type Value = High | Low
let flip value = 
    match value with 
    | High -> Low
    | Low -> High

type Pulse = {
    Sender : string
    Receiver : string
    Value : Value
}

type CircuitState = {
    NandStates : Map<string,Map<string,Value>>
    FlipFlopStates : Map<string,Value>
    Pulses : Pulse list
    NodeMap : Map<string,Node>
    LowPulsesSent : int
    HighPulsesSent : int
}

let sendPulses state seq =
    let pulseList = (seq |> Seq.toList)
    {state with 
        Pulses = state.Pulses @ pulseList
        LowPulsesSent = state.LowPulsesSent + (pulseList |> Seq.filter (fun p -> p.Value = Low) |> Seq.length)
        HighPulsesSent = state.HighPulsesSent + (pulseList |> Seq.filter (fun p -> p.Value = High) |> Seq.length)
    }

let stepForwarder state pulse node =
    let emits = node.Connections |> Seq.map (fun c -> {Sender = node.Name; Receiver = c; Value = pulse.Value}) 
    sendPulses state emits

let stepFlipFlop state pulse node =
    match pulse.Value with
    | High -> state
    | Low -> 
        let value = state.FlipFlopStates |> Map.find node.Name |> flip
        let emits = node.Connections |> Seq.map (fun c -> {Sender = node.Name; Receiver = c; Value = value})
        sendPulses { state with
                        FlipFlopStates = state.FlipFlopStates |> Map.change node.Name (fun v -> Some(value))
                    } emits

let stepNand state pulse node =
    let inputStates = state.NandStates |> Map.find node.Name |> Map.change pulse.Sender (fun v -> Some(pulse.Value))
    let emitValue = match inputStates |> Map.values |> Seq.forall ((=) High) with
                    | true -> Low
                    | false -> High
    let emits = node.Connections |> Seq.map (fun c -> {Sender = node.Name; Receiver = c; Value = emitValue})
    sendPulses { state with
                    NandStates = state.NandStates |> Map.change node.Name (fun v -> Some(inputStates)) 
                } emits

let step state =
    let pulse = state.Pulses |> List.head 
    let remainingPulses = state.Pulses |> List.skip 1
    let updatedState = {state with Pulses = remainingPulses}

    let nodeMaybe = state.NodeMap |> Map.tryFind pulse.Receiver
    match nodeMaybe with
    | Some node -> 
        match node.Type with 
        | Forwarder -> stepForwarder updatedState pulse node
        | FlipFlop -> stepFlipFlop updatedState pulse node
        | Nand -> stepNand updatedState pulse node
    | None -> 
        match pulse.Value with 
        | High -> updatedState
        | Low -> failwith "won"

let rec solveRec state  =     
    match state.Pulses |> List.isEmpty with
    | false -> 
        let updated = step state
        solveRec updated
    | true -> state

let rec initialState (nodemap : Map<string,Node>) =
    let getNandInputs nand = nodemap.Values |> Seq.filter (fun n -> n.Connections |> Seq.contains nand.Name) |> Seq.map (fun s -> (s.Name,Low)) |> Map.ofSeq
    let nandStates = nodemap.Values |> Seq.filter (fun n -> n.Type = Nand) |> Seq.map (fun nand -> (nand.Name, getNandInputs nand)) |> Map.ofSeq
    let flipFlopStates = nodemap.Values |> Seq.filter (fun n -> n.Type = FlipFlop) |> Seq.map (fun n -> (n.Name,Low)) |> Map.ofSeq    
    {
        NodeMap = nodemap
        NandStates = nandStates
        FlipFlopStates = flipFlopStates
        Pulses = List.empty
        LowPulsesSent = 0
        HighPulsesSent = 0
    }

let pushButton state =
    let pulse = {Sender = "button"; Receiver = "broadcaster"; Value = Low}
    sendPulses state [pulse]

let nodemap = File.ReadLines("input.txt") |> Seq.map parseNode |> Seq.map (fun node -> (node.Name, node)) |> Map.ofSeq
let initial = initialState nodemap

seq {1 .. 1000} 
|> Seq.fold (fun s i -> solveRec (pushButton s)) initial
|> (fun state -> printfn "Part 1: %d" (state.LowPulsesSent * state.HighPulsesSent))

//Part 2 idea:
// Divide the circuit in sub circuits, calculate the period of those sub circuits, then multiply to get total.
