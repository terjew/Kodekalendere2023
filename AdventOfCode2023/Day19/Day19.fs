open System.IO
open System.Text.RegularExpressions

open Utilities

type Category = X | M | A | S
let parseCategory str = 
    match str with
    |"x" -> X
    |"m" -> M
    |"a" -> A
    |"s" -> S
    |_ -> failwith "Unexpected category"

type Operation = GT | LT

let parseOperation str = 
    match str with
    | "<" -> LT
    | ">" -> GT
    | _ -> failwith "Unexpected comparison"

type Outcome = 
    Reject | Accept | Forward of string

let parseOutcome str = 
    match str with
    | "R" -> Reject
    | "A" -> Accept
    | any -> Forward(any)
    
type Condition = {
    Category : Category
    Operation : Operation
    Operand : int
}

type Rule = 
    {
        Outcome : Outcome
        Condition : Condition option
    }

let parseRule (m : Match) =
    {
        Outcome = parseOutcome m.Groups["outcome"].Value
        Condition = match m.Groups[1].Length with
                    | 0 -> None
                    | _ -> Some {
                            Category = parseCategory m.Groups["category"].Value
                            Operation = parseOperation m.Groups["operation"].Value
                            Operand = m.Groups["operand"].Value |> int
                        }
    }

let parseRules str =
    str 
    |> Regex.matches "[\{,](?:(?<category>[xmas])(?<operation>[<>])(?<operand>\d+):)?(?<outcome>\w+)"
    |> Seq.map parseRule
    |> Seq.toArray

type Workflow = {
    Name : string
    Rules : Rule[]
}

let parseWorkflow str =
    {
        Name = (Regex.matchPattern "\w+" str).Value
        Rules = parseRules str
    }

let parsePart str =    
    str |> Regex.transformMatches "(\d+)" Value.asInt
        |> Seq.zip [|X ; M ; A ; S|] 
        |> Map.ofSeq

let applyRule part rule  =
    match rule.Condition with
    | Some condition -> 
        let category = part |> Map.find condition.Category 
        let isMatch = match condition.Operation with
                        | GT -> category > condition.Operand
                        | LT -> category < condition.Operand
        match isMatch with 
        | true -> Some(rule.Outcome)
        | false -> None
    | None -> Some(rule.Outcome)

let rec applyWorkflow workflows workflow part =
    let outcome = workflow.Rules 
                    |> Seq.map (applyRule part)
                    |> Seq.pick id
    match outcome with 
    | Reject -> Reject
    | Accept -> Accept
    | Forward fw -> applyWorkflow workflows (workflows |> Map.find fw) part

let inputs = File.ReadLines("input.txt")
                |> Seq.toList
                |> SequenceHelper.splitByValue ""

let workflows = inputs[0] |> Seq.map parseWorkflow |> Seq.map (fun wf -> (wf.Name, wf)) |> Map.ofSeq
let parts = inputs[1] |> Seq.map parsePart |> Seq.toArray

parts 
|> Seq.filter (fun p -> p |> (applyWorkflow workflows workflows["in"]) = Accept) 
|> Seq.sumBy (fun p -> Map.values p |> Seq.sum) 
|> printfn "Part 1: %d"

