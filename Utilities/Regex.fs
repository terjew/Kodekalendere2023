namespace Utilities
open System.Text.RegularExpressions

module Regex =
    type ValueUnion =
        | MatchValue of m : Match
        | GroupValue of g : Group
    
    type CollectionUnion = 
        | MatchCollectionValue of m : MatchCollection
        | GroupCollectionValue of g : GroupCollection

    let getValue union =
        match union with 
        | MatchValue m -> m.Value
        | GroupValue g -> g.Value

    let getElements collection = 
        match collection with 
        | MatchCollectionValue m -> m |> Seq.map MatchValue
        | GroupCollectionValue g -> g |> Seq.skip 1 |> Seq.map GroupValue

    let matches pattern input =
        Regex.Matches(input, pattern)

    let matchesWithOptions pattern options input=
        Regex.Matches(input, pattern, options)

    let transformWith transform collection =
        collection |> getElements |> Seq.map transform

    let transformMatches pattern transform input =
        Regex.Matches(input, pattern) |> MatchCollectionValue |> transformWith transform

    let transformGroups pattern transform input =
        Regex.Match(input, pattern).Groups |> GroupCollectionValue |> transformWith transform

    let asInt64 (union : ValueUnion) =
        getValue union |> int64

    let asInt (union : ValueUnion) =
        getValue union |> int

    let asFloat (union : ValueUnion) =
        getValue union |> float
