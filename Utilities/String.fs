namespace Utilities

module String = 
    let splitWithAny (separators : string) (str : string) =
        str.Split(separators |> Seq.toArray, System.StringSplitOptions.RemoveEmptyEntries)

