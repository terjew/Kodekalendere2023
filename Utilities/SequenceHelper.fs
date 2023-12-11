namespace Utilities

module SequenceHelper = 
    
    let rec pairs list = 
        seq {  
            match list with 
            | head::tail -> for element in tail do
                                yield head, element
                            yield! pairs tail
            | _ -> () 
        }

    let duplicateIndices indicesToDuplicate (sequence : seq<'T>) =
        sequence 
        |> Seq.mapi (fun i r -> (i,r))
        |> Seq.collect (fun (i,r) -> match Set.contains i indicesToDuplicate with
                                        | true -> seq{r; r}
                                        | false -> seq{r} )
