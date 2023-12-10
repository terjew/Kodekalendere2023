let getCount (card,count) = count

type HandType = 
    FiveOfAKind 
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard

let orderedCounts hand = 
    hand |> Seq.countBy id |> Seq.map getCount |> Seq.sortDescending |> Seq.toArray

let orderedCountsWithJoker hand =
    match hand with 
    | "JJJJJ" -> orderedCounts hand
    | _ ->
        let counts = hand |> Seq.countBy id 
        let (c,i) = counts |> Seq.filter (fun (c,i) -> c <> 'J') |> Seq.maxBy (fun (c,i) -> i)
        hand.Replace('J', c) |> orderedCounts

let getHandType hand isJackJoker = 
    let orderedCounts = if isJackJoker then orderedCountsWithJoker hand else orderedCounts hand
    match orderedCounts[0] with 
    | 5 -> FiveOfAKind
    | 4 -> FourOfAKind
    | 3 -> match orderedCounts[1] with
            | 2 -> FullHouse
            | _ -> ThreeOfAKind
    | 2 -> match orderedCounts[1] with
            | 2 -> TwoPair
            | _ -> OnePair
    | _ -> HighCard

let compareHands isJackJoker (h1Type,h1,_) (h2Type,h2,_) =    
    if h1 = h2 then 0
    else
        let cardRanking = if isJackJoker then "AKQT98765432J" else "AKQJT98765432"
        let cards = cardRanking |> Seq.indexed |> Seq.map (fun (i,c) -> (c,i)) |> Map.ofSeq
        if h1Type < h2Type then 1
        else if h1Type > h2Type then -1
        else 
            let h1V = h1 |> Seq.map (fun c -> Map.find c cards)
            let h2V = h2 |> Seq.map (fun c -> Map.find c cards)
            Seq.zip h1V h2V |> Seq.find (fun (c1,c2) -> c1 <> c2) |> fun (c1,c2) -> if c1 < c2 then 1 else -1 
    
let rankHands isJackJoker handsWithBets  =
    handsWithBets 
    |> Seq.sortWith (compareHands isJackJoker)
    |> Seq.indexed 
    |> Seq.map (fun (r,(t,h,b)) -> (r+1,b))
    

let solve handsWithBets part isJackJoker =
    handsWithBets 
    |> Seq.map (fun (h,b) -> (getHandType h isJackJoker,h,b))
    |> rankHands isJackJoker
    |> Seq.sumBy (fun (rank,bet) -> rank * bet)
    |> printfn "Part %d:%d" part 

let handsWithBets = System.IO.File.ReadAllLines("input.txt")
                        |> Seq.map (Utilities.Regex.matchPattern "(\w+) (\d+)")
                        |> Seq.map (fun m -> (m.Groups[1].Value, m.Groups[2].Value |> int))

for (part,isJackJoker) in [|(1, false);(2, true)|] do
    solve handsWithBets part isJackJoker
