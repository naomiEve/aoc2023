open System.IO

type HandType =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighestCard
    | Impossible
    
type Bet = {
    Hand: HandType
    Cards: string
    Distribution: seq<char * int>
    Cost: int
}
    
let cardWeights =
    let cards = ['A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J']
    let cardsLen = Seq.length cards
    [cardsLen..(-1)..0] |>
    Seq.zip cards
    
let cardToWeight card =
    cardWeights |>
    Seq.pick (fun (chr, weight) ->
        match (chr = card) with
        | true -> Some(weight)
        | false -> None)
    
let cardDistributionFor hand =
    hand |>
    Seq.countBy id |>
    Seq.sortByDescending (fun (chr, i) ->
        (i, cardToWeight chr))
    
let getHandType (hand: string) =
    let distinctLength = hand |>
                         Seq.distinct |>
                         Seq.length
    
    let cardDistribution = hand |>
                           Seq.countBy id |>
                           Seq.sortByDescending snd
                         
    match distinctLength with
    | 5 ->
        HighestCard
    | 4 ->
        OnePair
    | 3 ->
        match (Seq.head cardDistribution) with
        | (_, count) when count = 3 -> ThreeOfAKind
        | (_, count) when count = 2 -> TwoPair
        | _ -> Impossible
    | 2 ->
        match (Seq.head cardDistribution) with
        | (_, count) when count = 4 -> FourOfAKind
        | (_, count) when count = 3 -> FullHouse
        | _ -> Impossible
    | 1 ->
        FiveOfAKind
    | _ ->
        Impossible
        
let lines = File.ReadAllLines("data.in")

let betSorter (bet1: Bet) (bet2: Bet) =
    let rec sorter (first: seq<char>) (second: seq<char>) =
        let v1 = first |> Seq.head
        let v2 = second |> Seq.head
        
        match (v1 = v2) with
        | true when (first |> Seq.skip 1 |> Seq.length) = 0 ->
            0
        | true ->
            sorter (first |> Seq.skip 1) (second |> Seq.skip 1)
        | false ->
            match ((cardToWeight v1) > (cardToWeight v2)) with
            | true -> 1
            | false -> -1
            
    let result = sorter bet1.Cards bet2.Cards
    result

let rec createBetWithJokers cards cost =
    let distrib = cardDistributionFor cards
    let jokers = distrib |>
                 Seq.filter (fun (chr, _) ->
                     chr = 'J') |>
                 Seq.tryHead
                 
    match jokers with
    | Some (_, count) when count < 5 ->
        let rec replaceAll (str: string) oldChar newChar =
            let length = str |>
                         Seq.filter (fun c -> c = oldChar) |>
                         Seq.length
            match length with
            | 0 -> str
            | _ -> replaceAll (str.Replace(oldChar, newChar)) oldChar newChar
        
        let top = distrib |>
                  Seq.filter (fun (c, _) ->
                      c <> 'J') |>
                  Seq.head |>
                  fst
                  
        let replacedCards = replaceAll cards 'J' top
        {
            Hand = getHandType replacedCards
            Cards = cards
            Distribution = distrib
            Cost = cost
        }
    | _ -> 
        {
            Hand = getHandType cards
            Cards = cards
            Distribution = distrib
            Cost = cost
        }

let part1 =
    lines |>
    Seq.map (fun line ->
        let parts = line.Split [|' '|]
        {
            Hand = getHandType parts[0]
            Cards = parts[0]
            Distribution = cardDistributionFor parts[0]
            Cost = parts[1] |> int
        }) |>
    Seq.groupBy (fun bet ->
        bet.Hand) |>
    Seq.sortByDescending fst |>
    Seq.map (fun (hand, bets) ->
        (hand, bets |> Seq.sortWith betSorter)) |>
    Seq.fold (fun acc (_, bets) ->
         acc @ (bets |> Seq.toList)) [] |>
    Seq.fold (fun (total, i) bet ->
        (total + bet.Cost * i, i + 1)) (0, 1) |>
    fst
    
let part2 =
    lines |>
    Seq.map (fun line ->
        let parts = line.Split [|' '|]
        createBetWithJokers parts[0] (parts[1] |> int)) |>
    Seq.groupBy (fun bet ->
        bet.Hand) |>
    Seq.sortByDescending fst |>
    Seq.map (fun (hand, bets) ->
        (hand, bets |> Seq.sortWith betSorter)) |>
    Seq.fold (fun acc (_, bets) ->
         acc @ (bets |> Seq.toList)) [] |>
    Seq.fold (fun (total, i) bet ->
        (total + bet.Cost * i, i + 1)) (0, 1) |>
    fst
   
printfn $"Part one: {part1}"
printfn $"Part two: {part2}"