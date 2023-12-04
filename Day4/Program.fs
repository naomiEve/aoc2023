open System.IO
open System.Linq

type LineData = int * seq<int> * seq<int>

type Card = {
    Index: int;
    mutable Count: int;
    Winning: int;
}

let parseListOfNumbers (str: string) =
    str.Split [|' '|] |>
    Seq.filter (fun a -> a.Length > 0) |>
    Seq.map int
    
let splitLine (line: string) =
    let gameStringEnd = line.IndexOf ':' + 1
    let numberLinePart = line.Substring (gameStringEnd, line.Length - gameStringEnd)
    let numberSplit = numberLinePart.Split [|'|'|]

    let cardIndex = line.Substring(0, gameStringEnd - 1)
                        .Replace("Card", "")
                        .TrimStart()
    
    let i = cardIndex |> int
    let winning = parseListOfNumbers numberSplit[0]
    let ours = parseListOfNumbers numberSplit[1]
    
    LineData(i, winning, ours)

let parseLine (line: string) =
    let _, winning, ours = splitLine line
    
    let count = ours |>
                Seq.filter (fun num -> Seq.contains num winning) |>
                Seq.length
                
    match count with
    | 0 -> 0
    | 1 -> 1
    | _ -> 1 * pown 2 (count - 1)
    
let lines = File.ReadAllLines("data.in")

let part1 =
    lines |>
    Seq.map parseLine |>
    Seq.sum
    
let part2 =
    let games = lines |>
                Seq.map splitLine
    
    let won = games |>
              Seq.map (fun game ->
                  let _, winning, ours = game
                  ours |>
                  Seq.filter (fun num -> Seq.contains num winning) |>
                  Seq.length) |>
              Seq.zip (games |> Seq.map (fun game ->
                  let i, _, _ = game
                  i)) |>
              Seq.map (fun (index, winning) ->
                  { Index = index; Count = 1; Winning = winning }) |>
              Seq.toList
              
              
    won |>
    Seq.map (fun card ->
        match card.Winning with
        | 0 -> card
        | _ ->
            Enumerable.Range(0, card.Winning) |>
            Seq.iter (fun i ->
                let nextCard = won[card.Index + i]
                nextCard.Count <- nextCard.Count + card.Count)
            card) |>
    Seq.sumBy (fun card -> card.Count)
    
printfn $"Part one: %d{part1}"
printfn $"Part two: %d{part2}"