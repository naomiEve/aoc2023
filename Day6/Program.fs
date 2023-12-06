open System
open System.IO

let simulateRace (time: int64) (dist: int64) =
    printfn $"Simulating race with time %d{time} and dist %d{dist}"
    let zero = 0 |> int64
    [zero..time] |>
    Seq.map (fun secsHeld ->
        let diff = time - secsHeld
        (secsHeld, diff * secsHeld)) |>
    Seq.filter (fun (held, distTravelled) ->
        distTravelled > dist) |>
    Seq.length
    
let getNumbersFromLine (line: string) =
    line.Split [|' '|] |>
    Seq.filter(fun s ->
        s.Length > 0 && Char.IsDigit s[0]) |>
    Seq.map int |>
    Seq.toList
    
let combineNumbersToOne (nums: seq<int>) =
    nums |>
    Seq.fold (fun s num ->
        s + (num |> string)) "" |>
    int64
    
let lines = File.ReadAllLines("data.in")
    
let part1 =
    let times = lines[0] |> getNumbersFromLine
    let distances = lines[1] |> getNumbersFromLine
    
    Seq.zip times distances |>
    Seq.map (fun (time, dist) ->
        let no = simulateRace time dist
        printfn $"Number of times we'd have won: %d{no}"
        no) |>
    Seq.filter (fun won -> won > 0) |>
    Seq.fold (fun acc a ->
        acc * a) 1
    
let part2 =
    let time = lines[0] |>
               getNumbersFromLine |>
               combineNumbersToOne
    let distance = lines[1] |>
                   getNumbersFromLine |>
                   combineNumbersToOne
    
    simulateRace time distance
    
printfn $"Part one: %d{part1}"
printfn $"Part two: %d{part2}"
