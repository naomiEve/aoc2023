open System.IO

type MinLegalGameCubes = int * int * int

let lines = File.ReadAllLines("data.in")

let checkGamePossible (game: string array) =
    let parts = game |>
                Seq.map (fun (param: string) -> param.Trim ' ')
    
    let matching = parts |>
                   Seq.filter (fun part ->
                        match part with
                        | t when part.Contains "red" ->
                            t.Replace(" red", "") |> int <= 12
                        | t when part.Contains "blue" ->
                            t.Replace(" blue", "") |> int <= 14
                        | t when part.Contains "green" ->
                            t.Replace(" green", "") |> int <= 13
                        | _ -> true)
                   
    (Seq.length parts) = (Seq.length matching)  

let checkLinePossible (gameLine: string) =
    let colon = gameLine.IndexOf ':'
    let cleanedLine = gameLine.Substring (colon + 1)
    let parts = cleanedLine.Split[|';'|]
    
    let firstImpossible = parts |>
                          Seq.map (fun game -> game.Split ", ") |>
                          Seq.tryFind (fun a -> checkGamePossible a |> not)
                          
    match firstImpossible with
    | Some _ -> false
    | None -> true
    
let getSmallestPossibleForLine (gameLine: string) =
    let colon = gameLine.IndexOf ':'
    let cleanedLine = gameLine.Substring (colon + 1)
    let parts = cleanedLine.Split[|';'|]
    
    let smallest = parts |>
                   Seq.map (fun game -> game.Split ", ") |>
                   Seq.fold (fun (acc: string array) part -> Array.concat[part; acc]) [||] |>
                   Seq.map (fun part -> part.Trim ' ') |>
                   Seq.fold (fun (acc: MinLegalGameCubes) part ->
                        let rn, gn, bn = match part with
                                         | t when part.Contains "red" ->
                                            (t.Replace(" red", "") |> int, 0, 0)
                                         | t when part.Contains "blue" ->
                                            (0, 0, t.Replace(" blue", "") |> int)
                                         | t when part.Contains "green" ->
                                            (0, t.Replace(" green", "") |> int, 0)
                                         | _ -> (0, 0, 0)
                        
                        let r, g, b = acc
                        (max rn r, max gn g, max bn b)
                        ) (0, 0, 0)
                   
    smallest
    
let part2 =
    lines |>
    Seq.map getSmallestPossibleForLine |>
    Seq.map (fun triplet ->
        let r, g, b = triplet
        r * g * b) |>
    Seq.reduce (fun acc a -> acc + a)
    
let part1 =
    lines |>
    Seq.filter (fun a -> checkLinePossible a) |>
    Seq.map (fun line ->
        let colonIdx = line.IndexOf ':'
        let gamePart = line.Substring(0, colonIdx)
        gamePart.Replace("Game ", "") |>
        int) |>
    Seq.reduce (fun acc a -> acc + a)
    
printfn $"Part one: %d{part1}"
printfn $"Part two: %d{part2}"
