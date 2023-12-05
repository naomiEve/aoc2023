open System
open System.IO
open System.Linq
open System.Threading
open System.Threading.Tasks
open FSharp.Collections.ParallelSeq

type Range(DestStart: int64, SrcStart: int64, Length: int64) =
    member this.MapSourceToStart src =
        match src with
        | _ when src < SrcStart -> None
        | _ when src >= SrcStart &&
                 src <= SrcStart + Length ->
            let diff = src - SrcStart
            Some (DestStart + diff)
        | _ -> None
        
type IndexMap(Ranges: Range list) =
    member this.Map i =
        let maybeRange = Ranges |>
                         Seq.filter (fun range ->
                            match range.MapSourceToStart i with
                            | Some _ -> true
                            | None -> false) |>
                         Seq.tryHead
                      
        match maybeRange with
        | Some range -> range.MapSourceToStart(i).Value
        | None -> i

let lines =
    File.ReadAllLines("data.in")
    
let makeRange (line: string) =
    let parts = line.Split [|' '|]
    Range(
        parts[0] |> int64,
        parts[1] |> int64,
        parts[2] |> int64)
    
type IndexMapAcc = (IndexMap list) * (Range list)
    
let makeIndexMaps =
    lines |>
    Seq.skip 1 |>
    Seq.filter (fun line ->
        line.Length = 0 || Char.IsDigit line[0]) |>
    Seq.rev |>
    Seq.fold (fun (indexMaps, ranges) (line: string) ->
        match line.Length with
        | 0 when List.length ranges > 0 -> (IndexMap(ranges) :: indexMaps, [])
        | 0 -> (indexMaps, [])
        | _ -> (indexMaps, makeRange line :: ranges)) ([], []) |>
    fst
    
let mapToLocation i =
    let indexMaps = makeIndexMaps
    indexMaps |>
    Seq.fold (fun acc map -> map.Map acc) i
    
let part1 =
    let seeds = lines |> Seq.head
    let seedValues = seeds.Replace("seeds: ", "")
                         .Split [|' '|]
                         
    seedValues |>
    Seq.map (fun seed ->
        mapToLocation (seed |> int64)) |>
    Seq.sort |>
    Seq.head
    
printfn $"Part one: %d{part1}"