open System.IO

let createDiffTable seq =
    seq |>
    Seq.skip 1 |>
    Seq.fold (fun (list, i) item ->
        let prev = Seq.item (i - 1) seq
        ((item - prev) :: list, i + 1)
        ) ([], 1) |>
    fst |>
    List.rev
    
let lineToIntSeq (line: string) =
    line.Split [|' '|] |>
    Seq.map int |>
    List.ofSeq
    
let processUntilSequenceAreZeroes seq =
    let rec processSeq list seq =
        let diffTbl = createDiffTable seq
        let allZeroes = Seq.forall (fun a -> a = 0) diffTbl
        match allZeroes with
        | true -> list
        | false ->
            processSeq (diffTbl :: list) diffTbl
            
    processSeq [List.ofSeq seq] seq
    
let predictNext seq =
    seq |>
    Seq.fold (fun acc list ->
        let last = list |>
                   Seq.last
        
        last + acc
        ) 0
    
let predictPrev seq =
    seq |>
    Seq.fold (fun acc list ->
        let last = list |>
                   Seq.head
        
        last - acc
        ) 0
    
let lines = File.ReadAllLines "data.in"

let part1 =
    lines |>
    Seq.map (fun line ->
        lineToIntSeq line |>
        processUntilSequenceAreZeroes |>
        predictNext) |>
    Seq.sum
    
let part2 =
    lines |>
    Seq.map (fun line ->
        lineToIntSeq line |>
        processUntilSequenceAreZeroes |>
        predictPrev) |>
    Seq.sum
    
printfn $"Part one: %d{part1}"
printfn $"Part one: %d{part2}"
