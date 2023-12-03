open System
open System.IO
open System.Linq

type Point2D = { X: int; Y: int }
type Boundary = Point2D * Point2D
type PartNumberAndPosition = int * Boundary
type StringAndIndex = string * int

let isANumber (str: string) =
    str |>
    Seq.exists (fun chr -> Char.IsDigit chr |> not) |>
    not

let boundsOverlap (b1: Boundary) (b2: Boundary) =
    let l1, r1 = b1
    let l2, r2 = b2
    
    let noOverlap = l1.X > r2.X ||
                    l2.X > r1.X ||
                    l1.Y > r2.Y ||
                    l2.Y > r1.Y
                    
    noOverlap |> not

let canIntersect (bound: Boundary) (point: Point2D) =
    let bound2 = Boundary(
        { X = point.X - 1; Y = point.Y - 1},
        { X = point.X + 1; Y = point.Y + 1})
    
    boundsOverlap bound bound2

let getNumbersInLine line =
    let pair = line + "." |>
                Seq.fold (fun (acc: StringAndIndex list * StringAndIndex * int) chr ->
                    let list, stringAndIndex, i = acc
                    let currentString, index = stringAndIndex
                    
                    match Char.IsDigit chr with
                    | true when currentString.Length = 0 -> (list, StringAndIndex(chr |> string, i), i + 1)
                    | true -> (list, StringAndIndex(currentString + (chr |> string), index), i + 1)
                    | false when currentString.Length > 0 -> (stringAndIndex :: list, StringAndIndex("", 0), i + 1)
                    | false -> (list, stringAndIndex, i + 1)
                    ) ([], StringAndIndex("", 0), 0)
                
    let list, _, _ = pair
    list
        

let gatherBoundsForLine (line: string) (y: int) =
    getNumbersInLine line |>
    Seq.map (fun numberIndexPair ->
        let number, beginning = numberIndexPair
        let bound = Boundary(
            { X = beginning; Y = y},
            { X = beginning + number.Length - 1; Y = y}
            )
        PartNumberAndPosition(number |> int, bound)
        )
    
let gatherPointsForLine (line: string) (y: int) =
    let notNumbers = line |>
                      Seq.filter (fun chr -> (Char.IsDigit chr || chr = '.') |> not)
                      
    let pair = notNumbers |>
                Seq.fold (fun (acc: (Point2D * char) list * int) chr ->
                    let list, lastCheckedIndex = acc
                    let x = line.IndexOf(chr, lastCheckedIndex)
                    (({ X = x; Y = y }, chr) :: list, x + 1)
                    ) ([], 0)
                
    let list, _ = pair
    list
  
let lines = File.ReadAllLines "data.in"

let gatherBoundaries =
    lines |>
    Seq.zip (Enumerable.Range (0, lines.Length)) |>
    Seq.fold (fun (list: PartNumberAndPosition list) pair ->
        let y, line = pair
        list @ Seq.toList (gatherBoundsForLine line y)
        ) []
    
let gatherPoints =
    lines |>
    Seq.zip (Enumerable.Range (0, lines.Length)) |>
    Seq.fold (fun (list: (Point2D * char) list) pair ->
        let y, line = pair
        list @ Seq.toList (gatherPointsForLine line y)
        ) []
    
let part1 =
    let bounds = gatherBoundaries
    let points = gatherPoints
    
    bounds |>
    Seq.filter (fun pair ->
        let _, bound = pair
        points |>
        Seq.map (fun pair ->
            let point, _ = pair
            point) |>
        Seq.filter (canIntersect bound) |>
        Seq.length > 0) |>
    Seq.map (fun pair ->
        let number, _ = pair
        number) |>
    Seq.sum
    
let part2 =
    let bounds = gatherBoundaries
    let points = gatherPoints
    
    points |>
    Seq.filter (fun pair ->
        let _, chr = pair
        chr = '*') |>
    Seq.map (fun pair ->
        let point, _ = pair
        let intersected = bounds |>
                          Seq.filter (fun numberBoundary ->
                            let _, bound = numberBoundary
                            canIntersect bound point) |>
                          Seq.map (fun numberBoundary ->
                              let number, _ = numberBoundary
                              number)
        match Seq.length intersected with
        | 2 -> intersected |>
               Seq.fold (fun acc a -> acc * a) 1
        | _ -> 0
        ) |>
    Seq.sum

printfn $"Part one: %d{part1}"
printfn $"Part one: %d{part2}"
