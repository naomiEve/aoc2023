open System
open System.IO;

let wordNumberMapping = [
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
    ("zero", 0)
]
    
let isDigit c =
    match c with
    | t when t >= '0' && t <= '9' -> true
    | _ -> false
  
let charToInt c =
    c - '0' |> int
    
let getDigits str =
    Seq.toList str |>
    Seq.filter isDigit
    
let findDigits str =
    let digits = getDigits str
    let chars = Char.ToString(Seq.head digits) + Char.ToString(Seq.last digits)
    chars |> int
    
let replacePair (acc: string) (pair: string * int) =
    let word, number = pair.Deconstruct()
    let numberStr = number |> string
    acc.Replace(word, numberStr)
    
let getIndexOfPair (str: string) (pair: string * int) =
    let word, _ = pair.Deconstruct()
    str.IndexOf word
    
let getLastIndexOfPair (str: string) (pair: string * int) =
    let word, _ = pair.Deconstruct()
    str.LastIndexOf word
    
let findFirstHappeningOfWords (str: string) =
    let mappings = wordNumberMapping |>
                   Seq.map (getIndexOfPair str) |>
                   Seq.zip wordNumberMapping |>
                   Seq.filter(fun (pair: (string*int)*int) -> 
                       let _, number = pair.Deconstruct()
                       number > -1
                   ) |>
                   Seq.sortBy (fun (pair: (string*int)*int) -> 
                       let _, number = pair.Deconstruct()
                       number
                   )
    mappings
    
let findLastHappeningsOfWord (str: string) =
    let mappings = wordNumberMapping |>
                   Seq.map (getLastIndexOfPair str) |>
                   Seq.zip wordNumberMapping |>
                   Seq.filter(fun (pair: (string*int)*int) -> 
                       let _, number = pair.Deconstruct()
                       number > -1
                   ) |>
                   Seq.sortByDescending (fun (pair: (string*int)*int) -> 
                       let _, number = pair.Deconstruct()
                       number
                   )
    mappings
    
let rec replaceWordsWithNumbers (str: string) (functor: string -> seq<(string * int) * int>) =
    let wordFrequencies = str |>
                          functor
                          
    match Seq.tryHead wordFrequencies with
    | Some head ->
        let wordPair, _ = head.Deconstruct()
        let newStr = replacePair str wordPair
        replaceWordsWithNumbers newStr functor
    | None -> str
    
let isNotEmpty (str: string) =
    match str.Length with
    | 0 -> false
    | _ -> true
    
let lines = File.ReadAllLines("data.in") |>
            Seq.filter isNotEmpty

let part1 =
    lines |>
    Seq.map findDigits |>
    Seq.reduce (fun acc a -> acc + a)

let part2 =
    let leftSide = lines |>
                   Seq.map(fun a -> replaceWordsWithNumbers a findFirstHappeningOfWords)
        
    let rightSide = lines |>
                    Seq.map (fun a -> replaceWordsWithNumbers a findLastHappeningsOfWord)
                    
    Seq.zip leftSide rightSide |>
    Seq.map (fun pair ->
        let str1, str2 = pair.Deconstruct()
        str1 + str2) |>
    Seq.map findDigits |>
    Seq.reduce (fun acc a -> acc + a)
                    

printfn $"Part one: %d{part1}"
printfn $"Part two: %d{part2}"
