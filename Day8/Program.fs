open System.Collections.Generic
open System.IO

type Node = {
    Name: string
    Children: string array
}

let parseNode (line: string) =
    let parts = line.Split(" = ")
    let nodeName = parts[0]
    let children = parts[parts.Length - 1]
                       .Replace("(", "")
                       .Replace(")", "")
                       .Split(", ")
    {
        Name = nodeName;
        Children = children
    }
    
let lines = File.ReadAllLines("data.in")

let parseNodes =
    lines |>
    Seq.skip 2 |>
    Seq.map parseNode |>
    Seq.map (fun node ->
        (node.Name, node)) |>
    Map.ofSeq
    
let nthNode n =
    lines |>
    Seq.skip (2 + n) |>
    Seq.pick (fun line ->
        Some(parseNode line))
    
let getNodeByName nodes name =
    Map.find name nodes
    
let traverseUntil directions nodes nodeName start =
    let mutable loop = []
    let rec recursiveTraversal (directions: string) nodes destNode currentNode i =
        let point = currentNode.Name + ((i % directions.Length) |> string)
        match (Seq.contains point loop) with
        | true ->
            None
        | false ->
            loop <- point :: loop
            match currentNode.Name = destNode with
            | true -> Some(i)
            | false ->
                let nextDirection = directions[i % directions.Length]
                match nextDirection with
                | 'R' ->
                    recursiveTraversal directions nodes destNode (getNodeByName nodes currentNode.Children[1]) (i + 1)
                | 'L' ->
                    recursiveTraversal directions nodes destNode (getNodeByName nodes currentNode.Children[0]) (i + 1)
                | _ -> None
        
    recursiveTraversal directions nodes nodeName start 0
    
let part1 =
    let directions = lines |>
                     Seq.head
                     
    let nodes = parseNodes

    match traverseUntil directions nodes "ZZZ" (getNodeByName nodes "AAA") with
    | None -> -1
    | Some value -> value
    
let part2 =
    let directions = lines |>
                     Seq.head
                     
    let nodes = parseNodes
    nodes |>
    Map.toSeq |>
    Seq.filter (fun (name, _) ->
        name.EndsWith('A')) |>
    ignore
    
printfn $"Part one: %d{part1}"