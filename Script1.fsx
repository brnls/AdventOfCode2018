open System.IO
#I __SOURCE_DIRECTORY__

open System.Collections.Generic

Directory.SetCurrentDirectory(__SOURCE_DIRECTORY__)

type Edge = {
    From: char 
    To: char
} 
let example = @"Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.".Split('\n')

let parseEdges str =
    let parseEdge (l: string) = 
        let split = l.Split()
        {From = split.[1].[0]; To= split.[7].[0]}
    str
    |> Array.map parseEdge

let buildLookups edgeList = 
    let edges = Dictionary<char, ResizeArray<char>>()
    let inverseEdges = Dictionary<char, ResizeArray<char>>()
    for e in edgeList do 
        if not (edges.ContainsKey(e.From)) then 
            edges.[e.From] <- ResizeArray()
        edges.[e.From].Add(e.To)
        if not (edges.ContainsKey(e.To)) then 
            edges.[e.To] <- ResizeArray()
        
        if not (inverseEdges.ContainsKey(e.To)) then 
            inverseEdges.[e.To] <- ResizeArray()
        inverseEdges.[e.To].Add(e.From)
        if not (inverseEdges.ContainsKey(e.From)) then 
            inverseEdges.[e.From] <- ResizeArray()
    edges, inverseEdges

let topologicalSort edgeList =
    let edges, inverseEdges = buildLookups edgeList
    let sources = 
        let startNodes = inverseEdges |> Seq.filter (fun x -> x.Value.Count = 0) |> Seq.map (fun x -> x.Key)
        HashSet(startNodes)
    let path = ResizeArray() 
    while sources.Count > 0 do 
        let n = sources |> Seq.sort |> Seq.head 
        sources.Remove(n) |> ignore
        path.Add(n)
        let nodes = edges.[n] |> Seq.toArray
        for m in nodes do 
            edges.[n].Remove(m) |> ignore
            let mList = inverseEdges.[m]
            mList.Remove(n) |> ignore
            if mList.Count = 0 then 
                sources.Add(m) |> ignore
    path
            
let edgeList = parseEdges (File.ReadAllLines("day7input.txt"))
//let edgeList = parseEdges example
let edges, inverseEdges = buildLookups edgeList
edges.['E']
let finalPath = topologicalSort edgeList |> Array.ofSeq |> System.String 