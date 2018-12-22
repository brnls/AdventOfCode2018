module Day6

open System
open System.IO
open System.Collections.Generic

let part1And2() = 
    let data = 
        File.ReadAllLines("day6input")
        |> Array.map(fun x -> 
            let split = x.Split(',')
            Int32.Parse(split.[0]),  Int32.Parse(split.[1]))

    let minX = data |> Array.minBy fst |> fst
    let minY = data |> Array.minBy snd |> snd


    let chars = [(int 'a')..(int 'z')] @ [(int 'A') .. (int 'Z')] |> Seq.map char
    let adjusted = 
        data
        |> Array.map (fun (x,y) -> (x - minX, y - minY) )

    let nodes = chars |> Seq.zip adjusted |> Array.ofSeq
    let maxX = data |> Array.maxBy fst |> fst
    let maxY = data |> Array.maxBy snd |> snd

    let distance (x1:int, y1:int) (x2:int, y2:int) =
        Math.Abs(x1 - x2) + Math.Abs(y1 - y2)

    let grid points offset =
        let pointsOffset = points |> Array.map (fun ((a,b),y) -> ((a + offset, b + offset), y))
        let arr =
            Array.init (maxX - minX + 1 + (offset*2)) (fun _ -> Array.init (maxY - minY + 1 + (offset*2)) (fun  _ -> '.'))
        for ((x,y), p) in pointsOffset do 
            arr.[x].[y] <- p 

        let uniqueMinBy projection (arr:'a[]) =
            let mutable min = Int32.MaxValue
            let mutable value = None
            for i in arr do 
                let result = projection i
                if result = min then value <- None
                elif result < min then 
                    value <- Some i
                    min <- result
            value
        arr
        |> Array.iteri (fun x t -> t |> Array.iteri (fun y point ->
            let min = 
                pointsOffset 
                |> uniqueMinBy (fun ((x1, y1), _) -> distance (x,y) (x1,y1))
            if arr.[x].[y] = '.' then
                match min with
                |Some ((_,_), p) ->
                    arr.[x].[y] <- p 
                |_ -> ()
            else arr.[x].[y] <- point  
        ))
        arr

    let getCount (arr: char[][]) =
        let d = Dictionary<char,int>()
        for i in 0..arr.Length - 1 do 
            for j in 0..arr.[0].Length - 1 do 
                if arr.[i].[j] <> '.' then 
                    if d.ContainsKey(arr.[i].[j]) then
                        d.[arr.[i].[j]] <- d.[arr.[i].[j]] + 1
                    else d.[arr.[i].[j]] <- 1 
        d

    let compare offset1 offset2 points =
        let first = grid points offset1 |> getCount
        let second = grid points offset2 |> getCount 
        let changing = ResizeArray()
        let fixedPoint = ResizeArray()
        for key in first do 
            if second.[key.Key] <> key.Value then changing.Add(key)
            else fixedPoint.Add(key)
        changing, fixedPoint 

    let part1 = compare 0 1 nodes |> snd |> Seq.maxBy (fun k -> k.Value) 

    let countSafeArea rowBound colBound nodes =
        let mutable areas = 0
        for i in 0..rowBound - 1 do 
            for j in 0..colBound - 1 do 
                let exceeds =
                    nodes
                    |> Seq.scan (fun acc (x,y) -> acc + distance (x,y) (i,j)) 0
                    |> Seq.exists (fun x -> x >= 10000)
                if not exceeds then areas <- areas + 1
        areas

    part1.Value, countSafeArea (maxX - minX + 1) (maxY - minY + 1) (nodes |> Array.map fst)
