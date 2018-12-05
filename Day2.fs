module Day2

open System.IO

let data = File.ReadAllLines("day2input.txt")

let part1() = 
    let counts (word: string) =
        let letterCount = Array.zeroCreate 26 
        for c in word do
            letterCount.[int c - 97] <- letterCount.[int c - 97] + 1
        let mutable exactlyTwo = false
        let mutable exactlyThree = false
        let mutable i = 0
        while i < letterCount.Length && (not exactlyTwo || not exactlyThree) do
            if letterCount.[i] = 2 then exactlyTwo <- true
            elif letterCount.[i] = 3 then exactlyThree <- true
            i <- i + 1
        exactlyTwo, exactlyThree

    data 
    |> Array.map counts
    |> Array.fold (fun (sx, sy) (x,y) ->  (if x then sx + 1 else sx), (if y then sy + 1 else sy)) (0, 0)
    |> (fun (x,y) -> x * y)

let part2() =
    let compare (id1:string, id2:string) =
        let rec inner idx diffIdx =
            if idx = id1.Length then 
                diffIdx |> Option.map (fun i -> id1.Remove(i, 1))
            else
            match diffIdx with
            |None ->
                if id1.[idx] <> id2.[idx] then
                    inner (idx + 1) (Some idx)
                else inner (idx + 1) None
            |Some _ ->
                if id1.[idx] <> id2.[idx] then
                    None
                else inner (idx + 1) diffIdx
        inner 0 None

    Seq.allPairs data data 
    |> Seq.choose compare
    |> Seq.head 
