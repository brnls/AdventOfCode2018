module Day1

open System.IO
open System.Collections.Generic
open System

let data = File.ReadAllLines("data.txt") |> Array.map Int32.Parse

let part1() = data |> Array.sum

let part2() =
    let seen = HashSet<int>()
    let runningTotal = 
        Seq.concat(Seq.initInfinite (fun _ -> data))
        |> Seq.scan (+) 0
    seq {
        for i in runningTotal do
            if seen.Contains(i) then yield i
            else seen.Add(i) |> ignore
    }
    |> Seq.head
