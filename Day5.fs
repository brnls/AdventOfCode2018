module Day5

open System.IO

let part1And2() = 
    let data = File.ReadAllText("day5input").ToCharArray()
    let n = '\000'

    let isPair (c1: char) (c2: char) =
        int(c1) + 32 = int(c2) || int(c1) - 32 = int(c2)
        
    let result (input: char []) =
        let mutable i = 0
        let mutable j = 1
        while j < input.Length do
            if isPair input.[i] input.[j] then 
                input.[i] <- n
                input.[j] <- n
                while i > 0 && input.[i] = n do 
                    i <- i - 1
                if i = 0 && input.[i] = n then 
                    i <- j
                j <- j + 1
            else 
                i <- j
                j <- j + 1
        input |> Array.filter (fun x -> x <> n)

    let reduction (input: char []) = 
        [(int 'a') .. (int 'z')]
        |> Seq.map (fun x -> 
           let lower, upper = (char x), (char (x - 32))
           input 
           |> Array.filter (fun y -> y <> lower && y <> upper)
           |> result
           |> Array.length
           )
        |> Seq.min

    result data,
    reduction data
