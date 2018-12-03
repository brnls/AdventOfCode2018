open System.IO
#I __SOURCE_DIRECTORY__
Directory.SetCurrentDirectory(__SOURCE_DIRECTORY__)

let data = File.ReadAllLines("day2input.txt")

let counts (word: string) =
    let letterCount = Array.zeroCreate 26 
    for c in word do
        letterCount.[int c - 97] <- letterCount.[int c - 97] + 1
    letterCount |> Array.exists(fun x -> x = 2),
    letterCount |> Array.exists(fun x -> x = 3)

let getChecksum() =
    let counts = data |> Array.map counts
    (counts |> Array.filter fst).Length * (counts |> Array.filter snd).Length

