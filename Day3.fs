module Day3
open System
open System.IO

let data() = File.ReadAllLines("day3input")

type Claim = {
    id: int
    x: int
    y: int
    width: int
    height: int
}

let parseClaim (line: string) =
    let arr = line.Split()
    let coords = arr.[2].Split(',')
    let dims = arr.[3].Split('x')
    {
        id = Int32.Parse(arr.[0].Replace("#",""))
        x = Int32.Parse(coords.[0])
        y = Int32.Parse(coords.[1].Replace(":", ""))
        width  = Int32.Parse(dims.[0])
        height  = Int32.Parse(dims.[1])
    }

type ClaimStatus =
|NotClaimed = 0
|Claimed = 1
|MultipleClaims = 2

let part1And2 () = 
    let claims = data() |> Array.map parseClaim    
    let iterClaim claim action = 
        for i in claim.x..(claim.x + claim.width - 1) do
            for j in claim.y..(claim.y + claim.height - 1) do
                action i j

    let markFabric claims =
        let fabric = Array2D.init 1001 1001 (fun _ _ -> ClaimStatus.NotClaimed)

        let markClaim claim = 
            iterClaim claim (fun i j ->
                fabric.[i,j] <- 
                    match fabric.[i,j] with
                    |ClaimStatus.NotClaimed -> ClaimStatus.Claimed
                    |_ ->ClaimStatus.MultipleClaims
            )
        for c in claims do markClaim c
        fabric

    let fabric  = markFabric claims        

    let claimIsUnique claim = 
        let mutable unique = true
        iterClaim claim (fun i j -> if fabric.[i,j] = ClaimStatus.MultipleClaims then unique <- false)
        unique

    let uniqueClaim = claims |> Seq.find claimIsUnique

    let mutable count = 0 
    fabric |> Array2D.iter(fun x -> if x = ClaimStatus.MultipleClaims then count <- count + 1)
    count, uniqueClaim.id

