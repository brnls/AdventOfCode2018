﻿open System.IO
#I __SOURCE_DIRECTORY__
Directory.SetCurrentDirectory(__SOURCE_DIRECTORY__)

open System
let data = File.ReadAllLines("day3input")

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

let claims = data |> Array.map parseClaim    

type ClaimStatus =
|NotClaimed = 0
|Claimed = 1
|MultipleClaims = 2

let part1And2 () = 
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

part1And2()
let claimsOverlap claim1 claim2 = 
    let overlapInDim p1 p1Length p2 p2Length =
        let p1End = p1 + p1Length    
        let p2End = p2 + p2Length
        (p2 >= p1 && p2 <= p1End) || (p2End >= p1 && p2 <= p1End)
    
    overlapInDim claim1.x claim1.width claim2.x claim2.width &&
    overlapInDim claim1.y claim1.height claim2.y claim2.height
claimsOverlap c1 c2
