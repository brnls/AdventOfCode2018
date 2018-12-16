module Day4

open System.IO
open System

type GuardEventType = 
|BeginShift of int
|FallAsleep
|WakeUp

type ShiftEvent = {
    EventTime: DateTime
    Type: GuardEventType
}

type Shift = {
    Date: DateTime
    Events: ShiftEvent []
}

type GuardRecord = {
    GuardId: int
    Shifts: Shift []
}

type MinuteRecord =
|Awake
|Asleep

let part1And2() = 
    let data = File.ReadAllLines("day4input")

    let parseEvent (line: string) =
        let arr = line.Split()
        let date = DateTime.Parse(sprintf "%s %s" (arr.[0].Trim('[')) (arr.[1].Trim(']')))
        let eventType =
            if line.Contains("begins") then 
                let guardId = Int32.Parse(arr.[3].Replace("#", ""))
                BeginShift guardId
            elif line.Contains("falls") then FallAsleep
            else WakeUp
        {
            EventTime = date
            Type = eventType
        }

    let guardRecords = 
        data 
        |> Array.map parseEvent
        |> Array.groupBy (fun x -> x.EventTime.AddHours(1.).Date)
        |> Array.map (fun (date, events) -> 
            let eventsSorted = events |> Array.sortBy (fun x -> x.EventTime)
            let guardId = 
                match eventsSorted.[0].Type with
                |BeginShift guardId -> guardId
                |_ -> failwith (sprintf "no shift begin event %A" eventsSorted)
            guardId,
            {
                Date = date
                Events = eventsSorted
            }
        )
        |> Array.groupBy fst
        |> Array.map(fun (guardId, shiftList) ->{
            GuardId = guardId
            Shifts = (shiftList |> Array.map snd)
        })

    let getSleepMinuteRecord shift =
        let sleepMinuteRecord = (Array.init 60 (fun _ -> Awake))
        for (event1, event2) in shift.Events |> Array.pairwise do
            match event1.Type, event2.Type with
            |FallAsleep, WakeUp -> 
                for i in (event1.EventTime.Minute) .. (event2.EventTime.Minute - 1) do
                    sleepMinuteRecord.[i] <- Asleep
            |_ -> ()
        sleepMinuteRecord

    let sumSleepTime sleepMinuteRecords = 
        sleepMinuteRecords |> Array.sumBy(fun x -> 
            x |> Array.sumBy (fun y -> match y with Asleep -> 1 | Awake -> 0)
        )

    let guard, sleepRecords, _ = 
        guardRecords 
        |> Seq.map (fun x -> 
            let minuterecords = x.Shifts |> Array.map getSleepMinuteRecord
            x, minuterecords, sumSleepTime minuterecords)
        |> Seq.maxBy (fun (_, _, time) -> time)

    let sleepMinuteFrequency minuteRecords = 
        minuteRecords
        |> Array.fold (fun s t ->
            s |> Array.map2 (fun min sum -> match min with Awake -> sum | Asleep -> sum + 1) t
            ) (Array.zeroCreate 60)

    let mostSleepMinute =
        sleepMinuteFrequency sleepRecords 
        |> Array.mapi(fun i x -> i, x)
        |> Array.maxBy snd

    let mostFrequentSleepMinute = 
        guardRecords 
        |> Seq.map(fun x -> 
            let minuteRecords = x.Shifts |> Array.map getSleepMinuteRecord
            let minute, highestFrequency = 
                sleepMinuteFrequency minuteRecords 
                |> Array.mapi (fun i x -> i, x)
                |> Array.maxBy snd
            x, minute, highestFrequency)
        |> Seq.maxBy (fun (_, _, freq) -> freq)
        |> fun (guard, minute, _) -> guard.GuardId * minute

    (fst mostSleepMinute) * guard.GuardId,
    mostFrequentSleepMinute
