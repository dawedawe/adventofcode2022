namespace AdventOfCode2022

module Day11 =

    open System.IO

    [<Literal>]
    let InputFile = "Day11Input.txt"

    type Monkey =
        { Id: int
          Items: int64 list
          Operation: (int64 -> int64)
          Devisor: int64
          ThrowToIfTrue: int
          ThrowToIfFalse: int
          Inspections: int64 }

    let parseOperation =
        function
        | [| "*"; "old" |] -> fun x -> x * x
        | [| "+"; "old" |] -> fun x -> x + x
        | [| "*"; y |] -> fun x -> x * (int64 y)
        | [| "+"; y |] -> fun x -> x + (int64 y)
        | _ -> failwith "Invalid operation"

    let parseMonkey (lines: string[]) =
        let lastWordAsInt (s: string) = s.Split([| ' ' |]) |> Array.last |> int

        let id = lines[ 0 ].Substring(7, 1) |> int

        let items =
            lines[1]
                .Substring(18)
                .Split([| ','; ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int64
            |> Array.toList

        let operation =
            lines[2]
                .Substring(23)
                .Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
            |> parseOperation

        let devisor = lines[3] |> lastWordAsInt |> int64
        let ifTrue = lines[4] |> lastWordAsInt
        let ifFalse = lines[5] |> lastWordAsInt

        { Id = id
          Items = items
          Operation = operation
          Devisor = devisor
          ThrowToIfTrue = ifTrue
          ThrowToIfFalse = ifFalse
          Inspections = 0L }

    let playMonkeyItem (minFunc: int64 -> int64) monkey item =
        let newWorryLevel = monkey.Operation item
        let newWorryLevelDivided = minFunc newWorryLevel
        let testResult = newWorryLevelDivided % monkey.Devisor = 0

        let target =
            if testResult then
                monkey.ThrowToIfTrue
            else
                monkey.ThrowToIfFalse

        let f =
            fun (monkeys: Monkey array) ->
                Array.updateAt
                    target
                    { monkeys[target] with Items = monkeys[target].Items @ [ newWorryLevelDivided ] }
                    monkeys

        f

    let playMonkey minFunc (monkeys: Monkey array) monkeyId =
        let monkey = monkeys[monkeyId]

        let updateFunctions =
            List.map (playMonkeyItem minFunc monkey) monkey.Items |> Array.ofList

        let monkeys' = Array.fold (fun monkeys f -> f monkeys) monkeys updateFunctions

        let monkey' =
            { monkey with
                Items = []
                Inspections = monkey.Inspections + int64 monkey.Items.Length }

        Array.updateAt monkey.Id monkey' monkeys'

    let playRound minFunc monkeys =
        let monkeys' =
            List.fold (playMonkey minFunc) monkeys [ 0 .. Seq.length monkeys - 1 ]

        monkeys'

    let play minFunc rounds monkeys =
        Array.fold (fun monkeys _ -> playRound minFunc monkeys) monkeys [| 1..rounds |]

    let part1 () =
        InputFile
        |> File.ReadAllLines
        |> Array.chunkBySize 7
        |> Array.map parseMonkey
        |> play (fun x -> x / 3L) 20
        |> Array.sortByDescending (fun x -> x.Inspections)
        |> fun ms -> ms[0].Inspections * ms[1].Inspections

    let minFuncPart2 devisors =
        let p = Array.reduce (*) devisors
        fun x -> x % p

    let part2 () =
        InputFile
        |> File.ReadAllLines
        |> Array.chunkBySize 7
        |> Array.map parseMonkey
        |> fun monkeys ->
            let f = minFuncPart2 (monkeys |> Array.map (fun m -> m.Devisor))
            play f 10000 monkeys
        |> Array.sortByDescending (fun x -> x.Inspections)
        |> fun ms -> ms[0].Inspections * ms[1].Inspections
