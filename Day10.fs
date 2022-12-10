namespace AdventOfCode2022

module Day10 =

    open System.IO

    type State =
        { RegX: int
          Cycle: int }

        static member Init = { RegX = 1; Cycle = 0 }

    type Instruction =
        | Noop
        | AddX of int

    [<Literal>]
    let InputFile = "Day10Input.txt"

    let parse (line: string) =
        match line with
        | "noop" -> [| Noop |]
        | addX -> [| Noop; Noop; AddX(addX.Substring(5) |> int) |]

    let rec execute values (state: State) (instructions: Instruction list) =
        match instructions with
        | [] -> values
        | Noop :: rest ->
            let values' = values @ [ state.RegX ]
            execute values' { state with Cycle = state.Cycle + 1 } rest
        | AddX x :: rest ->
            let state' = { state with RegX = state.RegX + x }
            execute values state' rest

    let part1 () =
        let cyclesToSum = [ 20; 60; 100; 140; 180; 220 ]

        InputFile
        |> File.ReadAllLines
        |> Array.map parse
        |> Array.concat
        |> List.ofArray
        |> execute [ 1 ] State.Init
        |> fun values -> cyclesToSum |> List.map (fun i -> values[i] * i) |> List.sum

    let printRow (values: int list) =
        for i in 0..39 do
            let sprite = [ values[i] - 1; values[i]; values[i] + 1 ]
            if List.contains i sprite then printf "#" else printf "."

        printfn ""

    let part2 () =
        let cycleValues =
            InputFile
            |> File.ReadAllLines
            |> Array.map parse
            |> Array.concat
            |> List.ofArray
            |> execute [ 1 ] State.Init

        cycleValues[1..] |> List.splitInto 6 |> List.iter printRow
