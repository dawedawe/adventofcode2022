namespace AdventOfCode2022

module Day5 =

    open System.IO

    [<Literal>]
    let InputFile = "Day5Input.txt"

    let getStackCount (lines: string array) =
        lines
        |> Array.skipWhile (fun s -> not (System.Char.IsDigit(s.Trim()[0])))
        |> Array.head
        |> fun s -> s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> Array.last

    let getStacks count (lines: string array) =
        let stacks = Array.create count List.empty

        let lines' =
            lines |> Array.takeWhile (fun s -> not (System.Char.IsDigit(s.Trim()[0])))

        for line in lines' do
            for c in 0 .. count - 1 do
                let pos = 1 + c * 4

                if line[pos] <> ' ' then
                    stacks[c] <- List.append [ line[pos] ] stacks[c]

        stacks

    let parseMove (s: string) =
        let parts = s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        let amount = parts[1] |> int
        let source = parts[3] |> int
        let target = parts[5] |> int
        (amount, source, target)

    let moveFunc1 amount (sourceStack: char list) targetStack =
        List.append targetStack (List.take amount (sourceStack |> List.rev))

    let makeMove2 (stacks: list<char> array) f (amount, source, target) =
        let sourceStack = stacks[source - 1]
        let sourceStack' = List.take (sourceStack.Length - amount) sourceStack
        let targetStack = stacks[target - 1]

        let targetStack' = f amount sourceStack targetStack

        stacks[source - 1] <- sourceStack'
        stacks[target - 1] <- targetStack'
        stacks

    let getMovesAndStacks () =
        let lines = InputFile |> File.ReadAllLines
        let stackCount = lines |> getStackCount
        let stacks = lines |> getStacks stackCount
        let movesLines = lines |> Array.skipWhile (fun s -> not (s.StartsWith("move")))
        let moves = movesLines |> Array.map parseMove
        (moves, stacks)

    let part1 () =
        let (moves, stacks) = getMovesAndStacks ()
        moves |> Array.map (makeMove2 stacks moveFunc1) |> ignore
        stacks |> Array.map List.last |> System.String

    let moveFunc2 amount (sourceStack: char list) targetStack =
        List.append targetStack (sourceStack[(sourceStack.Length - amount) ..])

    let part2 () =
        let (moves, stacks) = getMovesAndStacks ()
        moves |> Array.map (makeMove2 stacks moveFunc2) |> ignore
        stacks |> Array.map List.last |> System.String
