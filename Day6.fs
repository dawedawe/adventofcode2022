namespace AdventOfCode2022

module Day6 =

    open System.IO

    [<Literal>]
    let InputFile = "Day6Input.txt"

    let findStart length (signal: string) =
        signal.ToCharArray()
        |> Array.windowed length
        |> Array.findIndex (fun w -> Array.distinct w |> Array.length = length)
        |> (+) length


    let part1 () =
        InputFile |> File.ReadAllText |> findStart 4

    let part2 () =
        InputFile |> File.ReadAllText |> findStart 14
