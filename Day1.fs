namespace AdventOfCode2022

module Day1 =

    open System.IO

    [<Literal>]
    let InputFile = "Day1Input.txt"

    let rec grouper lines =
        let group = Array.takeWhile (fun x -> x <> "") lines |> Array.map int
        let rest = lines.[group.Length + 1 ..]
        if rest.Length = 0 then [ group ] else group :: grouper rest

    let part1 () =
        InputFile |> File.ReadAllLines |> grouper |> List.maxBy Array.sum |> Array.sum

    let part2 () =
        InputFile
        |> File.ReadAllLines
        |> grouper
        |> List.sortByDescending (Array.sum >> abs)
        |> List.take 3
        |> List.sumBy Array.sum
