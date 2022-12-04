namespace AdventOfCode2022

module Day4 =

    open System.IO

    [<Literal>]
    let InputFile = "Day4Input.txt"

    let parseLine (s: string) =
        s.Split([| ','; '-' |])
        |> Array.map int
        |> function
            | [| s1Start; s1End; s2Start; s2End |] -> (s1Start, s1End, s2Start, s2End)
            | _ -> failwith "can't parse"

    let hasFullContainment (s1Start, s1End, s2Start, s2End) =
        s2Start >= s1Start && s2End <= s1End || s1Start >= s2Start && s1End <= s2End

    let part1 () =
        InputFile
        |> File.ReadAllLines
        |> Array.map parseLine
        |> Array.sumBy (fun l -> if hasFullContainment l then 1 else 0)

    let hasOverlap (s1Start, s1End, s2Start, s2End) =
        s2Start >= s1Start && s2Start <= s1End
        || s2End >= s1Start && s2End <= s1End
        || s1Start >= s2Start && s1Start <= s2End
        || s1End >= s2Start && s1End <= s2End

    let part2 () =
        InputFile
        |> File.ReadAllLines
        |> Array.map parseLine
        |> Array.sumBy (fun l -> if hasOverlap l then 1 else 0)
