namespace AdventOfCode2022

module Day3 =

    open System.IO

    [<Literal>]
    let InputFile = "Day3Input.txt"

    let split (s: string) =
        let comp1 = s.Substring(0, s.Length / 2)
        let comp2 = s.Substring(comp1.Length)
        comp1, comp2

    let findDouble ((comp1, comp2): (string * string)) =
        let set1 = comp1.ToCharArray() |> Set.ofArray
        let set2 = comp2.ToCharArray() |> Set.ofArray
        Set.intersect set1 set2

    let prioritize s =
        s
        |> split
        |> findDouble
        |> Set.map (fun c -> if System.Char.IsLower c then int c - 96 else int c - 38)
        |> Set.toArray
        |> Array.sum

    let part1 () =
        InputFile |> File.ReadAllLines |> Array.sumBy prioritize
