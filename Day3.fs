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

    let prioF = (fun c -> if System.Char.IsLower c then int c - 96 else int c - 38)

    let prioritize s =
        s |> split |> findDouble |> Set.map prioF |> Set.toArray |> Array.sum

    let part1 () =
        InputFile |> File.ReadAllLines |> Array.sumBy prioritize

    let findBadge =
        function
        | [| (s1: string); (s2: string); (s3: string) |] ->
            Set.intersect (s1.ToCharArray() |> Set.ofArray) (s2.ToCharArray() |> Set.ofArray)
            |> Set.intersect (s3.ToCharArray() |> Set.ofArray)
            |> Set.toArray
            |> Array.head
        | _ -> failwith "not a group array of 3"

    let part2 () =
        let lines = InputFile |> File.ReadAllLines

        lines
        |> Array.splitInto (lines.Length / 3)
        |> Array.map findBadge
        |> Seq.sumBy prioF
