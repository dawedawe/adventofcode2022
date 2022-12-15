namespace AdventOfCode2022

module Day13 =

    open System
    open System.IO

    [<Literal>]
    let InputFile = "Day13Input.txt"

    type Package =
        | IntVal of int
        | Pack of Package list

    let getVal (s: string) =
        let digits = s.ToCharArray() |> Array.takeWhile Char.IsDigit |> String
        let intVal = digits |> int |> IntVal
        let rest = s.Substring(digits.Length)
        (intVal, rest)

    let rec parseList (soFar: Package) (s: string) =
        let package =
            match soFar with
            | IntVal x -> failwith $"bad parser state {soFar}"
            | Pack p -> p

        match s.[0] with
        | i when Char.IsDigit i ->
            let (intVal, rest) = getVal s
            let package' = (List.append package [ intVal ]) |> Pack
            parseList package' rest
        | ',' -> parseList soFar s[1..]
        | ']' -> soFar, s.Substring(1)
        | '[' -> parsePackage soFar s
        | _ -> failwith $"bad parser state: {s}"

    and parsePackage (soFar: Package) (s: string) =
        if s = "" || s = "]" then
            (soFar, "")
        else
            match s[0] with
            | '[' ->
                let pack =
                    match soFar with
                    | Pack p -> p
                    | _ -> failwith "bad parser state"

                let lst, rest = parseList (Pack List.empty) (s.Substring(1))
                let pack' = List.append pack [ lst ] |> Pack
                parseList pack' rest
            | ']' -> parsePackage soFar (s.Substring(1))
            | ',' -> parsePackage soFar (s.Substring(1))
            | _ -> failwith $"parsePackage: bad parser state {s}"

    let parseOutmostPackage (s: string) =
        if s.StartsWith("[") then
            s.Substring(1) |> parseList (Pack List.empty)
            |> fst
        else
            failwith "no package"

    type Comparison =
        | WrongOrder
        | RightOrder
        | Continue

    let comparionsToInt (c: Comparison) =
        match c with
        | WrongOrder -> 1
        | RightOrder -> -1
        | Continue -> 0

    let compareValues (left: int) (right: int) =
        match left, right with
        | lVal, rVal when  lVal < rVal -> RightOrder
        | lVal, rVal when  lVal > rVal -> WrongOrder
        | lVal, rVal when  lVal = rVal -> Continue
        | _ -> failwith "not implemented"

    let rec comparePackages (left: Package) (right: Package) =
        match left, right with
        | IntVal l, IntVal r -> compareValues l r
        | IntVal _, Pack _ -> comparePackages (Pack [left]) right
        | Pack _, IntVal _ -> comparePackages left (Pack [right])
        | Pack l, Pack r ->
            Seq.zip l r
            |> Seq.map (fun (l, r) -> comparePackages l r)
            |> fun s ->
                Seq.tryFind (fun x -> x <> Continue) s
                |> Option.defaultValue (compareValues l.Length r.Length)
                    
    let part1 () =
        InputFile
        |> File.ReadAllLines
        |> Array.chunkBySize 3
        |> Array.map (fun a -> (a[0], a[1]))
        |> Array.map (fun (l, r) -> parseOutmostPackage l, parseOutmostPackage r)
        |> Array.map (fun (l, r) -> comparePackages l r)
        |> Array.indexed
        |> Array.filter (fun (i, x) -> x = RightOrder)
        |> Array.sumBy (fun (i, x) -> i + 1)

    let part2 () =
        let dividers =
            [|"[[2]]"; "[[6]]"|]
            |> Array.map parseOutmostPackage

        InputFile
        |> File.ReadAllLines
        |> Array.filter (fun s -> s <> "")
        |> Array.map parseOutmostPackage
        |> Array.append dividers
        |> Seq.sortWith (fun l r -> comparePackages l r |> comparionsToInt)
        |> Seq.indexed
        |> Seq.filter (fun (_, x) -> x = dividers[0] || x = dividers[1])
        |> Seq.map (fun x -> 1 + fst x)
        |> Seq.reduce (*)
