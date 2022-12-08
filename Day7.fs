namespace AdventOfCode2022

module Day7 =

    open System.IO

    [<Literal>]
    let InputFile = "Day7Input.txt"

    let rec update keys (tree: Map<string, int64>) f =
        match keys with
        | [] -> tree
        | k :: rest ->
            let tree' = tree.Change(k, f)
            update rest tree' f

    let addToCwd cwd dir =
        if cwd = "/" then "/" + dir else cwd + "/" + dir

    let rec buildMap (tree: Map<string, int64>) (cwd: string) (lines: string array) =
        match lines with
        | [||] -> tree
        | ss when ss[ 0 ].StartsWith("$ cd ..") ->
            let idx = cwd.LastIndexOf("/")
            let cwd' = if idx = 0 then "/" else cwd.Substring(0, idx)
            buildMap tree cwd' ss[1..]
        | ss when ss[0] = "$ cd /" ->
            let cwd' = "/"
            buildMap tree cwd' ss[1..]
        | ss when ss[ 0 ].StartsWith("$ cd ") ->
            let dir = ss[ 0 ].Substring(5)
            let cwd' = addToCwd cwd dir
            buildMap tree cwd' ss[1..]
        | ss when ss[ 0 ].StartsWith("$ ls") -> buildMap tree cwd ss[1..]
        | ss when ss[ 0 ].StartsWith("dir ") ->
            let dir = ss[ 0 ].Substring(4)
            let path = addToCwd cwd dir
            let tree' = tree.Add(path, 0)
            buildMap tree' cwd ss[1..]
        | ss when System.Char.IsDigit(ss[0][0]) ->
            let size = ss[ 0 ].Split(' ') |> Array.head |> int64

            let f =
                function
                | Some v -> Some(v + size)
                | None -> Some size

            let tree' = tree.Change(cwd, f)

            let keysToUpdate =
                tree'.Keys |> Seq.toList |> List.filter (fun k -> cwd.StartsWith(k) && k <> cwd)

            let tree'' = update keysToUpdate tree' f

            buildMap tree'' cwd ss[1..]
        | _ -> failwith $"not supported {lines[0]}"

    let part1 () =
        InputFile
        |> File.ReadAllLines
        |> buildMap Map.empty ""
        |> Map.filter (fun k v -> v <= 100000)
        |> Map.values
        |> Seq.sum

    let part2 () =
        InputFile
        |> File.ReadAllLines
        |> buildMap Map.empty ""
        |> fun map ->
            let stillNeeded = 30000000L - (70000000L - map["/"])
            Map.filter (fun k v -> v >= stillNeeded) map
        |> Map.values
        |> Seq.min
