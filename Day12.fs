namespace AdventOfCode2022

module Day12 =

    open System.IO
    open FSharp.Collections

    [<Literal>]
    let InputFile = "Day12Input.txt"

    type Path = (int * int) list

    let findStart (heigthmap: char array array) =
        let row = Array.findIndex (fun row -> Array.exists (fun c -> c = 'S') row) heigthmap
        let col = Array.findIndex (fun c -> c = 'S') heigthmap.[row]
        (col, row)

    let isLegalPos (heigthmap: char array array) (nextX, nextY) =
        let maxRow = heigthmap.Length - 1
        let maxCol = heigthmap.[0].Length - 1
        nextX <= maxCol && nextY <= maxRow && nextX >= 0 && nextY >= 0

    let isLegalHeight (heigthmap: char array array) (curX, curY) (nextX, nextY) =
        let currentHeight =
            if heigthmap[curY][curX] = 'S' then
                'a' |> int
            else
                heigthmap[curY][curX] |> int

        let nextHeight =
            if heigthmap[nextY][nextX] = 'E' then
                'z' |> int
            else
                heigthmap[nextY][nextX] |> int

        nextHeight - 1 <= currentHeight

    let getNeighbours (heigthmap: char array array) (path: Path) =
        let (curX, curY) = path.[path.Length - 1]

        let neighbours =
            [ (curX - 1, curY); (curX + 1, curY); (curX, curY - 1); (curX, curY + 1) ]
            |> List.filter (isLegalPos heigthmap)
            |> List.filter (fun (x, y) -> not (List.contains (x, y) path))
            |> List.filter (isLegalHeight heigthmap (curX, curY))

        neighbours

    let bfs heigthmap start =
        let firstPath = List.singleton start
        let mutable goalPaths = None

        let queue = System.Collections.Generic.Queue<Path>()

        let higherNeighbours = getNeighbours heigthmap firstPath

        for n in higherNeighbours do
            let newPath = firstPath @ [ n ]
            let (newX, newY) = List.last newPath

            if heigthmap[newY][newX] = 'E' && Option.isNone goalPaths then
                goalPaths <- Some newPath
            else
                queue.Enqueue newPath

        while (queue.Count <> 0 && Option.isNone goalPaths) do
            let currentPath = queue.Dequeue()
            let neighbours = getNeighbours heigthmap currentPath

            for n in neighbours do
                let newPath = currentPath @ [ n ]
                let (newX, newY) = List.last newPath

                if heigthmap[newY][newX] = 'E' && Option.isNone goalPaths then
                    goalPaths <- Some newPath
                else
                    let alreadyVisited = queue.ToArray() |> Array.exists (fun p -> List.contains n p)

                    if not alreadyVisited then
                        queue.Enqueue newPath

        match goalPaths with
        | Some path -> path.Tail |> Some
        | None -> None

    let part1 () =
        InputFile
        |> File.ReadAllLines
        |> Array.map (fun s -> s.ToCharArray())
        |> fun m -> bfs m (findStart m)
        |> Option.map (fun p -> p.Length)

    let findStarts (heigthmap: char array array) =
        seq {
            for y in 0 .. heigthmap.Length - 1 do
                for x in 0 .. heigthmap.[0].Length - 1 do
                    if heigthmap.[y].[x] = 'S' || heigthmap.[y].[x] = 'a' then
                        yield (x, y)
        }
        |> Seq.toList

    let rec findTrails (heigthmap: char array array) pathsFound starts =
        match starts with
        | [] -> pathsFound
        | s :: rest ->
            let newPath = bfs heigthmap s

            match newPath with
            | Some path ->
                let tail = path.Tail

                let subStartIdxs =
                    tail
                    |> List.indexed
                    |> List.filter (fun (idx, (x, y)) -> heigthmap.[y].[x] = 'a' || heigthmap.[y].[x] = 'S')
                    |> List.map fst

                let subPaths = subStartIdxs |> List.map (fun idx -> tail.[idx..])
                let subStarts = subPaths |> List.map List.head
                let pathsFound' = path :: pathsFound @ subPaths
                let rest' = rest |> List.filter (fun (x, y) -> not (List.contains (x, y) subStarts))
                findTrails heigthmap pathsFound' rest'
            | None -> findTrails heigthmap pathsFound rest


    let part2 () =
        InputFile
        |> File.ReadAllLines
        |> Array.map (fun s -> s.ToCharArray())
        |> fun m ->
            let starts = findStarts m
            findTrails m [] starts
        |> Seq.minBy (fun p -> p.Length)
        |> Seq.length
