namespace AdventOfCode2022

module Day14 =

    open System.IO

    [<Literal>]
    let InputFile = "Day14Input.txt"

    let parse (line: string) =
        line.Split(" -> ")
        |> Array.windowed 2
        |> Array.map (fun x ->
            let a = x.[0].Split(',')
            let b = x.[1].Split(',')
            (int a.[0], int a.[1]), (int b.[0], int b.[1]))

    let insertRocks (map: Map<(int * int), char>) ((startX, startY), (endX, endY)) =
        let (startD, endD, constant) =
            if startX <> endX then startX, endX, 1 else startY, endY, 0
            |> fun (a, b, c) -> if a < b then (a, b, c) else (b, a, c)

        let xs =
            [ startD..endD ]
            |> Seq.map (fun x -> if constant = 0 then (startX, x) else (x, startY))

        let map' =
            Seq.fold (fun (m: Map<(int * int), char>) (x, y) -> m.Add((x, y), '#')) map xs

        map'

    let constructPath map path = Array.fold insertRocks map path

    let rec tickleDown abbysY (map: Map<(int * int), char>) (curX, curY) =
        let lower = (curX, curY + 1)
        let diagLeft = (curX - 1, curY + 1)
        let diagRight = (curX + 1, curY + 1)

        [ lower; diagLeft; diagRight ]
        |> Seq.map (fun k -> k, map.TryFind k)
        |> Seq.tryFind (fun (_, v) -> v = None)
        |> function
            | Some(k, _) ->
                let map' = map.Remove(curX, curY)
                let map'' = map'.Add(k, 'o')

                if snd k = abbysY then
                    map'', false
                else
                    tickleDown abbysY map'' k
            | None -> map, true

    let rec pourSand abbysY atRest (map: Map<(int * int), char>) =
        let map', resting = tickleDown abbysY map (500, 0)

        if resting then
            pourSand abbysY (atRest + 1) map'
        else
            atRest

    let part1 () =
        let cave =
            InputFile
            |> File.ReadAllLines
            |> Array.map parse
            |> Array.fold constructPath Map.empty

        let abbysY = cave.Keys |> Seq.map (fun (_, y) -> y) |> Seq.max
        pourSand abbysY 0 cave

    let rec tickleDown2 (map: Map<(int * int), char>) (curX, curY) =
        let lower = (curX, curY + 1)
        let diagLeft = (curX - 1, curY + 1)
        let diagRight = (curX + 1, curY + 1)

        [ lower; diagLeft; diagRight ]
        |> Seq.map (fun k -> k, map.TryFind k)
        |> Seq.tryFind (fun (_, v) -> v = None)
        |> function
            | Some(k, _) ->
                let map' = map.Remove(curX, curY)
                let map'' = map'.Add(k, 'o')
                tickleDown2 map'' k
            | None -> map

    let rec pourSand2 atRest (map: Map<(int * int), char>) =
        let start = (500, 0)
        let map' = tickleDown2 (map.Add(start, 'o')) start
        let start = map'.TryFind(500, 0)

        match start with
        | Some 'o' -> atRest + 1
        | _ -> pourSand2 (atRest + 1) map'

    let part2 () =
        let cave =
            InputFile
            |> File.ReadAllLines
            |> Array.map parse
            |> Array.fold constructPath Map.empty

        let floorY = 2 + (cave.Keys |> Seq.map (fun (_, y) -> y) |> Seq.max)
        let minX = cave.Keys |> Seq.map (fun (x, _) -> x) |> Seq.min
        let maxX = cave.Keys |> Seq.map (fun (x, _) -> x) |> Seq.max

        let cave' =
            Seq.fold
                (fun (m: Map<(int * int), char>) x -> m.Add((x, floorY), '#'))
                cave
                [ (minX - 1000) .. (maxX + 1000) ]

        pourSand2 0 cave'
