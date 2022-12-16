namespace AdventOfCode2022

module Day15 =

    open System.IO

    [<Literal>]
    let InputFile = "Day15Input.txt"

    type Reading =
        { X: int
          Y: int
          BeaconX: int
          BeaconY: int
          Dist: int }

    let calcManhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

    let regex =
        System.Text.RegularExpressions.Regex(
            @"Sensor at x=(-*\d+), y=(-*\d+): closest beacon is at x=(-*\d+), y=(-*\d+)"
        )

    let parse (line: string) =
        let m = regex.Match(line)
        let x = int m.Groups.[1].Value
        let y = int m.Groups.[2].Value
        let bx = int m.Groups.[3].Value
        let by = int m.Groups.[4].Value

        { X = x
          Y = y
          BeaconX = bx
          BeaconY = by
          Dist = calcManhattan (x, y) (bx, by) }

    let part1 () =
        let y = 2000000
        let readings = InputFile |> File.ReadAllLines |> Array.map parse

        let minX =
            readings
            |> Array.map (fun r -> [| r.X; r.BeaconX |])
            |> Array.concat
            |> Array.min

        let maxX =
            readings
            |> Array.map (fun r -> [| r.X; r.BeaconX |])
            |> Array.concat
            |> Array.max

        let maxManhattan = readings |> Array.map (fun r -> r.Dist) |> Array.max

        seq {
            for x in (minX - maxManhattan) .. (maxX + maxManhattan) do
                let reachable =
                    readings
                    |> Array.exists (fun s ->
                        let d = calcManhattan (x, y) (s.X, s.Y)
                        d <= s.Dist)

                let knownBeacon =
                    readings |> Array.exists (fun s -> (s.BeaconX, s.BeaconY) = (x, y))

                if reachable && not knownBeacon then yield 1 else yield 0
        }
        |> Seq.sum

    let constructEdges (r: Reading) =
        let d = r.Dist + 1

        seq {
            for i in 0..d do
                yield r.X + d - i, r.Y - i
                yield r.X + d - i, r.Y + i
                yield r.X - d + i, r.Y + i
                yield r.X - d + i, r.Y - i
        }
        |> Array.ofSeq


    let part2 () =
        let minX, maxX = 0, 4000000
        let minY, maxY = 0, 4000000
        let readings = InputFile |> File.ReadAllLines |> Array.map parse

        let edges =
            readings
            |> Array.map constructEdges
            |> Array.concat
            |> Array.filter (fun (x, y) -> x >= minX && x <= maxX && y >= minY && y <= maxY)
            |> Array.distinct


        seq {
            for (x, y) in edges do
                let reachable =
                    readings
                    |> Array.exists (fun s ->
                        let d = calcManhattan (x, y) (s.X, s.Y)
                        d <= s.Dist)

                let knownBeacon =
                    readings |> Array.exists (fun s -> (s.BeaconX, s.BeaconY) = (x, y))

                if not reachable && not knownBeacon then
                    yield (x, y)
        }
        |> Seq.head
        |> fun (x, y) -> int64 x * 4000000L + int64 y
