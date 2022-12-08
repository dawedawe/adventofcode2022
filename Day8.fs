namespace AdventOfCode2022

module Day8 =

    open System.IO

    [<Literal>]
    let InputFile = "Day8Input.txt"

    let getVertical (grid: int[][]) x =
        seq {
            for y in 0 .. grid.Length - 1 do
                yield grid.[y].[x]
        }
        |> Array.ofSeq

    let getNorth (grid: int[][]) x y =
        getVertical grid x |> Array.take y |> Array.rev

    let getEast (grid: int[][]) x y = grid[y][x + 1 ..]

    let getSouth (grid: int[][]) x y =
        getVertical grid x |> Array.skip (y + 1)

    let getWest (grid: int[][]) x y = grid[y][0 .. x - 1] |> Array.rev

    let countVisible (grid: int[][]) =
        let lines = grid.Length
        let columns = grid[0].Length

        seq {
            for y in 0 .. lines - 1 do
                for x in 0 .. columns - 1 do
                    let height = grid[y][x]
                    let north = getNorth grid x y |> Seq.forall (fun a -> a < height)
                    let east = getEast grid x y |> Seq.forall (fun a -> a < height)
                    let south = getSouth grid x y |> Seq.forall (fun a -> a < height)
                    let west = getWest grid x y |> Seq.forall (fun a -> a < height)
                    let visible = north || east || south || west
                    yield if visible then 1 else 0
        }
        |> Seq.sum

    let takeView height (view: int[]) =
        Array.tryFindIndex (fun a -> a >= height) view
        |> function
            | None -> view.Length
            | Some i -> i + 1

    let calcScenic (grid: int[][]) =
        let lines = grid.Length
        let columns = grid[0].Length


        seq {
            for y in 0 .. lines - 1 do
                for x in 0 .. columns - 1 do
                    let height = grid[y][x]
                    let north = getNorth grid x y |> takeView height
                    let east = getEast grid x y |> takeView height
                    let south = getSouth grid x y |> takeView height
                    let west = getWest grid x y |> takeView height
                    yield north * east * south * west
        }
        |> Seq.max

    let part1 () =
        InputFile
        |> File.ReadAllLines
        |> Array.map (fun a -> a.ToCharArray() |> Array.map (string >> int))
        |> countVisible

    let part2 () =
        InputFile
        |> File.ReadAllLines
        |> Array.map (fun a -> a.ToCharArray() |> Array.map (string >> int))
        |> calcScenic
