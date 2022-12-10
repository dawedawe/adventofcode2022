namespace AdventOfCode2022

module Day9 =

    open System.IO

    [<Literal>]
    let InputFile = "Day9Input.txt"

    type State =
        { Rope: (int * int) list
          Visited: (int * int) list }

        static member InitialPart1 =
            { Rope = [ (0, 0); (0, 0) ]
              Visited = [ (0, 0) ] }

        static member InitialPart2 =
            { Rope = List.init 10 (fun _ -> (0, 0))
              Visited = [ (0, 0) ] }

    let moveHead head motion =
        match motion with
        | 'U' -> (fst head, snd head + 1)
        | 'R' -> (fst head + 1, snd head)
        | 'D' -> (fst head, snd head - 1)
        | 'L' -> (fst head - 1, snd head)
        | _ -> failwith "Invalid motion"

    let tailMove headingKnot trailingKnot =
        match (headingKnot, trailingKnot) with
        | (hx, hy), (tx, ty) when hx = tx && ty + 2 = hy -> fun (x, y) -> (x, y + 1)
        | (hx, hy), (tx, ty) when hx = tx && ty - 2 = hy -> fun (x, y) -> (x, y - 1)
        | (hx, hy), (tx, ty) when hy = ty && tx + 2 = hx -> fun (x, y) -> (x + 1, y)
        | (hx, hy), (tx, ty) when hy = ty && tx - 2 = hx -> fun (x, y) -> (x - 1, y)
        | (hx, hy), (tx, ty) when abs (hx - tx) >= 2 || abs (hy - ty) >= 2 ->
            let xF = if hx > tx then (+) else (-)
            let yF = if hy > ty then (+) else (-)
            fun (x, y) -> (xF x 1, yF y 1)
        | _ -> id

    let moveSingle ({ Rope = rope; Visited = visited } as state) (motion: char) =
        let head = List.head rope
        let tail = List.last rope
        let head' = moveHead head motion
        let tail' = (tailMove head' tail) tail
        let rope' = [ head'; tail' ]

        let state' =
            { state with
                Rope = rope'
                Visited = visited @ [ tail' ] }

        state'

    let moveSinglePart2 ({ Rope = rope; Visited = visited } as state) (motion: char) =
        let head = List.head rope
        let head' = moveHead head motion
        let knots = [ head' ] @ (rope[1..9]) |> Array.ofList

        for i in 1..9 do
            knots[i] <- (tailMove knots[i - 1] knots[i]) knots[i]

        let rope' = knots |> List.ofSeq

        let state' =
            { state with
                Rope = rope'
                Visited = visited @ [ List.last rope' ] }

        state'

    let move mFunc state (motion: string) =
        let steps = motion.Substring(2) |> int
        let state' = Seq.init steps (fun _ -> motion[0]) |> Seq.fold mFunc state
        state'

    let part1 () =
        InputFile
        |> File.ReadAllLines
        |> Array.fold (fun state line -> move moveSingle state line) State.InitialPart1
        |> fun s -> List.distinct s.Visited
        |> List.length

    let part2 () =
        InputFile
        |> File.ReadAllLines
        |> Array.fold (fun state line -> move moveSinglePart2 state line) State.InitialPart2
        |> fun s -> List.distinct s.Visited
        |> List.length
