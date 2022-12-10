namespace AdventOfCode2022

module Day9 =

    open System.IO

    [<Literal>]
    let InputFile = "Day9Input.txt"

    type State =
        { HeadPos: int * int
          TailPos: int * int
          Visited: (int * int) list }

        static member Initial =
            { HeadPos = (0, 0)
              TailPos = (0, 0)
              Visited = [ (0, 0) ] }

    let moveSingle
        ({ HeadPos = head
           TailPos = tail
           Visited = visited } as state)
        (motion: char)
        =
        let head' =
            match motion with
            | 'U' -> (fst head, snd head + 1)
            | 'R' -> (fst head + 1, snd head)
            | 'D' -> (fst head, snd head - 1)
            | 'L' -> (fst head - 1, snd head)
            | _ -> failwith "Invalid motion"

        let tailMove =
            match (head', tail) with
            | (hx, hy), (tx, ty) when hx = tx && ty + 2 = hy -> fun (x, y) -> (x, y + 1)
            | (hx, hy), (tx, ty) when hx = tx && ty - 2 = hy -> fun (x, y) -> (x, y - 1)
            | (hx, hy), (tx, ty) when hy = ty && tx + 2 = hx -> fun (x, y) -> (x + 1, y)
            | (hx, hy), (tx, ty) when hy = ty && tx - 2 = hx -> fun (x, y) -> (x - 1, y)
            | (hx, hy), (tx, ty) when abs (hx - tx) >= 2 || abs (hy - ty) >= 2 ->
                let xF = if hx > tx then (+) else (-)
                let yF = if hy > ty then (+) else (-)
                fun (x, y) -> (xF x 1, yF y 1)
            | _ -> id

        let tail' = tailMove tail

        let state' =
            { state with
                HeadPos = head'
                TailPos = tail'
                Visited = visited @ [ tail' ] }

        state'

    let move state (motion: string) =
        let steps = motion.Substring(2) |> int
        let state' = Seq.init steps (fun _ -> motion[0]) |> Seq.fold moveSingle state
        state'

    let part1 () =
        InputFile
        |> File.ReadAllLines
        |> Array.fold (fun state line -> move state line) State.Initial
        |> fun s -> List.distinct s.Visited
        |> List.length
