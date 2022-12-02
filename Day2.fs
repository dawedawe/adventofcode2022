namespace AdventOfCode2022

module Day2 =

    open System.IO

    type Shape =
        | Rock
        | Paper
        | Scissors

    let parseElfShape (c: char) =
        match c with
        | 'A' -> Rock
        | 'B' -> Paper
        | 'C' -> Scissors
        | _ -> failwith "Invalid shape"

    let parseOwnShape (c: char) =
        match c with
        | 'X' -> Rock
        | 'Y' -> Paper
        | 'Z' -> Scissors
        | _ -> failwith "Invalid shape"

    let scoreOutcome elf own =
        match (elf, own) with
        | (Rock, Paper) -> 6
        | (Rock, Scissors) -> 0
        | (Rock, Rock) -> 3
        | (Paper, Paper) -> 3
        | (Paper, Scissors) -> 6
        | (Paper, Rock) -> 0
        | (Scissors, Paper) -> 0
        | (Scissors, Scissors) -> 3
        | (Scissors, Rock) -> 6

    let scoreShape shape =
        match shape with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let scoreRound elf own =
        let outcome = scoreOutcome elf own
        let shape = scoreShape own
        outcome + shape

    [<Literal>]
    let InputFile = "Day2Input.txt"

    let part1 () =
        InputFile
        |> File.ReadAllLines
        |> Array.map (fun line ->
            let elf = line.[0] |> parseElfShape
            let own = line.[2] |> parseOwnShape
            scoreRound elf own)
        |> Array.sum

    type DesiredOutcome =
        | Win
        | Lose
        | Draw

    let parseDesiredOutcome c =
        match c with
        | 'X' -> Lose
        | 'Y' -> Draw
        | 'Z' -> Win
        | _ -> failwith "Invalid desired outcome"

    let choose elfHand desiredOutcome =
        match (elfHand, desiredOutcome) with
        | (Rock, Lose) -> Scissors
        | (Rock, Win) -> Paper
        | (Paper, Lose) -> Rock
        | (Paper, Win) -> Scissors
        | (Scissors, Lose) -> Paper
        | (Scissors, Win) -> Rock
        | (_, Draw) -> elfHand

    let part2 () =
        InputFile
        |> File.ReadAllLines
        |> Array.map (fun line ->
            let elf = line.[0] |> parseElfShape
            let own = line.[2] |> parseDesiredOutcome |> choose elf
            scoreRound elf own)
        |> Array.sum
