open System
open System.IO

type ColorSet = { Color: string; Count: int }
type Game = { Id: int; Sets: ColorSet list list }

let parse (input: string) =
    input.Split("\n")
    |> Seq.map (fun g ->
        let parts = g.Split(":")
        let id = parts[0].Split(" ")[1] |> Int32.Parse

        let sets =
            parts[1].Split(";")
            |> Seq.map (fun s ->
                s.Split(",")
                |> Seq.map (fun setElement ->
                    let numberAndColor = setElement.Trim().Split(" ")

                    { Color = numberAndColor[1]
                      Count = Int32.Parse numberAndColor[0] })
                |> Seq.toList)
            |> Seq.toList

        { Id = id; Sets = sets })
    |> Seq.toList

let solvePartOne maxRed maxGreen maxBlue (games: Game seq) =
    let isPossible (game: Game) =
        game.Sets
        |> Seq.forall (fun s ->
            let countByColor color =
                match (s |> Seq.tryFind (fun ss -> ss.Color = color)) with
                | Some gameSet -> gameSet.Count
                | None -> 0

            let redCount = countByColor "red"
            let greenCount = countByColor "green"
            let blueCount = countByColor "blue"

            if (redCount > maxRed || greenCount > maxGreen || blueCount > maxBlue) then
                false
            else
                true)

    games |> Seq.where isPossible |> Seq.map (fun g -> g.Id) |> Seq.sum

let solvePartTwo (games: Game seq) =
    games
    |> Seq.map (fun game ->
        game.Sets
        |> Seq.collect (fun set -> set)
        |> Seq.groupBy (fun set -> set.Color)
        |> Seq.map (fun (_, set) -> set |> Seq.maxBy (fun set -> set.Count))
        |> Seq.map (fun cs -> cs.Count)
        |> Seq.reduce (fun prev next -> prev * next))
    |> Seq.sum

let input = parse (File.ReadAllText "day2.txt")

let part1 = solvePartOne 12 13 14 input
let part2 = solvePartTwo input
