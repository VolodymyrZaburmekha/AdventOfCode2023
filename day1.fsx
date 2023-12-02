open System
open System.IO

let data = File.ReadAllLines "day.txt"

let findFirstAndLast line =
    let first, last =
        (line |> Seq.find Char.IsDigit), (line |> Seq.findBack Char.IsDigit)

    Int32.Parse($"%c{first}%c{last}")

let pat1 = data |> Seq.map findFirstAndLast |> Seq.sum

let part2 =
    data
    |> Seq.map (fun l ->
        let replacements =
            [ ("one", "one1one")
              ("two", "two2two")
              ("three", "three3three")
              ("four", "four4four")
              ("five", "five5five")
              ("six", "six6six")
              ("seven", "seven7seven")
              ("eight", "eight8eight")
              ("nine", "nine9nine") ]

        let alteredStr =
            replacements
            |> Seq.fold (fun (state: string) (old, newVal) -> state.Replace(old, newVal)) l

        findFirstAndLast alteredStr)
    |> Seq.sum
