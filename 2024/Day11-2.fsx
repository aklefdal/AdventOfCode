#load "Utils.fsx"

open System.IO
open Microsoft.FSharp.Core

// Input
let input = File.ReadAllText "input11.txt"

let inputSample = """125 17"""


// Part 1
type Stone = int64

type StoneCount = { Stone: Stone; Count: int64 }

let parseStones (s: string) : Stone list =
    s.Split(' ') |> Array.map int64 |> Array.toList

let splitString (i: int) (s: string) : Stone list =
    let head = s.Substring(0, i) |> int64
    let tail = s.Substring(i) |> int64
    [ head; tail ]

let blinkOn (stone: Stone) : Stone list =
    if stone = 0L then
        [ 1L ]
    else
        let stoneAsString = string stone
        let stoneLength = stoneAsString.Length

        if stoneLength % 2 = 0 then
            splitString (stoneLength / 2) stoneAsString
        else
            [ stone * 2024L ]


let performBlinks (counted: StoneCount list) =
    let performBlinks' (stones: StoneCount list) =
        let after =
            stones
            |> List.collect (fun sc ->
                sc.Stone
                |> blinkOn
                |> List.map (fun s -> { Stone = s; Count = sc.Count }))

        let counted =
            after
            |> List.groupBy _.Stone
            |> List.map (fun (stone, stoneCounts) ->
                { Stone = stone
                  Count = stoneCounts |> List.sumBy _.Count })

        Some(counted, counted)

    counted |> Seq.unfold performBlinks'

let solve (blinks: int) (s: string) =
    s
    |> parseStones
    |> List.countBy id
    |> List.map (fun (stone, count) -> { Stone = stone; Count = count })
    |> performBlinks
    |> Seq.take blinks
    |> Seq.last
    |> List.map _.Count
    |> List.sum

let sampleSolution1 = inputSample |> solve 25

let solution1 = input |> solve 25

// Part 2
let solution2 = input |> solve 75
