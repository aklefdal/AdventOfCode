#load "Utils.fsx"

open System.IO
open Microsoft.FSharp.Core

// Input
let input = File.ReadAllText "input11.txt"

let inputSample = """125 17"""


// Part 1
type Stone = int64

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
            let newStone = stone * 2024L
            [ newStone ]

let rec performBlinks (i: int) (stones: Stone list) : Stone list =
    let after = stones |> List.collect blinkOn
    if i = 1 then after else performBlinks (i - 1) after

let solve (blinks: int) (s: string) =
    s
    |> parseStones
    |> performBlinks blinks
    |> List.length

let sampleSolution1 = inputSample |> solve 25 

let solution1 = input |> solve 25

// Part 2
let solution2 = input |> solve 40
