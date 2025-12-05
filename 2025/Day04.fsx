#load "Utils.fsx"

open System
open System.IO

// Input
let input = 
    "2025/input04.txt"
    |> File.ReadAllLines
    |> Array.map Utils.toChars

let inputSample =
    """..@@.@@@@.
       @@@.@.@.@@
       @@@@@.@.@@
       @.@@@@..@.
       @@.@@@@.@@
       .@@@@@@@.@
       .@.@.@.@@@
       @.@@@.@@@@
       .@@@@@@@@.
       @.@.@@@.@."""
    |> Utils.splitLines
    |> Array.map Utils.toChars

let getNeighbours (a: char[][]) (x: int) (y: int) : char list =
    let height = a.Length
    let width = a[0].Length

    seq {
        x - 1, y - 1
        x, y - 1
        x + 1, y - 1
        x - 1, y
        x + 1, y
        x - 1, y + 1 
        x, y + 1 
        x + 1, y + 1 
    }
    |> Seq.filter (fun (x, y) -> x >= 0 && y >= 0 && x < width && y < height)
    |> Seq.map (fun (x, y) -> a[y][x])
    |> Seq.toList

let getCount (a: char[][]) = seq {
    let height = a.Length
    let width = a[0].Length

    for x = 0 to width - 1 do
        for y = 0 to height - 1 do
            let c = a[y][x]
            if c = '@' then
                let hasLessThan4neighbours = getNeighbours a x y |> List.filter (fun c -> c = '@') |> List.length < 4
                if hasLessThan4neighbours then
                    yield x, y
}

// Part 1
let solutionSample =
    inputSample
    |> getCount
    |> Seq.length

let solution1 =
    input
    |> getCount
    |> Seq.length

// Part 2
type State = {
    Arr: char[][]
    Removed: int
}

let rec doWork (state: State) : State =
    let toRemove = getCount state.Arr |> Seq.toList
    if List.isEmpty toRemove then
        state
    else
        let newArr = state.Arr |> Array.map Array.copy
        for (x, y) in toRemove do
            newArr[y][x] <- '.'

        let newState = {
            Arr = newArr
            Removed = state.Removed + List.length toRemove
        }
        doWork newState

let solutionSample2 =
    inputSample
    |> fun arr -> { Arr = arr; Removed = 0 }
    |> doWork
    |> fun s -> s.Removed

let solution2 =
    input
    |> fun arr -> { Arr = arr; Removed = 0 }
    |> doWork
    |> fun s -> s.Removed
