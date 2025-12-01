#load "Utils.fsx"

open System
open System.IO

// Input
let input =
    File.ReadAllLines "input02.txt"
    |> Array.map Utils.toInts

let inputSample =
    """7 6 4 2 1
       1 2 7 8 9
       9 7 6 2 1
       1 3 2 4 5
       8 6 4 4 1
       1 3 6 7 9"""
    |> Utils.splitLines
    |> Array.map Utils.toInts

// Part 1
let isLevelSorted (level: int[]) =
    (level |> Array.sort) = level
    || (level |> Array.sortDescending) = level

let isLevelSafe (level: int[]) =
    let diffs =
        level
        |> Array.pairwise
        |> Array.map (fun (a, b) -> Math.Abs(b - a))

    let maxDiff = diffs |> Array.max
    let minDiff = diffs |> Array.min

    minDiff >= 1 && maxDiff <= 3

let solution1 =
    input
    |> Array.filter isLevelSorted
    |> Array.filter isLevelSafe
    |> Array.length

// Part 2

let check (level: int[]) =
    level |> isLevelSorted && level |> isLevelSafe

let checkWithDampener (level: int[]) =
    seq { 0 .. level.Length - 1 }
    |> Seq.map (fun i -> level |> Array.removeAt i)
    |> Seq.exists check

let checkAll (level: int[]) =
    level |> check || level |> checkWithDampener

let sampleSolution2 =
    inputSample
    |> Array.filter checkAll
    |> Array.length

let solution2 = input |> Array.filter checkAll |> Array.length
