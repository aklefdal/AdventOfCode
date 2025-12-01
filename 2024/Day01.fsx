#load "Utils.fsx"

open System
open System.Collections.Generic
open System.IO

// Input
let input =
    File.ReadAllLines "input01.txt"
    |> Array.map Utils.toInts

// Part 1
let solution1 =
    input
    |> Array.map (fun a -> a[0], a[1])
    |> Array.unzip
    |> fun (a, b) -> a |> Array.sort, b |> Array.sort
    ||> Array.zip
    |> Array.sumBy (fun (a, b) -> Math.Abs(a - b))


// Part 2
let counts =
    input
    |> Array.map (fun a -> a[1])
    |> Array.countBy id
    |> dict

let similarityIndex (counts: IDictionary<int, int>) (i: int) =
    if counts.ContainsKey(i) then i * counts[i] else 0

let solution2 =
    input
    |> Array.map (fun a -> a[0])
    |> Array.map (similarityIndex counts)
    |> Array.sum
