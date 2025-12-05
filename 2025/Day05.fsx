#load "Utils.fsx"

open System
open System.IO

// Input
let input = 
    "2025/input05.txt"
    |> File.ReadAllText
    |> Utils.replace "\r\n" "\n"

let inputSample =
    """3-5
10-14
16-20
12-18

1
5
8
11
17
32"""

type Range = {
    Min: int64
    Max: int64
}

let parse (s: string) = 
    let doubleNewLine = "\n\n"
    let parts = s.Split(doubleNewLine, StringSplitOptions.RemoveEmptyEntries)
    let ranges =
        parts[0]
        |> Utils.splitLines
        |> Array.map (Utils.split "-")
        |> Array.map (fun arr -> { Min = int64 arr[0]; Max = int64 arr[1] })
    let ingredients =
        parts[1]
        |> Utils.splitLines
        |> Array.map int64
    ranges, ingredients

let isInRange (ingredient: int64) (range: Range) =
    ingredient >= range.Min && ingredient <= range.Max

let isInRanges (ranges: Range[]) (ingredient: int64) =
    ranges
    |> Array.exists (isInRange ingredient)

let findIngredientInRange (ranges: Range[], ingredients: int64[]) =
    ingredients
    |> Array.filter (isInRanges ranges)

// Part 1
let solutionSample =
    inputSample
    |> parse
    |> findIngredientInRange
    |> Array.length

let solution1 =
    input
    |> parse
    |> findIngredientInRange
    |> Array.length

// Part 2

let getRanges (s: string) =
    s
    |> parse
    |> fst
    |> Array.toList
    |> List.sort

let deDuplicate (range1: Range) (range2: Range) =
    if range1.Max >= range2.Min then
        [ { Min = range1.Min; Max = Math.Max(range1.Max, range2.Max) } ]
    else
        [range1; range2]

let rec deDuplicateRanges (ranges: Range list) =
    match ranges with
    | [] -> []
    | [a] -> [a]
    | a :: b :: tail ->
        match deDuplicate a b with
        | [] -> failwith "Should not happen"
        | a :: b :: c :: tail -> failwith "Should not happen"
        | [r] -> deDuplicateRanges (r :: tail)
        | [a; b] -> a :: deDuplicateRanges (b :: tail)

let getCount (range: Range) =
    range.Max - range.Min + 1L

let solutionSample2 =
    inputSample
    |> getRanges
    |> deDuplicateRanges
    |> List.map getCount
    |> List.sum

let solution2 =
    input
    |> getRanges
    |> deDuplicateRanges
    |> List.map getCount
    |> List.sum
