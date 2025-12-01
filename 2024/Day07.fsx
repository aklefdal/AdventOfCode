#load "Utils.fsx"

open System.IO

// Input
let input = File.ReadAllLines "input07.txt"

let inputSample =
    """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""
    |> Utils.splitLines

let parseLine (line: string) =
    let parts = line.Split(": ")
    let result = parts[0] |> int64
    let numbers = parts[1].Split(" ") |> Array.map int64

    result, numbers

type Operator =
    | Add
    | Multiply

let rec getOperators (i: int) (ns: Operator list list)=
    match i, ns with
    | 0, _ -> ns |> List.map (fun n -> Add :: n)
    | _, ns' ->
        let adds = ns' |> List.map (fun n -> Add :: n)
        let muls = ns' |> List.map (fun n -> Multiply :: n)
        getOperators (i - 1) (adds @ muls)

let calculate (ns: int64[]) (operators: Operator list) =
    ns
    |> List.ofArray
    |> List.zip operators
    |> List.fold (fun acc (o, n) ->
        match o with
        | Add -> acc + n
        | Multiply -> acc * n) 0L

let findCalculations (ns: int64 []) =
    getOperators (ns.Length - 1) [[]]
    |> List.map (calculate ns)

let matches (i: int64, ns: int64 []) : bool =
    findCalculations ns |> List.contains i

let solve1 (lines: string[]) =
    lines
    |> Array.map parseLine
    |> Array.filter matches
    |> Array.sumBy fst

let sampleSolution1 = inputSample |> solve1

let solution1 = input |> solve1


// Part 2
type Operator2 =
    | Add
    | Multiply
    | Concat

let rec getOperators2 (i: int) (ns: Operator2 list list)=
    match i, ns with
    | 0, _ -> ns |> List.map (fun n -> Add :: n)
    | _, ns' ->
        let adds = ns' |> List.map (fun n -> Add :: n)
        let muls = ns' |> List.map (fun n -> Multiply :: n)
        let concats = ns' |> List.map (fun n -> Concat :: n)
        getOperators2 (i - 1) (adds @ muls @ concats)

let calculate2 (ns: int64[]) (operators: Operator2 list) =
    ns
    |> List.ofArray
    |> List.zip operators
    |> List.fold (fun acc (o, n) ->
        match o with
        | Add -> acc + n
        | Multiply -> acc * n
        | Concat -> string acc + string n |> int64) 0L

let findCalculations2 (ns: int64 []) =
    getOperators2 (ns.Length - 1) [[]]
    |> List.map (calculate2 ns)

let matches2 (i: int64, ns: int64 []) : bool =
    findCalculations2 ns |> List.contains i

let solve2 (lines: string[]) =
    lines
    |> Array.map parseLine
    |> Array.filter matches2
    |> Array.sumBy fst

let sampleSolution2 = inputSample |> solve2

let solution2 = input |> solve2
