#load "Utils.fsx"

open System.IO

// Input
let input =
    File.ReadAllLines "input04.txt"
    |> Array.map (fun s -> s.ToCharArray())

let inputSample =
    """
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""
    |> Utils.splitLines
    |> Array.map (fun s -> s.ToCharArray())


// Part 1
let isXmas (cs: char[]) : bool =
    cs = [| 'X'; 'M'; 'A'; 'S' |]
    || cs = [| 'S'; 'A'; 'M'; 'X' |]

let findHorizontalCombinations (cs: char[][]) : int =
    let mutable count = 0

    for i in 0 .. cs.Length - 1 do
        for j in 0 .. cs[i].Length - 4 do
            let css =
                [| cs[i][j]
                   cs[i][j + 1]
                   cs[i][j + 2]
                   cs[i][j + 3] |]

            if isXmas css then
                count <- count + 1
                printfn $"Found horizontal XMAS at (%d{i}, %d{j})"

    count

inputSample |> findHorizontalCombinations

let findVerticalCombinations (cs: char[][]) : int =
    let mutable count = 0

    for i in 0 .. cs.Length - 4 do
        for j in 0 .. cs[i].Length - 1 do
            let css =
                [| cs[i][j]
                   cs[i + 1][j]
                   cs[i + 2][j]
                   cs[i + 3][j] |]

            if isXmas css then
                count <- count + 1
                printfn $"Found vertical XMAS at (%d{i}, %d{j})"

    count

inputSample |> findVerticalCombinations

let findTopLeftCombinations (cs: char[][]) : int =
    let mutable count = 0

    for i in 0 .. cs.Length - 4 do
        for j in 0 .. cs[i].Length - 4 do
            let css =
                [| cs[i][j]
                   cs[i + 1][j + 1]
                   cs[i + 2][j + 2]
                   cs[i + 3][j + 3] |]

            if isXmas css then
                count <- count + 1
                printfn $"Found top-left XMAS at (%d{i}, %d{j})"

    count

inputSample |> findTopLeftCombinations

let findTopRightCombinations (cs: char[][]) : int =
    let mutable count = 0

    for i in 0 .. cs.Length - 4 do
        for j in 3 .. cs[i].Length - 1 do
            let css =
                [| cs[i][j]
                   cs[i + 1][j - 1]
                   cs[i + 2][j - 2]
                   cs[i + 3][j - 3] |]

            if isXmas css then
                count <- count + 1
                printfn $"Found top-right XMAS at (%d{i}, %d{j})"

    count

inputSample |> findTopRightCombinations

let findXmas (cs: char[][]) : int =
    (cs |> findHorizontalCombinations)
    + (cs |> findVerticalCombinations)
    + (cs |> findTopLeftCombinations)
    + (cs |> findTopRightCombinations)


let sampleSolution1 = inputSample |> findXmas
let solution1 = input |> findXmas

// Part 2
let isXmasTopDown (cs: char[][]) : bool =
    // M  M
    //  A
    // S  S
    cs[0].[0] = 'M'
    && cs[0].[2] = 'M'
    && cs[1].[1] = 'A'
    && cs[2].[0] = 'S'
    && cs[2].[2] = 'S'

let isXmasBottomUp (cs: char[][]) : bool =
    // S  S
    //  A
    // M  M
    cs[0].[0] = 'S'
    && cs[0].[2] = 'S'
    && cs[1].[1] = 'A'
    && cs[2].[0] = 'M'
    && cs[2].[2] = 'M'

let isXmasLeftRight (cs: char[][]) : bool =
    // M  S
    //  A
    // M  S
    cs[0].[0] = 'M'
    && cs[0].[2] = 'S'
    && cs[1].[1] = 'A'
    && cs[2].[0] = 'M'
    && cs[2].[2] = 'S'

let isXmasRightLeft (cs: char[][]) : bool =
    // S  M
    //  A
    // S  M
    cs[0].[0] = 'S'
    && cs[0].[2] = 'M'
    && cs[1].[1] = 'A'
    && cs[2].[0] = 'S'
    && cs[2].[2] = 'M'

let isXmas2 (cs: char[][]) : bool =
    isXmasTopDown cs
    || isXmasBottomUp cs
    || isXmasLeftRight cs
    || isXmasRightLeft cs

let findXmas2 (cs: char[][]) : int =
    let mutable count = 0

    for i in 0 .. cs.Length - 3 do
        for j in 0 .. cs[i].Length - 3 do
            let css =
                [| cs[i][j .. j + 3]
                   cs[i + 1][j .. j + 3]
                   cs[i + 2][j .. j + 3] |]

            if css |> isXmas2 then
                count <- count + 1
                printfn $"Found XMAS at (%d{i}, %d{j})"

    count

let sampleSolution2 = inputSample |> findXmas2

let solution2 = input |> findXmas2
