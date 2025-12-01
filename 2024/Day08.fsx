#load "Utils.fsx"

open System.IO

// Input
let input = File.ReadAllLines "input08.txt"

let inputSample =
    """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"""
    |> Utils.splitLines

type Position = { X: int; Y: int }

type Antenna = { Frequency: char; Position: Position }

let getAntennas (lines) =
    lines
    |> Array.map Utils.toChars
    |> Array.mapi (fun y line ->
        line
        |> Array.mapi (fun x c ->
            { Frequency = c
              Position = { X = x; Y = y } }))
    |> Array.concat
    |> Array.filter (fun a -> a.Frequency <> '.')

let getFrequencyPositions antennas =
    antennas
    |> Array.groupBy _.Frequency
    |> Array.map (fun (_, antennas) -> antennas |> Array.map _.Position)

let isInside (maxX: int) (maxY: int) (pos: Position) =
    if
        pos.X >= 0
        && pos.X < maxX
        && pos.Y >= 0
        && pos.Y < maxY
    then
        Some pos
    else
        None

let sortPositions (pos1: Position, pos2: Position) =
    if pos1 > pos2
    then
        (pos2, pos1)
    else
        (pos1, pos2)

let getAntiNodes (maxX: int) (maxY: int) (pos1: Position, pos2: Position) : Position[] =
    let diffX = pos2.X - pos1.X
    let diffY = pos2.Y - pos1.Y

    let left =
        { X = pos1.X - diffX
          Y = pos1.Y - diffY }

    let right =
        { X = pos2.X + diffX
          Y = pos2.Y + diffY }

    [| left; right |]
    |> Array.choose (isInside maxX maxY)

let getAllAntiNodes (maxX: int) (maxY: int) (positions: Position[]) =
    Array.allPairs positions positions
    |> Array.filter (fun (pos1, pos2) -> pos1 <> pos2)
    |> Array.map sortPositions
    |> Array.distinct
    |> Array.collect (getAntiNodes maxX maxY)
    |> Array.distinct

let solve1 (lines: string[]) =
    let maxY = lines.Length
    let maxX = lines[0].Length

    lines
    |> getAntennas
    |> getFrequencyPositions
    |> Array.map (getAllAntiNodes maxX maxY)
    |> Array.concat
    |> Array.distinct
    |> Array.length

let sampleSolution1 = inputSample |> solve1

let solution1 = input |> solve1


// Part 2
let getAntiNodes2 (maxX: int) (maxY: int) (pos1: Position, pos2: Position) : Position[] =
    let diffX = pos2.X - pos1.X
    let diffY = pos2.Y - pos1.Y
    
    let allLefts =
        pos1
        |> Seq.unfold (fun (pos: Position) ->
            { X = pos.X - diffX; Y = pos.Y - diffY }
            |> (isInside maxX maxY)
            |> Option.map (fun pos -> (pos, pos)))
        |> Seq.toArray

    let allRights =
        pos2
        |> Seq.unfold (fun (pos: Position) ->
            { X = pos.X + diffX; Y = pos.Y + diffY }
            |> (isInside maxX maxY)
            |> Option.map (fun pos -> (pos, pos)))
        |> Seq.toArray

    [| pos1; pos2 |]
    |> Array.append allLefts
    |> Array.append allRights

let getAllAntiNodes2 (maxX: int) (maxY: int) (positions: Position[]) =
    Array.allPairs positions positions
    |> Array.filter (fun (pos1, pos2) -> pos1 <> pos2)
    |> Array.map sortPositions
    |> Array.distinct
    |> Array.collect (getAntiNodes2 maxX maxY)
    |> Array.distinct
    |> Array.sort

let solve2 (lines: string[]) =
    let maxY = lines.Length
    let maxX = lines[0].Length

    lines
    |> getAntennas
    |> getFrequencyPositions
    |> Array.map (getAllAntiNodes2 maxX maxY)
    |> Array.concat
    |> Array.distinct
    |> Array.sort
    |> Array.length

let sampleSolution2 = inputSample |> solve2

let solution2 = input |> solve2
