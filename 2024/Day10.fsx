#load "Utils.fsx"

open System.IO

// Input
let input = File.ReadAllLines "input10.txt"

let inputSample =
    """89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"""
    |> Utils.splitLines

type Position = { X: int; Y: int; Height: int }
type Positions = Position list

type Map = Set<Position>

let getMap (lines: string[]) =
    lines
    |> Array.map Utils.toChars
    |> Array.mapi (fun y line ->
        line
        |> Array.mapi (fun x c -> { X = x; Y = y; Height = int c - 48 }))
    |> Array.concat
    |> Set.ofArray

let getTrailStarts (map: Map) : Positions =
    map
    |> Set.filter (fun p -> p.Height = 0)
    |> Set.toList

// Part 1

let getNextSteps (map: Map) (pos: Position) : Positions =
    let height = pos.Height
    let x = pos.X
    let y = pos.Y

    let steps =
        [ { X = x
            Y = y - 1
            Height = height + 1 }
          { X = x + 1
            Y = y
            Height = height + 1 }
          { X = x
            Y = y + 1
            Height = height + 1 }
          { X = x - 1
            Y = y
            Height = height + 1 } ]
        |> Set.ofList

    steps |> Set.intersect map |> Set.toList

let findTrailEndCounts (map: Map) (start: Position) =
    let rec findTrailEnds (starts: Positions) =
        if starts.Head.Height = 9 then
            starts
        else
            let nextSteps =
                starts
                |> List.collect (getNextSteps map)
                |> List.distinct

            findTrailEnds nextSteps

    [ start ] |> findTrailEnds |> List.length

let solve1 (lines: string[]) =
    let map = getMap lines
    let starts = getTrailStarts map

    starts
    |> List.map (findTrailEndCounts map)
    |> List.sum

let sampleSolution1 = inputSample |> solve1

let solution1 = input |> solve1

// Part 2
type Trail = Position list

let findTrailNextSteps (map: Map) (trail: Trail) =
    let previousEnd = trail.Head

    match previousEnd |> getNextSteps map with
    | [] -> []
    | nextSteps -> nextSteps |> List.map (fun step -> step :: trail)

let findDistinctTrails (map: Map) (start: Position) =
    let rec findTrailEnds (trails: Trail list) =
        if trails.Head.Head.Height = 9 then
            trails
        else
            let nextTrails =
                trails
                |> List.collect (findTrailNextSteps map)
                |> List.distinct

            findTrailEnds nextTrails

    [[ start ]] |> findTrailEnds |> List.length

let solve2 (lines: string[]) =
    let map = lines |> getMap
    let starts = map |> getTrailStarts
    
    starts
    |> List.map (findDistinctTrails map)
    |> List.sum

let sampleSolution2 = inputSample |> solve2

let solution2 = input |> solve2
