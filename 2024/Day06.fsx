#load "Utils.fsx"

open System.IO

// Input
let input = File.ReadAllLines "input06.txt"

let inputSample =
    """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""
    |> Utils.splitLines

type Facing =
    | Up
    | Right
    | Down
    | Left

type Position =
    | Empty
    | Visited of Set<Facing>
    | Obstacle
    | Guard of Facing

type PuzzleMap = Position[][]

let initPosition (c: char) =
    match c with
    | '.' -> Empty
    | '#' -> Obstacle
    | '^' -> Guard Up
    | '>' -> Guard Right
    | 'v' -> Guard Down
    | '<' -> Guard Left
    | _ -> failwith "Invalid character"

type GuardPosition = { X: int; Y: int; Facing: Facing }

// Part 1
let buildMap (i: string[]) =
    i
    |> Array.map Utils.toChars
    |> Array.map (Array.map initPosition)

let findGuard (m: PuzzleMap) =
    let mutable guardPosition = { X = 0; Y = 0; Facing = Up }

    for y in 0 .. m.Length - 1 do
        for x in 0 .. m[y].Length - 1 do
            match m[y][x] with
            | Guard f -> guardPosition <- { X = x; Y = y; Facing = f }
            | _ -> ()

    guardPosition

let turn (f: Facing) =
    match f with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

type MoveResult =
    | Outside
    | InALoop
    | NewPosition of GuardPosition

let nextMove (map: PuzzleMap) (guard: GuardPosition) : MoveResult =
    let x = guard.X
    let y = guard.Y

    let nextX, nextY =
        match guard.Facing with
        | Left -> x - 1, y
        | Right -> x + 1, y
        | Up -> x, y - 1
        | Down -> x, y + 1

    if
        nextY < 0
        || nextY >= map.Length
        || nextX < 0
        || nextX >= map[0].Length
    then
        Outside
    else
        match map[nextY][nextX] with
        | Visited facing ->
            if facing.Contains guard.Facing then
                InALoop
            else
                NewPosition { guard with X = nextX; Y = nextY }
        | Empty -> NewPosition { guard with X = nextX; Y = nextY }
        | Obstacle ->
            NewPosition
                { guard with
                    Facing = guard.Facing |> turn }
        | Guard _ -> failwith "What?"

type GameResult =
    | Finished of PuzzleMap
    | Looping
    | NotRun

let addVisited (map: PuzzleMap) (guard: GuardPosition) =
    map[guard.Y][guard.X] <-
        match map[guard.Y][guard.X] with
        | Visited fs -> Visited(fs.Add guard.Facing)
        | _ -> Visited(guard.Facing |> Set.singleton)

let rec move (map: PuzzleMap) (guard: GuardPosition) : GameResult =
    addVisited map guard

    match nextMove map guard with
    | Outside -> Finished map
    | InALoop -> Looping
    | NewPosition nextGuard -> move map nextGuard

let solve1 (i: string[]) =
    let map = i |> buildMap
    let guard = findGuard map
    let finalMap = guard |> move map

    match finalMap with
    | Finished m ->
        m
        |> Array.concat
        |> Array.map (function
            | Visited _ -> 1
            | _ -> 0)
        |> Array.sum
    | _ -> failwith "Looping"

let sampleSolution1 = inputSample |> solve1

let solution1 = input |> solve1


// Part 2
let checkNewPosition (i: string[]) (x: int, y: int) =
    let map = i |> buildMap
    let guard = findGuard map

    if map[y][x] = Empty then
        map[y][x] <- Obstacle
        guard |> move map
    else
        NotRun

let solve2 (i: string[]) =
    let mutable count = 0

    for y in 0 .. i.Length - 1 do
        for x in 0 .. i[y].Length - 1 do
            match checkNewPosition i (x, y) with
            | Looping -> count <- count + 1
            | _ -> ()

    count

let sampleSolution2 = inputSample |> solve2

let solution2 = input |> solve2
