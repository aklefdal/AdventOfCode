#load "Utils.fsx"

open System.IO

type Direction =
    | L
    | R

type Move = {
    Direction: Direction
    Steps: int
}

let parseMove (line: string) =
    let dir =
        match line[0] with
        | 'L' -> L
        | 'R' -> R
        | _ -> failwith "Invalid direction"

    let steps = line[1..] |> int

    { Direction = dir; Steps = steps }

type State = {
    Position: int
    Counter: int
}

// Input
let input =
    File.ReadAllLines "2025/input01.txt"
    |> Array.map parseMove


let inputSample =
    """L68
       L30
       R48
       L5
       R60
       L55
       L1
       L99
       R14
       L82"""
    |> Utils.splitLines
    |> Array.map parseMove

let applyMove (state: State) (move: Move) =
    match move.Direction with
    | L ->
        match state.Position - move.Steps with
        | n when n % 100 = 0 -> { Position = 0; Counter = state.Counter + 1 }
        | n when n < 0 -> {state with Position = n % 100 + 100 }
        | n -> { state with Position = n }
    | R ->
        match state.Position + move.Steps with
        | n when n % 100 = 0 -> { Position = 0; Counter = state.Counter + 1 }
        | n when n >= 100 -> { state with Position = n % 100 }
        | n -> { state with Position = n }

let solutionSample =
    inputSample
    |> Array.fold applyMove { Position = 50; Counter = 0 }

// Part 1
let solution1 =
    input
    |> Array.fold applyMove { Position = 50; Counter = 0 }

// Part 2
let getPosition (position: int) =
    match position with
    | n when n < 0 -> (n % 100 + 100) % 100
    | n when n = 0 -> 0
    | n when n = 100 -> 0
    | n when n > 100 -> n % 100
    | n -> n

let getLeftCounts (position: int) (steps: int) =
    if position - steps > 0 then
        0
    elif position = 0 then
        steps / 100
    else
        1 - (position - steps) / 100

let getRightCounts (position: int) (steps: int) =
    match position + steps with
    | n when n = 100 -> 1
    | n when n > 100 -> n / 100
    | _ -> 0

let applyMove2 (state: State) (move: Move) =
    match move.Direction with
    | L -> 
        let newPos = state.Position - move.Steps |> getPosition
        let counts = getLeftCounts state.Position move.Steps
        {Position = newPos; Counter = state.Counter + counts }
    | R -> 
        let newPos = state.Position + move.Steps |> getPosition
        let counts = getRightCounts state.Position move.Steps
        {Position = newPos; Counter = state.Counter + counts }

let solutionSample2 =
    inputSample
    |> Array.fold applyMove2 { Position = 50; Counter = 0 }

// Part 1
let solution2 =
    input
    |> Array.fold applyMove2 { Position = 50; Counter = 0 }
