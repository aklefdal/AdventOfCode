#load "Utils.fsx"

open System
open System.IO

// Input
let input = 
    "2025/input03.txt"
    |> File.ReadAllLines
    |> Array.map (Utils.toChars)

let inputSample =
    """987654321111111
       811111111111119
       234234234234278
       818181911112111""" 
    |> Utils.splitLines 
    |> Array.map Utils.toChars

let findMax (chars:char[]) (fromLast: int) : char * int =
    let maxChar =
        chars[.. chars.Length - 1 - fromLast]
        |> Array.max

    let pos =
        chars
        |> Array.findIndex (fun c -> c = maxChar)

    maxChar, pos

let getJoltage (chars: char[]) =
    let firstchar, pos = findMax chars 1
    let secondChar, _ = findMax chars[pos + 1 ..] 0
    int64 ([|firstchar; secondChar|] |> String)

// Part 1
let solutionSample =
    inputSample
    |> Array.map getJoltage
    |> Array.sum

let solution1 =
    input
    |> Array.map getJoltage
    |> Array.sum

// Part 2
type State = {
    Chars: char[]
    Accumulated: char[]
}

let findMax2 (state: State) (fromLast: int) : State =
    let chars = state.Chars
    let maxChar =
        chars[.. chars.Length - 1 - fromLast]
        |> Array.max

    let pos =
        chars
        |> Array.findIndex (fun c -> c = maxChar)

    let remaining = chars[pos + 1 ..]
    let accumulated= Array.append state.Accumulated [|maxChar|]

    { Chars = remaining; Accumulated = accumulated }

let getJoltage2 (chars: char[]) =
    let result =
        seq {for i =11 downto 0 do yield i }
        |> Seq.fold findMax2 { Chars = chars; Accumulated = [||] }

    result.Accumulated
    |> String
    |> int64

let solutionSample2 =
    inputSample
    |> Array.map getJoltage2
    |> Array.sum

let solution2 =
    input
    |> Array.map getJoltage2
    |> Array.sum
