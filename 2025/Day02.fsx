#load "Utils.fsx"

open System.IO

// Input
let input = File.ReadAllText "2025/input02.txt"

let inputSample =
    "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"




let splitStringInTwo (s: string) =
    let length = s.Length
    s.[0..(length / 2 - 1)], s.[(length / 2)..length - 1]

let isRepeated (a: int64) : bool =
    let s = a.ToString()
    let firstHalf, secondHalf = splitStringInTwo s
    firstHalf = secondHalf

let toSeq (a: int64, b : int64 ) =
    seq { a .. b }

let getNumberSeq (s: string) =
    s
    |> Utils.split ","
    |> Array.map (Utils.split "-" >> fun arr -> arr[0] |> int64, arr[1] |> int64)
    |> Array.map toSeq
    |> Seq.concat

"1-10" |> getNumberSeq |> Seq.toList

let parse (s: string) =
    s
    |> getNumberSeq
    |> Seq.filter isRepeated
    |> Seq.sum

let solutionSample =
    inputSample |> parse

// Part 1
let solution1 =
    input |> parse

// Part 2

let splitIn (s: string) (parts: int) =
    let length = s.Length

    if length % parts <> 0 then
        [||]
    else
        let partLength = length / parts
        [| for i in 0 .. parts - 1 ->
             s.[i * partLength .. (i + 1) * partLength - 1] |]


let getParts (i: int) : int array =
    seq {
        if i > 1 then
            yield i
        for k = i / 2 downto 2 do
            if i % k = 0 then
                yield k
    }
    |> Seq.toArray

let areAllPartsEqual (parts: string array) : bool =
    parts |> Array.forall ((=) parts[0])

let isValid (l: int64) : bool =
    let s = l.ToString()
    let length = s.Length
    let parts = length |> getParts

    parts
    |> Array.map (splitIn s)
    |> Array.map areAllPartsEqual
    |> Array.exists id

let l = 121212121212L
let s = "1-10"

let parse2 (s: string) =
    s
    |> getNumberSeq
    |> Seq.filter isValid
    |> Seq.sum

let solutionSample2 =
    inputSample |> parse2

let solution2 =
    input |> parse2
