#load "Utils.fsx"

open System.IO

// Input
let input = File.ReadAllText "input09.txt"

let inputSample = """2333133121414131402"""

type File = { Length: int; Id: int }

type Content =
    | File of File
    | Space of length: int

let tryGetFile =
    function
    | File f -> Some f
    | Space _ -> None

type Block =
    | Occupied of Id: int
    | Empty

let getBlocks (content: Content) : Block[] =
    match content with
    | File f -> Array.create f.Length (Occupied f.Id)
    | Space l -> Array.create l Empty

let getBlockValue (block: Block) =
    match block with
    | Occupied id -> id
    | Empty -> 0

let tryGetFileId = tryGetFile >> Option.map _.Id

// Part 1
let isContinuous (blocks: Block[]) =
    let firstEmpty = blocks |> Array.findIndex (fun b -> b = Empty)

    let lastOccupied =
        blocks
        |> Array.findIndexBack (fun b -> b <> Empty)

    firstEmpty > lastOccupied

let solve1 (s: string) =
    let blocks =
        s
        |> Utils.toChars
        |> Array.mapi (fun i c ->
            if i % 2 = 0 then
                let fileId = i / 2
                File { Length = (int c - 48); Id = fileId }
            else
                Space(int c - 48))
        |> Array.collect getBlocks

    while not (blocks |> isContinuous) do
        let firstEmpty = blocks |> Array.findIndex (fun b -> b = Empty)

        let lastOccupied =
            blocks
            |> Array.findIndexBack (fun b -> b <> Empty)

        blocks[firstEmpty] <- blocks[lastOccupied]
        blocks[lastOccupied] <- Empty

    blocks
    |> Array.map getBlockValue
    |> Array.mapi (fun i id -> int64 i * int64 id)
    |> Array.sum

let sampleSolution1 = inputSample |> solve1

let solution1 = input |> solve1

// Part 2
let swapFile (content: Content[]) (fileId: int) =
    let fileIndex =
        content
        |> Array.findIndex (function
            | File f -> f.Id = fileId
            | Space _ -> false)

    let fileLength =
        content
        |> Array.choose tryGetFile
        |> Array.find (fun f -> f.Id = fileId)
        |> _.Length

    let spaceIndex =
        content
        |> Array.tryFindIndex (function
            | File _ -> false
            | Space length -> length >= fileLength)

    match spaceIndex, fileIndex with
    | None, _ -> content
    | Some i, f when i > f -> content
    | Some si, _ ->
        let spaceLength =
            content[si]
            |> function
                | Space l -> l
                | _ -> failwith "Invalid space"

        let file = content[fileIndex]
        let newSpace = Space fileLength
        content[si] <- file
        content[fileIndex] <- newSpace

        if spaceLength > fileLength then
            let extraSpace = Space(spaceLength - fileLength)
            content |> Array.insertAt (si + 1) extraSpace
        else
            content

let solve2 (s: string) =
    let content =
        s
        |> Utils.toChars
        |> Array.mapi (fun i c ->
            if i % 2 = 0 then
                let fileId = i / 2
                File { Length = (int c - 48); Id = fileId }
            else
                Space(int c - 48))

    let maxFileId = content |> Array.choose tryGetFileId |> Array.max

    seq { maxFileId .. -1 .. 0 }
    |> Seq.fold swapFile content
    |> Array.collect getBlocks
    |> Array.map getBlockValue
    |> Array.mapi (fun i id -> int64 i * int64 id)
    |> Array.sum


let sampleSolution2 = inputSample |> solve2

let solution2 = input |> solve2
