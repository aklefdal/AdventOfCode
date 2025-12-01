#load "Utils.fsx"

open System
open System.IO
open Microsoft.FSharp.Core

// Input
let input = File.ReadAllText "input12.txt"

let inputSampleA =
    """AAAA
BBCD
BBCC
EEEC"""

let inputSampleB =
    """RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"""

type Position = { X: int; Y: int }

type Region = Set<Position>

type Plot =
    { Position: Position
      Name: char }

type PlotMap = Plot[][]

type NeighBour =
    | Plot of Plot
    | Outside

let parsePlotMap (s: string) : PlotMap =
    s
    |> Utils.splitLines
    |> Array.mapi (fun y line ->
        line
        |> Utils.toChars
        |> Array.mapi (fun x c ->
            { Position = { X = x; Y = y }
              Name = c }))

let isInside (map: PlotMap) (pos: Position) =
    if
        pos.X >= 0
        && pos.X < map[0].Length
        && pos.Y >= 0
        && pos.Y < map.Length
    then
        Some pos
    else
        None

let rec getAllInRegion (map: PlotMap) (pos: Position) (region: Region) : Region =
    let plot = map[pos.Y].[pos.X]
    let sameNeighbours =
        [ { Y = plot.Position.Y - 1
            X = plot.Position.X }
          { Y = plot.Position.Y
            X = plot.Position.X + 1 }
          { Y = plot.Position.Y + 1
            X = plot.Position.X }
          { Y = plot.Position.Y
            X = plot.Position.X - 1 } ]
        |> List.choose (isInside map)
        |> List.filter (fun pos ->
            map[pos.Y].[pos.X].Name = plot.Name)
    let neighboursNotInRegion =
        sameNeighbours
        |> List.filter (fun pos ->
            region |> Set.contains |> not)
    
    match neighboursNotInRegion with
    | [] -> region
    | _ ->
        let newRegion = region @ neighboursNotInRegion
        neighboursNotInRegion 
    
        
    

let rec getAllRegions (map: PlotMap) (regions: Region list) : Region list =
    let plots = 
        map
        |> Array.concat

    let plotNotInRegion =
        plots
        |> Array.tryFind (fun plot ->
            regions
            |> List.exists (fun region ->
                region |> Set.contains plot.Position |> not))
    
    match plotNotInRegion with
    | Some plot ->
        let newRegion = getAllInRegion map plot.Position
        getAllRegions map (newRegion :: regions)
    | None -> regions

let getNeighbours (map: PlotMap) (plot: Plot) =
    let x = plot.Position.X
    let y = plot.Position.Y

    let neighbours =
        [ (x, y - 1)
          (x + 1, y)
          (x, y + 1)
          (x - 1, y) ]
        |> List.map (fun (x, y) ->
            if
                x < 0
                || x >= Array.length map[0]
                || y < 0
                || y >= Array.length map
            then
                Outside
            else
                Plot map[y].[x])

    plot, neighbours




let rec setRegion (map: PlotMap) (newRegion: Guid) (plot: Plot) =
    match map[plot.Position.Y].[plot.Position.X].Region with
    | None ->
        map[plot.Position.Y].[plot.Position.X].Region <- Some newRegion

        [ { Y = plot.Position.Y - 1
            X = plot.Position.X }
          { Y = plot.Position.Y
            X = plot.Position.X + 1 }
          { Y = plot.Position.Y + 1
            X = plot.Position.X }
          { Y = plot.Position.Y
            X = plot.Position.X - 1 } ]
        |> List.choose (isInside map)
        |> List.map (fun pos -> map[pos.Y][pos.X])
        |> List.iter (setRegion map newRegion)
    | Some _ -> ()

let getFences ((plot, neighbours): Plot * NeighBour list) =
    let fences =
        neighbours
        |> List.filter (function
            | Outside -> true
            | Plot p -> p.Name <> plot.Name)
        |> List.length

    plot, fences

let solve1 (map: PlotMap) =
    map |> Array.concat |> Array.iter (setRegion map)

    map
    |> Array.concat
    |> Array.map (getNeighbours map)
    |> Array.map getFences
    |> Array.groupBy (fun (p, _) -> p.Region)
    |> Array.map (fun (name, fences) ->
        let area = fences.Length
        let fenceCount = fences |> Array.sumBy snd
        name.ToString(), area, fenceCount, area * fenceCount)

let map = inputSampleA |> parsePlotMap

// Part 1
let sampleSolution1A = inputSampleA |> parsePlotMap |> solve1
let sampleSolution1B = inputSampleB |> parsePlotMap |> solve1

let solution1 = input

// Part 2

let sampleSolution2 = inputSampleB

let solution2 = input
