// https://adventofcode.com/2017/day/13

module Day13

open System.Text.RegularExpressions
open Commons

type Range = int
type Depth = int

type ScannerPosition = int
type Packet = int
type Direction = Up | Down
type Layer = Depth * ScannerPosition * Direction
type Firewall = Map<Range, Layer>
type Severity = int
type Game = Packet * Firewall * Severity

type GameState = GameEnd of Severity | GameProgress of Game

let initialLayer depth : Layer = depth, 0, Down

let advanceScanner ((depth, scannerPos, dir) : Layer) : Layer =
    match scannerPos with
    | 0 -> depth, 1, Down
    | pos when pos >= depth - 1 -> depth, pos - 1, Up
    | pos ->
        match dir with
        | Up -> depth, pos - 1, dir
        | Down -> depth, pos + 1, dir

let advanceFirewall (firewall : Firewall) : Firewall =
    firewall
    |> Map.map (fun _ layer -> advanceScanner layer)

let getGameEnd (firewall : Firewall) : Severity =
    let maxPos =
        firewall
        |> Map.toList
        |> List.map fst
        |> List.max

    let rec gameStepper ((packet, firewall, severity) : Game) : Severity =
        if packet >= maxPos then severity
        else
            let inLayer = Map.tryFind packet firewall
            match inLayer with
            | None ->
                gameStepper (packet + 1, advanceFirewall firewall, severity)
            | Some (depth, scannerPos, _) ->
                if scannerPos <> 0 then
                    gameStepper (packet + 1, advanceFirewall firewall, severity)
                else
                    gameStepper (packet + 1, advanceFirewall firewall, severity + (depth * packet))


    gameStepper (0, firewall, 0)


let constructFirewall lines : Firewall=
    let makeLine str : (Range * Layer) option =
        let m = Regex.Match(str, @"^(\d+): (\d+)$")
        if m.Success then
            let matches = [ for g in m.Groups -> g.Value ]
            let layer = initialLayer (List.item 2 matches |> int)
            ((List.item 1 matches |> int), layer) |> Some
        else None

    lines
    |> List.choose makeLine
    |> Map.ofList

let firewall =
    "day13.txt"
    |> getLines
    |> constructFirewall

let part1func = getGameEnd

let part1 =
    part1func firewall

