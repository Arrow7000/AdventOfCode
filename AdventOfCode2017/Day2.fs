// http://adventofcode.com/2017/day/2

module Day2

open System.IO
open System.Security.Cryptography.X509Certificates

let rowDiff (row : int list) =
    (List.max row) - (List.min row) |> abs

let rowToInts (row : string) =
    row.Split [|'\t'|]
    |> Array.toList
    |> List.map int

let rows = 
    File.ReadAllLines "./day2.tsv" 
    |> Array.toList
    |> List.map rowToInts

let main = 
    rows
    |> List.map rowDiff
    |> List.sum


let divideEachOther a b =
    if a = b then None else

    let fA = float a
    let fB = float b
    let right = fA / fB
    let left = fB / fA
    let rightInt = floor right
    let leftInt = floor left
    if rightInt = right then
        Some <| int rightInt
    else if leftInt = left then
        Some <| int leftInt
    else None

let divideDivisibles row =
    List.allPairs row row
    |> List.choose (fun (a, b) -> divideEachOther a b)
    |> List.item 0

let part2 = 
    rows
    |> List.map divideDivisibles
    |> List.sum