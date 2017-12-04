// http://adventofcode.com/2017/day/2

module Day2

open System.IO

let rowDiff (row : int list) =
    (List.max row) - (List.min row) |> abs

let processEachRow row = rowDiff <| List.map int row


let rows = 
    File.ReadAllLines "./day2.tsv" 
    |> Array.toList
    |> List.map (fun row -> row.Split [|'\t'|])
    |> List.map Array.toList

let main = 
    rows
    |> List.map processEachRow
    |> List.sum
