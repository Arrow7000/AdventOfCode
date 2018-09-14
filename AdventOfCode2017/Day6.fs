// http://adventofcode.com/2017/day/6

module Day6

open System.IO
open Commons

type Blocks = int

let banks: Blocks list = 
    getText "day6.txt"
    |> (fun str -> str.Split([|'\t'|]))
    |> Array.map int
    |> Array.toList


let getMaxIndex vals =
    vals
    |> List.mapi (fun i v-> i, v)
    |> List.maxBy snd

let distribute (banks : Blocks list) =
    let startIndex, blocks = getMaxIndex banks
    let len = List.length banks
    let wraps = blocks % len
    let remainder = blocks - len * wraps
    banks
    |> List.mapi (fun i bank -> 
        if wraps < 1 then 
            if i > startIndex && i <= startIndex + blocks then
                bank + 1
            else 
                bank
        else
            bank
            )