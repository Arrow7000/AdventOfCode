// http://adventofcode.com/2017/day/6

module Day6

open System.IO

let banks = 
    File.ReadAllText "./day6.txt"
    |> (fun str -> str.Split([|'\t'|]))
    |> Array.map int
    |> Array.toList


