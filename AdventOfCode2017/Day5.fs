// http://adventofcode.com/2017/day/5

module Day5

open System.IO

let steps = 
    File.ReadAllLines "./day5.txt"
    |> Array.toList
    |> List.map int

// I suspect the replace function is very compute intensive, should replace
let replace i item list =
    List.take i list @ 
    [item] @ 
    List.rev (List.take (list.Length - i - 1) (List.rev list))

let stepper list  =
    let rec stepIncr (list : int list) (index : int) (n : int) =
        let stepFromHere = list.[index]
        let newIndex = index + stepFromHere
        if newIndex < 0 || newIndex > list.Length - 1 then
            n + 1
        else
            let newList = replace index (stepFromHere + 1) list
            stepIncr newList newIndex (n + 1)
    stepIncr list 0 0

let main =
    stepper steps
