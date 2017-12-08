// http://adventofcode.com/2017/day/5

module Day5

open System.IO

let steps () = 
    File.ReadAllLines "./day5.txt"
    |> Array.map int

let replaceArray i item (array : 'a[]) =
    array.[i] <- item
    array



let stepper list  =
    let rec stepIncr (list : int []) (index : int) (n : int) =
        let stepFromHere = list.[index]
        let newIndex = index + stepFromHere
        if newIndex < 0 || newIndex > list.Length - 1 then
            n + 1
        else
            let newList = replaceArray index (stepFromHere + 1) list
            stepIncr newList newIndex (n + 1)
    stepIncr list 0 0

let main () =
    stepper <| (steps())




let stepper2 list  =
    let rec stepIncr (list : int []) (index : int) (n : int) =
        let stepFromHere = list.[index]
        let newIndex = index + stepFromHere
        let newStepFromHere = stepFromHere + (if (stepFromHere) > 2 then -1 else 1)
        if newIndex < 0 || newIndex > list.Length - 1 then
            n + 1
        else
            let newList = replaceArray index newStepFromHere list
            stepIncr newList newIndex (n + 1)
    stepIncr list 0 0

let part2 () =
    stepper2 <| steps ()