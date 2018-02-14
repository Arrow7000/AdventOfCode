// https://adventofcode.com/2017/day/10

module Day10

open Commons

type PuzzleInputItem = int
type StringNumber = int
type Index = int

let getLengths str : PuzzleInputItem list =
    str
    |> strSplit ','
    |> List.map int


let reverseSublist (indexes : Index list) (list : 'a list) =
    let toRev =
        indexes
        |> List.map (fun i -> list.[i])
    let reved = List.rev toRev
    let replaced =
        list
        |> List.mapi
            (fun i hashChar ->
                if List.contains i indexes then
                    let indexIndex = List.findIndex (fun index -> i = index) indexes
                    reved.[indexIndex]
                else
                    hashChar)
    replaced

let rec hashIterator (currPos : int) (skipSize : int) (stringCirc : StringNumber list) (remainingInput : PuzzleInputItem list) =
    match remainingInput with
    | [] -> stringCirc
    | puzzleItem :: rest ->
        let listLen = List.length stringCirc
        let indexes = 
            [0..(puzzleItem - 1)]
            |> List.map (fun i -> (i + currPos) % listLen)
        let replaced = reverseSublist indexes stringCirc
            
        let newCurrPos = (currPos + puzzleItem + skipSize) % listLen
        hashIterator newCurrPos (skipSize + 1) replaced rest

let hasherGen (listLen : int) (input : PuzzleInputItem list) =
    hashIterator 0 0 [0..(listLen - 1)] input


let main =
    let output =
        "157,222,1,2,177,254,0,228,159,140,249,187,255,51,76,30"
        |> strSplit ','
        |> List.map int
        |> hasherGen 256
    output.[0] * output.[1]

