// https://adventofcode.com/2017/day/10

module Day10

open Commons

type Length = int
type HashNum = int

let getLengths str : Length list =
    str
    |> strSplit ','
    |> List.map int


let reverseSublist (indexes : int list) (list : 'a list) =
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

let hasherGen (listLen : int) (input : Length list) =
    let rec iterator (currPos : int) (skipSize : int) (hashList : HashNum list) (remainingInput : Length list) =
        match remainingInput with
        | [] -> hashList
        | hash :: rest ->
            let listLen = List.length hashList
            let indexes = 
                [0..(hash - 1)]
                |> List.map (fun i -> (i + currPos) % listLen)
            let replaced = reverseSublist indexes hashList
            
            let newCurrPos = (currPos + 1 + hash + skipSize) % listLen
            iterator newCurrPos (skipSize + 1) replaced rest


    iterator 0 0 [0..(listLen - 1)] input

let main =
    "157,222,1,2,177,254,0,228,159,140,249,187,255,51,76,30"
    |> strSplit ','
    |> List.map int
    |> (hasherGen 256)