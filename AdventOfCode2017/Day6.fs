// http://adventofcode.com/2017/day/6

module Day6

open Commons

type Blocks = int

let banks: Blocks list = 
    "day6.txt"
    |> getText
    |> strSplit '\t'
    |> List.map int


let getMaxIndex vals =
    vals
    |> List.mapi (fun i v-> i, v)
    |> List.maxBy snd

let makeIndexFirst i banks =
    let atEnd = List.take i banks
    let atStart = List.skip i banks
    atStart @ atEnd

let setListItemTo setter index list =
    list
    |> List.mapi (fun i item -> if i = index then setter i item else item)

let rec distribute startIndex blocks banks =
    if blocks < 1 then banks
    else
        let boundedIndex = startIndex % (List.length banks)
        banks
        |> setListItemTo (fun _ bank -> bank + 1) boundedIndex
        |> distribute (startIndex+1) (blocks-1)

let takeAndDistribute banks =
    let maxIndex, blocks = getMaxIndex banks
    banks
    |> setListItemTo (fun _ _ -> 0) maxIndex
    |> distribute (maxIndex+1) blocks

let redistributeUntilRepeat banks =
    let rec repeater iterations stateSet banks =
        let newBanks = takeAndDistribute banks
        let newIters = iterations + 1
        if Set.contains newBanks stateSet then newIters
        else
            let newSet = Set.add newBanks stateSet
            repeater newIters newSet newBanks
    repeater 0 Set.empty banks


let part1 = redistributeUntilRepeat banks
