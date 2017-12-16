// http://adventofcode.com/2017/day/7

module Day7
open System.IO

type Entry = {
    name: string
    weight: int
    children: Map<string, Entry option>
    }

type EntryOrName =
    | Entry of Entry
    | Name of string

let makeEntry name weight (children : EntryOrName list) =
    let entryMap = 
        (new Map<string, Entry option>(
            List.map (fun entry -> entry) children
            ))
    {
        name = name
        weight = weight
        children = entryMap
    }

let getLines file =
    File.ReadAllLines file
    |> Array.toList

let linesToEntries lines =
    lines
    |> List.map 
