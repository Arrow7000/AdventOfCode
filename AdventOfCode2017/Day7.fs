// http://adventofcode.com/2017/day/7

module Day7
open System.IO
open System
open System.Text.RegularExpressions

type Name = string

type Entry = {
    name: string
    weight: int
    children: Name list
    }

//type TreeEntry = {
//    name: string
//    weight: int
//    children: Map<string, Entry option>
//    }

//type EntryOrName =
//    | Entry of Entry
//    | Name of string


//let childToMapItem child : string * Entry option =
//    match child with
//    | Entry entry -> (entry.name, Some entry)
//    | Name entry -> (entry, None)

//let makeEntry name weight (children : EntryOrName list) =
//    let mapEntries = List.map childToMapItem children

//    let entryMap = 
//        (new Map<string, Entry option>(mapEntries))
//    {
//        name = name
//        weight = weight
//        children = entryMap
//    }


let makeEntry name weight children : Entry =
    {
        name = name
        weight = weight
        children = children
    }

let getLines file =
    File.ReadAllLines file
    |> Array.toList

    
let (|LineEntryMatch|_|) line =
    let regMatch = Regex.Match (line, "(\w+) \((\d+)\)(?: -> ([\w, ]+))?")
    if not regMatch.Success then
        None
    else
        let getGroup (i : int) = regMatch.Groups.[i]

        let name, weight, childrenStr = getGroup 1, getGroup 2, getGroup 3

        let children = 
            if childrenStr.Success then
                childrenStr.Value.Split ([|", "|], StringSplitOptions.None)
                |> Array.toList
                |> Some
            else
                None
        
        Some <|
        makeEntry
            name.Value
            (int weight.Value)
            (match children with
             | Some children -> children
             | None -> [])

let parseLineToEntry line =
    match line with
    | LineEntryMatch entry -> Some entry
    | _ -> None

let linesToEntries lines =
    lines
    |> List.map parseLineToEntry
    |> List.choose id
