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

type TreeEntry = {
    name: string
    weight: int
    totalWeight: int
    children: Map<string, TreeEntry>
    descendants: Map<string, TreeEntry>
    }

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

let constructEntryMap (entryList : Entry list) =
    let namedTuples = 
        List.map (fun (entry : Entry) -> entry.name, entry) entryList
    new Map<string, Entry>(namedTuples)


let mapToList = Map.toList >> List.map snd

let rec convertEntryToTreeEntry (map : Map<string, Entry>) (entry : Entry) : TreeEntry =
    let childList = List.map (fun child -> convertEntryToTreeEntry map map.[child]) entry.children

    let childTupleList = List.map (fun child -> child.name, child) childList
    let childMap = new Map<string, TreeEntry>(childTupleList)

    let descendantList = 
        childList
        |> List.map ((fun entry -> entry.descendants) >> mapToList)
        |> List.fold (@) []

    let descendantTupleList = List.map (fun child -> child.name, child) descendantList
    let descendantMap = new Map<string, TreeEntry>(descendantTupleList)

    {
        name = entry.name
        children = childMap
        descendants = descendantMap
        weight = entry.weight
        totalWeight = 
            if childMap.Count < 1 then
                entry.weight
            else
                mapToList childMap
                |> List.map (fun entry -> entry.totalWeight)
                |> List.sum
     }

let constructWholeTree map (entries : Entry list) =
    entries
    |> List.map (convertEntryToTreeEntry map)
    |> List.maxBy (fun entry -> entry.totalWeight) // heaviest subtree should be full tree



let main =
    let entries =
        getLines "./day7.txt"
        |> List.map parseLineToEntry
        |> List.choose id
    let map = constructEntryMap entries
    let tree = constructWholeTree map entries
    tree.name