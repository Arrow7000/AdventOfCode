// http://adventofcode.com/2017/day/7

module Day7
open System.IO
open System
open System.Text.RegularExpressions

type Name = string
type Weight = int

type SimpleEntry = {
    name: Name
    weight: Weight
    children: Name list
    }

type TreeEntry = {
    name: Name
    ownWeight: Weight
    totalWeight: Weight
    ownChildren: TreeEntry list
    descendants: TreeEntry list
    }


let makeEntry name weight children = { name = name; weight = weight; children = children }

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

let constructEntryMap (entryList : SimpleEntry list) =
    let namedTuples = 
        List.map (fun (entry : SimpleEntry) -> entry.name, entry) entryList
    new Map<string, SimpleEntry>(namedTuples)
    

let rec convertEntryToTreeEntry (map : Map<string, SimpleEntry>) (entry : SimpleEntry) : TreeEntry =
    let childList = List.map (fun child -> convertEntryToTreeEntry map map.[child]) entry.children

    let descendantList = 
        childList
        |> List.map (fun entry -> entry.descendants)
        |> List.fold (@) childList

    { name = entry.name
      ownChildren = childList
      descendants = descendantList
      ownWeight = entry.weight
      totalWeight = 
        if childList.Length < 1 then
            entry.weight
        else
            childList
            |> List.map (fun entry -> entry.totalWeight)
            |> List.sum }

let constructWholeTree map (entries : SimpleEntry list) =
    entries
    |> List.map (convertEntryToTreeEntry map)
    |> List.maxBy (fun entry -> entry.totalWeight) // heaviest subtree will be the root tree



let main =
    let entries =
        getLines "./day7.txt"
        |> List.map parseLineToEntry
        |> List.choose id
    let map = constructEntryMap entries
    let tree = constructWholeTree map entries
    tree.name


let getWeight entry = entry.ownWeight

let getUnBalanced (tree : TreeEntry) =
    let rec traverser tree =
        let children = tree.ownChildren
        let distinct =
            children
            |> List.groupBy (fun entry -> entry.ownWeight)
            |> List.distinctBy fst
            |> List.length
            |> (<) 1

        //let max = List.maxBy getWeight children
        //let min = List.minBy getWeight children
        //let hasDiff = max > min
        //if hasDiff then
        //    let highVotes = List.filter (fun entry -> entry.ownWeight = max) children
        //    let lowVotes = List.filter (fun entry -> entry.ownWeight = min) children
        
        //    List.groupBy
        //else
        //    None

        
    traverser tree



let part2 =
    let entries =
        getLines "./day7.txt"
        |> List.map parseLineToEntry
        |> List.choose id
    let map = constructEntryMap entries
    let tree = constructWholeTree map entries
