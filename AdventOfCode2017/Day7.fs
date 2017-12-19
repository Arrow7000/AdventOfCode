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
    descendantWeight: Weight
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

    let descendantWeight =
        childList
        |> List.map (fun entry -> entry.totalWeight)
        |> List.sum


    { name = entry.name
      ownChildren = childList
      descendants = descendantList
      descendantWeight = descendantWeight
      ownWeight = entry.weight
      totalWeight = 
        if childList.Length < 1 then
            entry.weight
        else
            descendantWeight + entry.weight }

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

//let getUnBalanced (tree : TreeEntry) =
//    let rec traverser tree =
//        let childCount = tree.ownChildren.Length
//        if childCount > 0 && childCount < 3 then
//            printf "Tree has %i children\n" childCount
//            if childCount = 2 then
//                let w1 = tree.ownChildren.[0].totalWeight
//                let w2 = tree.ownChildren.[1].totalWeight
//                if w1 <> w2 then
//                    printf "  Child 1 weighs %i, child 2 weighs %i\n" w1 w2
//        List.map traverser tree.ownChildren |> ignore
//        ()
//    traverser tree
//    0

type Number = int
let getOddOneOut (getter : 'a -> Number) (items : 'a list) : 'a option =
    let countGroups =
        items
        |> List.countBy getter
    
    if items.Length < 3 then
        None
    else
        let oddWeight = 
            countGroups
            |> List.find (fun group -> snd group = 1)
            |> fst

        items
        |> List.find (fun item -> getter item = oddWeight)
        |> Some



let getUnBalanced (tree : TreeEntry) =
    let rec traverser isInsideOdd tree =
        let children = tree.ownChildren
        let childCount = children.Length
        if childCount < 2 then
            None
        else
            let oddOne = getOddOneOut getWeight children
            match oddOne with
            | None -> None
            | Some odd -> 
                Some (List.choose (traverser true) children)
    
    traverser tree


let part2 =
    let entries =
        getLines "./day7.txt"
        |> List.map parseLineToEntry
        |> List.choose id
    let map = constructEntryMap entries
    let tree = constructWholeTree map entries
    getUnBalanced tree

