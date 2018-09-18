// https://adventofcode.com/2017/day/12

module Day12

open System.Text.RegularExpressions
open Commons


type ID = string
type Node =
    { ID            : ID
      Connections   : ID list }

let getConnections node =
    List.filter (fun thisNode -> List.contains thisNode.ID node.Connections)

let getAllNodeRelations node nodeList =
    let rec gatherer nodeSet node =
        let connections = getConnections node nodeList
        let folder nodeSet node =
            if Set.contains node nodeSet then nodeSet
            else
                let newSet = Set.add node nodeSet
                gatherer newSet node

        connections
        |> List.fold folder nodeSet

    gatherer Set.empty node
    |> Set.toList


let parseNode str =
    let m = Regex.Match(str, @"(\d+) <-> (\d+(?:, \d+)*)")
    if m.Success then
        let matches = [for g in m.Groups -> g.Value ]
        let parsedIDs =
            matches
            |> List.item 2
            |> fun str -> str.Split ", "
            |> List.ofArray
        Some { ID = List.item 1 matches; Connections = parsedIDs }
    else None

let allNodes =
    "day12.txt"
    |> getLines
    |> List.choose parseNode

let part1func nodes =
    let node0 =
        nodes
        |> List.find (fun node -> node.ID = "0")
    getAllNodeRelations node0 nodes |> List.length


let part1 = part1func allNodes



