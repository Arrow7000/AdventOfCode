module Day12Test

open Swensen.Unquote
open Xunit
open Day12
open Commons

[<Fact>]
let ``Get all node relations`` () =
    let nodeA =
        {ID = "0"; Connections = ["1";"2"]}
    let nodeB =
        {ID = "1"; Connections = ["0"]}
    let nodeC =
        {ID = "2"; Connections = ["0";"3"]}
    let nodeD =
        {ID = "3"; Connections = ["2"]}
    let nodeE =
        {ID = "4"; Connections = ["5"]}
    let nodeF =
        {ID = "5"; Connections = ["4"]}
    let nodeList = [nodeA; nodeB; nodeC; nodeD; nodeE; nodeF]
    let result1 =
        getAllNodeRelations nodeC nodeList

    test <@ result1 = [nodeA; nodeB; nodeD] @>

    let result2 =
        getAllNodeRelations nodeF nodeList
    test <@ result2 = [nodeE] @>


[<Fact>]
let ``part1func test`` () =
    let nodesStr =
        """0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
"""
    let nodes = 
        nodesStr
        |> strSplit '\n'
        |> List.choose parseNode

    let result = part1func nodes

    test <@ result = 6 @>

[<Fact>]
let ``Solution part 1`` () =
    test <@ part1 = 175 @>
