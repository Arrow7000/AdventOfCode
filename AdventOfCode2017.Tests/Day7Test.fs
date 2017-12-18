module Day7Test

open Swensen.Unquote
open Xunit
open Day7

//let inputLines =
//    ["wdysq (135) -> vjwuuft, oislgqy";
//    "vjwuuft (33) -> lphki";
//    "oislgqy (77)";
//    "lphki (233)"]

let inputLines =
    [
        "shds (102)";
        "sdfsdf (90)"
        "wdysq (135) -> sdfsdf, entry0name";
        "djfhkdf (12) -> shds, wdysq";
    ]


[<Fact>]
let ``makeEntry works``() =

    let entry = makeEntry "name2323" 234 ["child 1"; "child num 2"]
    let expectedEntry = 
        {
            name = "name2323"
            weight = 234
            children = ["child 1"; "child num 2"]
        }
    test <@ entry = expectedEntry @>


[<Fact>]
let ``parseLineToEntry works``() =
    let entryNoChild = parseLineToEntry inputLines.[0]
    let expectedNoChild = Some <| makeEntry "shds" 102 []

    let entryWithChildren = parseLineToEntry inputLines.[2]
    let expectedNoChildren = Some <| makeEntry "wdysq" 135 ["sdfsdf"; "entry0name"]

    test <@ entryNoChild = expectedNoChild @>
    test <@ entryWithChildren = expectedNoChildren @>


[<Fact>]
let ``linesToEntries works``() =

    let entries = linesToEntries inputLines

    let entry2 = makeEntry "shds" 102 []
    let entry4 = makeEntry "sdfsdf" 90 []
    let entry3 = makeEntry "wdysq" 135 [entry4.name; "entry0name"]
    let entry1 = makeEntry "djfhkdf" 12 [entry2.name; entry3.name]
    let expectedEntries = [entry2; entry4; entry3; entry1]


    test <@ entries = expectedEntries @>


//let getLines (text : string) = 
//    text.Split [|'\n'|]
//    |> Array.toList

//[<Fact>]
//let ``getLines works``() =
//    let testStr =
//        """line 1
//line 2
//line 3"""
//    let lines = getLines testStr
//    let expectedLines = ["line 1"; "line 2"; "line 3"]
//    test <@ lines = expectedLines @>