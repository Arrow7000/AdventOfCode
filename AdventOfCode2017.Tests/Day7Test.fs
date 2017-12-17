module Day7Test

open Swensen.Unquote
open Xunit
open Day7

let inputLines =
    ["wdysq (135) -> vjwuuft, oislgqy";
    "vjwuuft (33) -> lphki";
    "oislgqy (77)";
    "lphki (233)"]


[<Fact>]
let ``linesToEntries works``() =
    let entries = linesToEntries inputLines

    let entry2 = makeEntry "shds" 102 []
    let entry4 = makeEntry "sdfsdf" 90 []
    let entry3 = makeEntry "shds" 102 [entry4]
    let entry1 = makeEntry "djfhkdf" 12 [entry2; entry3]
    let expectedEntries = [entry1; entry2; entry3; entry4]

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