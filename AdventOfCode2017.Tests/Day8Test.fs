module Day8Test

open Swensen.Unquote
open Xunit
open Day8

[<Fact>]
let ``Test line to Op``() =
    let convertedLine1 = lineToOp "gug dec 188 if zpw >= 8"
    let convertedLine2 = lineToOp "qt inc 274 if d == -8"

    let op1 = makeOp "gug" Dec 188 "zpw" Gte 8
    let op2 = makeOp "qt" Inc 274 "d" Eq -8

    test <@ convertedLine1 = op1 @>
    test <@ convertedLine2 = op2 @>


[<Fact>]
let ``getAllRegisters gets unique registers from op list``() =
    // a b c zpw def 

    let regs1 = ["a"; "b"; "c"; "def"]
    let regs2 = ["c"; "zpw"; "def"; "a"]

    let regs = 
        List.map2 (fun a b -> makeOp a Inc 1 b Eq -1 ) regs1 regs2
        |> getAllRegisters
        |> List.sort

    let expectedRegs =
        ["a"; "b"; "c"; "def"; "zpw"]
        |> List.sort

    test <@ regs = expectedRegs @>


[<Fact>]
let ``executeOp returns correct new value``() =
    let val1 = executeOp Inc 3 4
    let val2 = executeOp Dec 9 14

    test <@ val1 = 7 @>
    test <@ val2 = -5 @>


[<Fact>]
let ``executeRegisters works on single operations``() =
    let regs = ["a"; "b"; "zpw"]
    let op = makeOp "a" Inc 2 "zpw" Gte -1
    let expectedRegsVals = ["a",2; "b",0; "zpw",0]

    let result = executeRegisters regs [op]

    test <@ result = expectedRegsVals @>