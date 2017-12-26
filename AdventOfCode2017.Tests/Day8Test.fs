module Day8Test

open Swensen.Unquote
open Xunit
open Day8

[<Fact>]
let ``Test line to Op``() =
    let convertedLine1 = lineToOp "gug dec 188 if zpw >= 8"
    let convertedLine2 = lineToOp "qt inc 274 if d == -8"
    let op1 =
        {
            Register = "gug"
            OpType = Dec
            OpVal = 188
            CompareRegister = "zpw"
            CompareOp = Gte
            CompareVal = 8
        }

    let op2 =
        {
            Register = "qt"
            OpType = Inc
            OpVal = 274
            CompareRegister = "d"
            CompareOp = Eq
            CompareVal = -8
        }
    test <@ convertedLine1 = op1 @>
    test <@ convertedLine2 = op2 @>