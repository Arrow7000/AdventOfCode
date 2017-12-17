module Day6Test

open Swensen.Unquote
open Xunit
open Day6

[<Fact>]
let ``getMaxIndex works``() =
    let input1 = [0; 2; 7; 0]
    let result1 = getMaxIndex input1
    let expected1 = 2, 7

    test <@ result1 = expected1 @>

[<Fact>]
let ``getMaxIndex gets first of tie``() =
    let input1 = [0; 4; 4; 3]
    let result1 = getMaxIndex input1
    let expected1 = 1, 4

    test <@ result1 = expected1 @>

[<Fact>]
let ``distribute works``() =
    let input1 = [0; 2; 7; 0]
    let result1 = distribute input1
    let expected1 = [2; 4; 1; 2]

    test <@ result1 = expected1 @>

[<Fact>]
let ``in tie distribute picks first ocurring bank``() =
    let input1 = [3; 1; 2; 3]
    let result1 = distribute input1
    let expected1 = [0; 2; 3; 4]

    test <@ result1 = expected1 @>

//[<Fact>]
//let ``in tie distribute picks first ocurring bank``() =
//    let input1 = [3; 1; 2; 3]
//    let result1 = distribute input1
//    let expected1 = [0; 2; 3; 4]

//    test <@ result1 = expected1 @>