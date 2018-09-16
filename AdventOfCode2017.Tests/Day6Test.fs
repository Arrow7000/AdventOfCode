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
let ``makeIndexFirst works`` () =
    let input = [0..7]
    let result = makeIndexFirst 3 input
    let expected = [3;4;5;6;7;0;1;2]
    test <@ result = expected @>

[<Theory>]
[<InlineData(0,2,7,0,2,4,1,2)>]
[<InlineData(2,4,1,2,3,1,2,3)>]
[<InlineData(3,1,2,3,0,2,3,4)>]
[<InlineData(0,2,3,4,1,3,4,1)>]
[<InlineData(1,3,4,1,2,4,1,2)>]
let ``takeAndDistribute works``(i1,i2,i3,i4,o1,o2,o3,o4) =
    let input = [i1;i2;i3;i4]
    let result = takeAndDistribute input
    let expected = [o1;o2;o3;o4]

    test <@ result = expected @>

[<Fact>]
let ``in tie distribute picks first ocurring bank``() =
    let input1 = [3; 1; 2; 3]
    let result1 = takeAndDistribute input1
    let expected1 = [0; 2; 3; 4]

    test <@ result1 = expected1 @>

[<Fact>]
let ``Solution part 1`` () = test <@ part1 = 6681 @>
