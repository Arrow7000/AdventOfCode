module Day10Test

open Swensen.Unquote
open Xunit
open Day10

[<Fact>]
let ``Reverse sublist``() =
    let list1 = [0..4]
    let indexes1 = [0; 1; 2]

    let expected1 = [2; 1; 0; 3; 4]

    test <@ reverseSublist indexes1 list1 = expected1 @>


    let list2 = [2; 1; 0; 3; 4]
    let indexes2 = [0; 1; 3; 4]

    let expected2 = [4; 3; 0; 1; 2]

    test <@ reverseSublist indexes2 list2 = expected2 @>


    let list3 = [3..10]
    let indexes3 = [7]

    test <@ reverseSublist indexes3 list3 = list3 @>


    let list4 = [4; 3; 0; 1; 2]
    let indexes4 = [1; 2; 3; 4; 0]

    let expected4 = [3; 4; 2; 1; 0]

    test <@ reverseSublist indexes4 list4 = expected4 @>


[<Fact>]
let ``Test hashIterator step 1``() =
    let input = [3; 4; 1; 5]
    let result = hashIterator 0 0 [0..4] <| List.take 1 input
    let expected = [2; 1; 0; 3; 4]

    test <@ result = expected @>

[<Fact>]
let ``Test hashIterator step 2``() =
    let input = [3; 4; 1; 5]
    let result = hashIterator 0 0 [0..4] <| List.take 2 input
    let expected = [4; 3; 0; 1; 2]

    test <@ result = expected @>

[<Fact>]
let ``Test hashIterator step 3``() =
    let input = [3; 4; 1; 5]
    let result = hashIterator 0 0 [0..4] <| List.take 3 input
    let expected = [4; 3; 0; 1; 2]

    test <@ result = expected @>

[<Fact>]
let ``Test hashIterator step 4``() =
    let input = [3; 4; 1; 5]
    let result = hashIterator 0 0 [0..4] <| List.take 4 input
    let expected = [3; 4; 2; 1; 0]

    test <@ result = expected @>

[<Fact>]
let ``Test hasher``() =
    let hasher = hasherGen 5
    let lengths = [3; 4; 1; 5]

    let expectedHash = [3; 4; 2; 1; 0]

    test <@ hasher lengths = expectedHash @>
