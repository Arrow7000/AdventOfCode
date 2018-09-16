module Day1Test

open Swensen.Unquote
open Xunit
open Day1

[<Fact>]
let ``Solution part 1`` () = test <@ main = 1029 @>

[<Theory>]
[<InlineData("1212", 6)>]
[<InlineData("1221", 0)>]
[<InlineData("123425", 4)>]
[<InlineData("123123", 12)>]
[<InlineData("12131415", 4)>]
let ``Halfway solution`` ((input, solution) : string * int) =
    let expected =
        input
        |> Seq.map (string >> int)
        |> part2func
    test <@ expected = solution @>

[<Fact>]
let ``Solution part 2`` () = test <@ part2 = 1220 @>
