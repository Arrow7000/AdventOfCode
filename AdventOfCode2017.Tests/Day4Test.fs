module Day4Test

open Swensen.Unquote
open Xunit
open Day4

[<Fact>]
let ``Solution part 1`` () =
    test <@ main = 466 @>

[<Fact>]
let ``Solution part 2`` () =
    test <@ part2 = 251 @>
