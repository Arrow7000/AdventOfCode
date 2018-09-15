module Day1Test

open Swensen.Unquote
open Xunit
open Day1

[<Fact>]
let ``Solution part 1`` () = test <@ main = 1029 @>
