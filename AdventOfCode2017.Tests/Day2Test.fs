module Day2Test

open Swensen.Unquote
open Xunit
open Day2

[<Fact>]
let ``Solution part 1`` () = test <@ main = 42299 @>
