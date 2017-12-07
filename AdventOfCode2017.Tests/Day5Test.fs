module Day5Test

open Swensen.Unquote
open Xunit
open Day5

[<Fact>]
let ``Replace works as expected``() =
    let orig = [0..4]
    let replaced1 = replace 0 5 orig
    test <@ replaced1 = [5; 1; 2; 3; 4] @>

    let replaced2 = replace 4 9 orig
    test <@ replaced2 = [0; 1; 2; 3; 9] @>
