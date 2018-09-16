module Day5Test

open Swensen.Unquote
open Xunit
open Day5

[<Fact>]
let ``Replace works as expected``() =
    let orig () = List.toArray [0..4]

    let replaced1 = replaceArray 0 5 (orig())
    test <@ replaced1 = List.toArray [5; 1; 2; 3; 4] @>

    let replaced2 = replaceArray 4 9 (orig())
    test <@ replaced2 = List.toArray [0; 1; 2; 3; 9] @>

[<Fact>]
let ``Solution part 1`` () =
    test <@ main() = 339351 @>

[<Fact>]
let ``Solution part 2`` () =
    test <@ part2() = 24315397 @>
