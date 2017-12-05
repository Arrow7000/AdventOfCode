module Test

open Swensen.Unquote
open Xunit
open Day3

[<Fact>] 
let ``when coord is left``() =
    let c1 = (-1, 0)
    let c2 = (-2, -1)
    let c1d = whichSide c1
    let c2d = whichSide c2

    test <@ c1d = Left c1 @>
    test <@ c2d = Left c2 @>

[<Fact>] 
let ``when coord is right``() =
    let c1 = (1, 0)
    let c2 = (2, -1)
    let c1d = whichSide c1
    let c2d = whichSide c2

    test <@ c1d = Right c1 @>
    test <@ c2d = Right c2 @>

[<Fact>] 
let ``when coord is top``() =
    let c1 = (0, -1)
    let c2 = (1, -2)
    let c1d = whichSide c1
    let c2d = whichSide c2

    test <@ c1d = Top c1 @>
    test <@ c2d = Top c2 @>

[<Fact>] 
let ``when coord is bottom``() =
    let c1 = (0, 1)
    let c2 = (1, 2)
    let c1d = whichSide c1
    let c2d = whichSide c2

    test <@ c1d = Bottom c1 @>
    test <@ c2d = Bottom c2 @>



[<Fact>] 
let ``when coord is BottomRight``() =
    let c1 = (1, 1)
    let c2 = (2, 2)
    let c1d = whichCorner c1
    let c2d = whichCorner c2

    test <@ c1d = BottomRight c1 @>
    test <@ c2d = BottomRight c2 @>

[<Fact>] 
let ``when coord is TopRight``() =
    let c1 = (1, -1)
    let c2 = (2, -2)
    let c1d = whichCorner c1
    let c2d = whichCorner c2

    test <@ c1d = TopRight c1 @>
    test <@ c2d = TopRight c2 @>

[<Fact>] 
let ``when coord is TopLeft``() =
    let c1 = (-1, -1)
    let c2 = (-2, -2)
    let c1d = whichCorner c1
    let c2d = whichCorner c2

    test <@ c1d = TopLeft c1 @>
    test <@ c2d = TopLeft c2 @>

[<Fact>] 
let ``when coord is BottomLeft``() =
    let c1 = (-1, 1)
    let c2 = (-2, 2)
    let c1d = whichCorner c1
    let c2d = whichCorner c2

    test <@ c1d = BottomLeft c1 @>
    test <@ c2d = BottomLeft c2 @>