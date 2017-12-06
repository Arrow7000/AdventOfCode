namespace Day3

open Swensen.Unquote
open Xunit
open Day3

module SidesAndCorners =

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


module NextStep =

    [<Fact>]
    let ``Next steps should be correct``() =
        let next1 = nextStep (0,0)
        test <@ next1 = (1,0) @>

        let next2 = nextStep (1,0)
        test <@ next2 = (1,-1) @>

        let next3 = nextStep (1,-1)
        test <@ next3 = (0,-1) @>

        let next4 = nextStep (1,-2)
        test <@ next4 = (0,-2) @>

        let next5 = nextStep (-2,-2)
        test <@ next5 = (-2,-1) @>


module Stepper =

    [<Fact>]
    let ``stepper works``() =
        let coord1 = stepper 1
        test <@ coord1 = (0,0) @>

        let coord2 = stepper 2
        test <@ coord2 = (1,0) @>

        let coord3 = stepper 5
        test <@ coord3 = (-1,-1) @>

        let coord4 = stepper 20
        test <@ coord4 = (-2,1) @>

        let coord5 = stepper 17
        test <@ coord5 = (-2,-2) @>

        let expectedSteps = [(0,0); 
                             (1,0); 
                             (1,-1); 
                             (0,-1); 
                             (-1,-1); 
                             (-1,0); 
                             (-1,1); 
                             (-1,-1); 
                             (-1,-1); 
                             (-1,-1); 
                            ]
        
        let steps = 
            [1..5]
            |> List.map stepper
        
        <@ expectedSteps = steps @>

module Misc =

    [<Fact>]
    let ``taxiDist works``() =
        let dist1 = taxiDist (3, -14)
        test <@ dist1 = 17 @>

        let dist2 = taxiDist (-17, 31)
        test <@ dist2 = 48 @>