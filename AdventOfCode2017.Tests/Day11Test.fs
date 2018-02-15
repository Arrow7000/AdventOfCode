namespace Day11Test

open Swensen.Unquote
open Xunit
open Day11
open System.IO

module ``EvenCol single movements`` =
        
    let from = cell 0 0

    [<Fact>]
    let ``N works`` () =
        let result = moveCell N from

        test <@ result = EvenCol (0, -1) @>

    [<Fact>]
    let ``NE works`` () =
        let result = moveCell NE from

        test <@ result = OddCol (1, -1) @>

    [<Fact>]
    let ``SE works`` () =
        let result = moveCell SE from

        test <@ result = OddCol (1, 0) @>

    [<Fact>]
    let ``S works`` () =
        let result = moveCell S from

        test <@ result = EvenCol (0, 1) @>

    [<Fact>]
    let ``SW works`` () =
        let result = moveCell SW from

        test <@ result = OddCol (-1, 0) @>

    [<Fact>]
    let ``NW works`` () =
        let result = moveCell NW from

        test <@ result = OddCol (-1, -1) @>



module ``OddCol single movements`` =
        
    let from = cell 1 0

    [<Fact>]
    let ``N works`` () =
        let result = moveCell N from

        test <@ result = OddCol (1, -1) @>

    [<Fact>]
    let ``NE works`` () =
        let result = moveCell NE from

        test <@ result = EvenCol (2, 0) @>

    [<Fact>]
    let ``SE works`` () =
        let result = moveCell SE from

        test <@ result = EvenCol (2, 1) @>

    [<Fact>]
    let ``S works`` () =
        let result = moveCell S from

        test <@ result = OddCol (1, 1) @>

    [<Fact>]
    let ``SW works`` () =
        let result = moveCell SW from

        test <@ result = EvenCol (0, 1) @>

    [<Fact>]
    let ``NW works`` () =
        let result = moveCell NW from

        test <@ result = EvenCol (0, 0) @>

module ``Quickest path`` =

    [<Fact>]
    let ``Path 1`` () =
        let from = cell 0 0
        let dest = applyPath [NE; NE; NE] from
        let result = getSteps from dest
        
        test <@ result = 3 @>

    [<Fact>]
    let ``Path 2`` () =
        let from = cell 0 0
        let dest = applyPath [NE; NE; SW; SW] from
        let result = getSteps from dest
        
        test <@ result = 0 @>

    [<Fact>]
    let ``Path 3`` () =
        let from = cell 0 0
        let dest = applyPath [NE; NE; S; S] from
        let result = getSteps from dest
        
        test <@ result = 2 @>

    [<Fact>]
    let ``Path 4`` () =
        let from = cell 0 0
        let dest = applyPath [SE; SW; SE; SW; SW] from
        let result = getSteps from dest
        
        test <@ result = 3 @>

    