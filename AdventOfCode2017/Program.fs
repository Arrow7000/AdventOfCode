// http://adventofcode.com/2017

[<EntryPoint>]
let main argv = 
    Day1.main |> printfn "Day 1: %i"
    Day1.part2 |> printfn "Day 1 part 2: %i"

    Day2.main |> printfn "Day 2: %i"
    Day2.part2 |> printfn "Day 2 part 2: %i"

    Day3.main |> printfn "Day 3: %i"
    Day3.part2 |> printfn "Day 3 part 2: %i"

    Day4.main |> printfn "Day 4: %i"
    Day4.part2 |> printfn "Day 4 part 2: %i"

    Day5.main () |> printfn "Day 5: %i"
    Day5.part2 () |> printfn "Day 5 part 2: %i"

    Day6.part1 |> printfn "Day 6: %i"
    Day6.part2 |> printfn "Day 6 part 2: %i"

    Day7.main |> printfn "Day 7: %s"
    Day7.part2 |> printfn "Day 7 part 2: %i"

    Day8.main |> printfn "Day 8: %i"
    Day8.part2 |> printfn "Day 8 part 2: %i"
    
    Day9.main |> printfn "Day 9: %i"
    Day9.part2 |> printfn "Day 9 part 2: %i"
    
    Day10.main |> printfn "Day 10: %A"
    printfn "All Finished!"
    0
