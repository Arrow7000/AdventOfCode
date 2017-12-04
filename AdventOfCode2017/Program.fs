// http://adventofcode.com/2017

[<EntryPoint>]
let main argv = 
    Day1.main |> printf "Day 1: %i\n"
    Day2.main |> printf "Day 2: %i\n"
    Day2.part2 |> printf "Day 2 part 2: %i\n"

    System.Console.ReadLine() |> ignore
    0
