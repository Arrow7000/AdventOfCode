// http://adventofcode.com/2017

[<EntryPoint>]
let main argv = 
    Day1.main |> printf "%i\n"
    Day2.main |> printf "%i\n"

    System.Console.ReadLine() |> ignore
    0
