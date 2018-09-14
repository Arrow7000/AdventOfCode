module Commons

open System.IO

let inPath file = "./AdventOfCode2017/" + file

let getLines =
    inPath
    >> File.ReadAllLines 
    >> Array.toList

let getText =
    inPath >> File.ReadAllText

let strSplit sep (str : string) =
    str.Split([| sep |])
    |> Array.toList