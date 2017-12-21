module Commons

open System.IO


let getLines file =
    File.ReadAllLines file
    |> Array.toList

let getText file =
    File.ReadAllText file

let strSplit sep str =
    str.Split([| sep |]))
    |> Array.toList