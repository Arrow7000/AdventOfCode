// http://adventofcode.com/2017/day/9

module Day9

open Commons
open System.Text.RegularExpressions


let chars = getText "./day9.txt"

let deleteCancelleds input =
    Regex.Replace(input, "!.", "")

let deleteGarbage input =
    let str = deleteCancelleds input
    Regex.Replace(str, "<[^>]*>", "")
    
let countGroups input =
    let rec traverser levelsDeep completed (str : char list) =
        match str with
        | char :: rest ->
            match char with
            | '{' ->
                traverser (levelsDeep + 1) completed rest
            | '}' ->
                traverser (levelsDeep - 1) (completed + 1 * levelsDeep) rest
            | _ -> traverser levelsDeep completed rest
        | [] -> completed
    traverser 0 0 <| Seq.toList (deleteGarbage input)

let main =
    countGroups chars




let countGarbage input =
    let str = deleteCancelleds input
    let matches = Regex.Matches(str, "<[^>]*>")
    let matchSeq = seq { for m in matches -> m}
    Seq.map (fun (m : Match) -> m.Length - 2) matchSeq
    |> Seq.sum

let part2 = countGarbage chars
