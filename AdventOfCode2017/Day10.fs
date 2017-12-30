// https://adventofcode.com/2017/day/10

module Day10

type inputNum = int


let hasher input int list =
    let rec iterator (currPos : int) (hashList : int list) (remainingInput : int list) =
        match remainingInput with
        | [] -> ()
        | hash :: rest ->
            let listLen = List.length hashList
            let indexes = 
                [0..hash]
                |> List.map (fun i -> (i + currPos) % listLen)
            let toRev =
                indexes
                |> List.map (fun i -> hashList.[i])
            let reved = List.rev toRev
            let replaced =
                hashList
                |> List.mapi (fun i hashChar -> if List.contains i)

            //let toRev = 
            //    remainingInput
            //    |> List.skip (currPos - 1)
            //    |> List.take hash
            //let reved = List.rev toRev
            //iterator (currPos+1)    rest


    iterator 0 [0..255] remainingInput
