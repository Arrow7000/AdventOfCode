module Day3

type Coord = (int * int)

type Corner =
    | TopLeft of Coord
    | TopRight of Coord
    | BottomRight of Coord
    | BottomLeft of Coord

type Side =
    | Top of Coord
    | Right of Coord
    | Bottom of Coord
    | Left of Coord

let origin = 0,0

let isCorner coord =
    abs (fst coord) = abs (snd coord)

let whichCorner coord = 
    if abs (fst coord) = abs (snd coord) then
        match coord with
        | (x, y) when x > 0 && y > 0 -> BottomRight coord
        | (x, y) when x > 0 && y < 0 -> TopRight coord
        | (x, y) when x < 0 && y < 0 -> TopLeft coord
        | (x, y) when x < 0 && y > 0 -> BottomLeft coord
        | _ -> failwith "Doesn't match any corner"
    else failwith "Is not corner!"

let whichSide coord =
    match coord with
    | (x, y) when x > abs y -> Right coord
    | (x, y) when y < -(abs x) -> Top coord
    | (x, y) when -x > (abs y) -> Left coord
    | (x, y) when y > (abs x) -> Bottom coord
    | _ -> failwith "Doesn't match any side"



let nextStep coord =
    match coord with
    | 0,0 -> 1,0
    | coord when isCorner coord -> 
        match whichCorner coord  with
        | TopRight (x, y) -> (x - 1, y)
        | TopLeft (x, y) -> (x, y + 1)
        | BottomLeft (x, y) | BottomRight (x, y) -> (x + 1, y)
    | _ ->
        match whichSide coord with
        | Right (x, y) -> (x, y - 1)
        | Top (x, y) -> (x - 1, y)
        | Left (x, y) -> (x, y + 1)
        | Bottom (x, y) -> (x + 1, y)

let stepper n =
    let rec stepIter nLeft start =
        if nLeft < 1 then failwith "Smallest square is 1"
        else if nLeft = 1 then
            start
        else
            stepIter (nLeft - 1) (nextStep start)
    stepIter n (0, 0)

let taxiDist coord = 
    [fst; snd]
    |> List.map ((fun get -> get coord) >> abs)
    |> List.sum


let main = 
    stepper 277678 |> taxiDist