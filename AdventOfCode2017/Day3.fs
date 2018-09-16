// http://adventofcode.com/2017/day/3

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

let whichCorner (coord : Coord) = 
    if abs (fst coord) = abs (snd coord) then
        match coord with
        | (x, y) when x > 0 && y > 0 -> BottomRight coord
        | (x, y) when x > 0 && y < 0 -> TopRight coord
        | (x, y) when x < 0 && y < 0 -> TopLeft coord
        | (x, y) when x < 0 && y > 0 -> BottomLeft coord
        | _ -> failwith "Doesn't match any corner"
    else failwith "Is not corner!"

let whichSide (coord : Coord) =
    match coord with
    | (x, y) when x > abs y -> Right coord
    | (x, y) when y < -(abs x) -> Top coord
    | (x, y) when -x > (abs y) -> Left coord
    | (x, y) when y > (abs x) -> Bottom coord
    | _ -> failwith "Doesn't match any side"


let (|Center|Corner|Side|) (coord : Coord) =
    match coord with
    | _ when coord = origin -> Center
    | (x,y) when abs x = abs y -> Corner (whichCorner coord)
    | _ -> Side (whichSide coord)


let nextStep (coord : Coord) : Coord =
    match coord with
    | Center -> 1,0
    | Corner corner -> 
        match corner  with
        | TopRight (x, y) -> (x - 1, y)
        | TopLeft (x, y) -> (x, y + 1)
        | BottomLeft (x, y) | BottomRight (x, y) -> (x + 1, y)
    | Side side ->
        match side with
        | Right (x, y) -> (x, y - 1)
        | Top (x, y) -> (x - 1, y)
        | Left (x, y) -> (x, y + 1)
        | Bottom (x, y) -> (x + 1, y)

let stepper n : Coord =
    let rec stepIter nLeft start =
        if nLeft < 1 then failwith "Smallest square is 1"
        else if nLeft = 1 then
            start
        else
            stepIter (nLeft - 1) (nextStep start)
    stepIter n (0, 0)


let taxiDist (coord : Coord) =
    let dirDist get = coord |> get |> abs
    dirDist fst + dirDist snd

let part1func = stepper >> taxiDist
let main = part1func 277678




let getNeighbours (coord : Coord) : Coord list =
    let x,y = coord
    [ x+1,y+1
      x+1,y
      x+1,y-1
      x,y-1
      x-1,y-1
      x-1,y
      x-1,y+1
      x,y+1 ]

let stepperPart2 threshold =
    let rec traverser (map : Map<Coord,int>) coord =
        if coord = origin then
            traverser (Map.add coord 1 map) (nextStep coord)
        else
            let neighbours = getNeighbours coord
            let neighbourSum =
                map
                |> Map.filter (fun key _ -> List.contains key neighbours)
                |> Map.toList
                |> List.map snd
                |> List.fold (+) 0
            if neighbourSum > threshold then
                neighbourSum
            else
                traverser (Map.add coord neighbourSum map) (nextStep coord)

    traverser Map.empty origin




let part2 = stepperPart2 277678
