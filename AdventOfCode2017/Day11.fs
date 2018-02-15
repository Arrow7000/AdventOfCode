// https://adventofcode.com/2017/day/11

module Day11

open Commons

type x = int
type y = int
type afterY = int
type CellCoord = 
    | EvenCol of x * y
    | OddCol of x * afterY // y is 'after y'

let cell x y : CellCoord =
    if x % 2 = 0 then EvenCol (x, y) else OddCol (x, y)

//type CellCoord = x * y

//let isValidCell = 
//    function
//    | EvenCol (x, y) -> true
//    | OddCol (x, y) when y % 1.0 = 0.5 -> true
//    | _ -> false

type Direction =
    | N
    | NE
    | SE
    | S
    | SW
    | NW

let origin : CellCoord = EvenCol (0, 0)

let moveCell (dir : Direction) (cellCrd : CellCoord) : CellCoord =
    match cellCrd with
    //| EvenCol (x, y) ->
    //    match dir with
    //    | N -> EvenCol (x, y - 1)
    //    | NE -> OddCol (x + 1, y - 1)
    //    | SE -> OddCol (x + 1, y)
    //    | S -> EvenCol (x, y + 1)
    //    | SW -> OddCol (x - 1, y)
    //    | NW -> OddCol (x - 1, y - 1)
    //| OddCol (x, y) ->
    //    match dir with
    //    | N -> OddCol (x, y - 1)
    //    | NE -> EvenCol (x + 1, y)
    //    | SE -> EvenCol (x + 1, y + 1)
    //    | S -> OddCol (x, y + 1)
    //    | SW -> EvenCol (x - 1, y + 1)
    //    | NW -> EvenCol (x - 1, y)
    | EvenCol (x, y) ->
        match dir with
        | N -> cell x (y - 1)
        | NE -> cell (x + 1) (y - 1)
        | SE -> cell (x + 1) y
        | S -> cell x (y + 1)
        | SW -> cell (x - 1) y
        | NW -> cell (x - 1) (y - 1)
    | OddCol (x, y) ->
        match dir with
        | N -> cell x (y - 1)
        | NE -> cell (x + 1) y
        | SE -> cell (x + 1) (y + 1)
        | S -> cell x (y + 1)
        | SW -> cell (x - 1) (y + 1)
        | NW -> cell (x - 1) y


let rec applyPath (dirs : Direction list) cell : CellCoord =
    dirs |> List.fold (fun from dir -> moveCell dir from) cell

let getDiff ax ay bx by = abs (ax - bx) + abs (ay - by)

let getXY = 
    function
    | EvenCol (x,y) -> x,y
    | OddCol (x,y) -> x,y

let getOneCloser fromCell toCell : Direction = // only intended for cells of different types. Otherwise simple Cartesian coordinate maths will do.
    let failMsg = "getOneCloser used on cells of same type"
    match fromCell with
    | EvenCol (fromX, fromY) ->
        match toCell with
        | OddCol (toX, toY) -> // will only need 4 for the cross-parity cells
            if fromX < toX then
                if fromY <= toY then SE
                else NE
            else
                if fromY <= toY then SW
                else NW
        | EvenCol _ -> failwith failMsg
    | OddCol (fromX, fromY) ->
        match toCell with
        | EvenCol (toX, toY) ->
            if fromX < toX then 
                if fromY >= toY then SE
                else NE
            else
                if fromY >= toY then SW
                else NW
        | OddCol _ -> failwith failMsg
        

let getSteps (fromCell : CellCoord) (toCell : CellCoord) : int =
    match fromCell with
    | EvenCol (fromX, fromY) ->
        match toCell with
        | EvenCol (toX, toY) -> 
            getDiff fromX fromY toX toY
        | OddCol (toX, toY) ->
            let x, y = moveCell (getOneCloser fromCell toCell) fromCell |> getXY                
            1 + getDiff fromX fromY x y
    | OddCol (fromX, fromY) ->
        match toCell with
        | OddCol (toX, toY) ->
            getDiff fromX fromY toX toY
        | EvenCol (toX, toY) -> 
            let x, y = moveCell (getOneCloser fromCell toCell) fromCell |> getXY
            1 + getDiff fromX fromY x y

