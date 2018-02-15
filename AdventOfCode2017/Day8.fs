// http://adventofcode.com/2017/day/8

module Day8

open System.Text.RegularExpressions
open Commons


type Register = string
type RegisterVal = int
type OpType = Inc | Dec
type OpVal = int
type CompareType = Gt | Lt | Gte | Lte | Eq | Neq
type AppliedCond = RegisterVal -> bool
type Condition = RegisterVal -> AppliedCond

let getOp opStr =
    match opStr with
    | "inc" -> Inc
    | "dec" -> Dec
    | _ -> failwith "Not a valid register operation"
    
let getCompareOp opStr : CompareType =
    match opStr with
    | ">" -> Gt
    | "<" -> Lt
    | ">=" -> Gte
    | "<=" -> Lte
    | "==" -> Eq
    | "!=" -> Neq
    | _-> failwith "Not a valid comparison operator"

let compareOpToCondition compareType : Condition =
    match compareType with
    | Gt -> (>)
    | Lt -> (<)
    | Gte -> (>=)
    | Lte -> (<=)
    | Eq -> (=)
    | Neq -> (<>)
    

type Op =
    {
        Register: Register
        OpType: OpType
        OpVal: RegisterVal
        CompareRegister: Register
        CompareOp: CompareType
        CompareVal: RegisterVal
    }

let makeOp register opType opVal condReg compareOp compareVal =
    {
        Register = register
        OpType = opType
        OpVal = opVal
        CompareRegister = condReg
        CompareOp = compareOp
        CompareVal = compareVal
    }

let lineToOp line =
    let reg =
        "^(\w{1,3}) (inc|dec) (-?\d+) if (\w{1,3}) (>|<|>=|<=|==|!=) (-?\d+)$"
    let mat = Regex.Match(line, reg)
    let getGr (i : int) = mat.Groups.[i].Value
    let register, opType, opVal, condReg, compareOp, compareVal =
        getGr 1, getOp (getGr 2), int (getGr 3), getGr 4, getCompareOp (getGr 5), int (getGr 6)
    
    makeOp register opType opVal condReg compareOp compareVal


let getAllRegisters ops : Register list =
    ops
    |> List.map ((fun op -> op.Register, op.CompareRegister) >> (fun item -> [fst item; snd item]))
    |> List.reduce (@)
    |> List.distinct


let executeOp opType regVal opVal : RegisterVal =
    let operation = 
        match opType with
        | Inc -> (+)
        | Dec -> (-)
    operation regVal opVal

let getStepMap (regMap : Map<Register, RegisterVal>) (op : Op) =
    let compReg, compOp, compVal = op.CompareRegister, op.CompareOp, op.CompareVal
    let compRegActual = regMap.[compReg]
    let shouldChange = (compareOpToCondition compOp) compRegActual compVal
    if shouldChange then
        let reg = op.Register
        let regVal = regMap.[reg]
        let newVal = executeOp op.OpType regVal op.OpVal
        regMap.Add(reg, newVal)
    else
        regMap


let executeRegisters registers ops : (Register * RegisterVal) list =
    let mapList = List.map (fun reg -> reg, 0) registers
    let map = new Map<Register, RegisterVal>(mapList)
    let rec iterOps map ops =
        match ops with
        | op :: restOps ->
            let newMap = getStepMap map op
            iterOps newMap restOps
        | [] ->
            Map.toList map
            
    iterOps map ops


let ops =
    getLines "./day8.txt"
    |> List.map lineToOp

let registers = getAllRegisters ops


let main =
    let newRegisters = executeRegisters registers ops
    List.maxBy snd newRegisters
    |> snd
    

let executeRegistersGetMax registers ops =
    let mapList = List.map (fun reg -> reg, 0) registers
    let map = new Map<Register, RegisterVal>(mapList)
    let rec iterOps map ops max =
        match ops with
        | op :: restOps ->
            let newMap = getStepMap map op
            let highest = 
                Map.toList newMap
                |> List.map snd
                |> (@) [max]
                |> List.max
            iterOps newMap restOps highest
        | [] ->
            max
            
    iterOps map ops 0

let part2 =
    executeRegistersGetMax registers ops