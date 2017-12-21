// http://adventofcode.com/2017/day/8

module Day8

open System.Text.RegularExpressions
open Commons


type Register = string
type RegisterVal = int
type OpType = Inc | Dec
type OpVal = int
type AppliedCond = Register -> bool
type Condition = Register -> AppliedCond
//type CompareOp = 
//    | Gt of Condition
//    | Lt of Condition
//    | Gte of Condition
//    | Lte of Condition
//    | Eq of Condition
//    | Neq of Condition

let getOp opStr =
    match opStr with
    | "inc" -> Inc
    | "dec" -> Dec
    | _ -> failwith "Not a valid register operation"

let executeOp op reg opVal : RegisterVal =
    let operation =
        match op with
        | Inc -> (+)
        | Dec -> (-)
    operation reg opVal

let getCompareOp opStr : Condition =
    match opStr with
    | ">" -> (>)
    | "<" -> (<)
    | ">=" -> (>=)
    | "<=" -> (<=)
    | "==" -> (=)
    | "!=" -> (<>)
    | _-> failwith "Not a valid operation"

//let strToCompareop str =
//    match str with
//    | CompareOp op -> Op
//    | _ -> failwith "Is not valid operation"

type Op =
    {
        Register: Register
        OpType: OpType
        OpVal: RegisterVal
        ConditionRegister: Register
        CompareOp: Condition
        CompareVal: Register
    }

let lineToOp line =
    let reg =
        "^(\w{1,3}) (inc|dec) (-?\d+) if (\w{1,3}) (>|<|>=|<=|==|!=) (-?\d+)$"
    let mat = Regex.Match(line, reg)
    let getGr (i : int) = mat.Groups.[i].Value
    let register, opType, opVal, condReg, compareOp, compareVal =
        getGr 1, getOp (getGr 2), int (getGr 3), getGr 4, getCompareOp (getGr 5), getGr 6
    
    {
        Register = register
        OpType = opType
        OpVal = opVal
        ConditionRegister = condReg
        CompareOp = compareOp
        CompareVal = compareVal
    }


let getAllRegisters ops : Register list =
    ops
    |> List.map (fun op -> op.Register, op.ConditionRegister)
    |> List.reduce (fun a b -> [fst a; snd a; fst b; snd b])
    |> List.distinct



let main =
    let map = new Map<Register, RegisterVal>()
    let ops =
        getLines "./day8.txt"
        |> List.map lineToOp

    
