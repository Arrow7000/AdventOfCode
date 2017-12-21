// http://adventofcode.com/2017/day/8

module Day8

open System.Text.RegularExpressions


type Register = string
type OpType = Inc | Dec
type OpVal = int
type Condition = Register -> Register -> bool
type CompareOp = 
    | Gt of Condition
    | Lt of Condition
    | Gte of Condition
    | Lte of Condition
    | Eq of Condition
    | Neq of Condition

let strToCompareop str =
    match str with
    | CompareOp op -> Op
    | _ -> failwith "Is not valid operation"

type Op =
    {
        Register: Register
        OpType: OpType
        OpVal: OpVal
        ConditionRegister: Register
        CompareOp: CompareOp
        CompareVal: Register
    }

let lineToOp line =
    let reg =
        "^(\w{1,3}) (inc|dec) (-?\d+) if (\w{1,3}) (>|<|>=|<=|==|!=) (-?\d+)$"
    let mat = Regex.Match(line, reg)
    let getGr (i : int) = mat.Groups.[i].Value
    let register, opType, opVal, condReg, compareOp, compareVal =
        getGr 1, getGr 2, int (getGr 3), getGr 4, getGr 5, getGr 6
    
    {
        Register = register
        OpType = opType
        OpVal = opVal
        ConditionRegister = condReg
        CompareOp = compareOp
        CompareVal = compareVal
    }