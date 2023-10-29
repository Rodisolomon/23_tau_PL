structure Sugary = struct

  datatype term
    = Nat of int
    | True
    | False
    | Unit
    | Add of term * term
    | Subtract of term * term
    | Mul of term * term
    | Pow of term * term
    | Less of term * term
    | Greater of term * term
    | LessEq of term * term
    | GreaterEq of term * term
    | Not of term
    | And of term * term
    | Or of term * term
    | Xor of term * term
    | Cond of term * term * term
    | Eq of term * term
    | Pair of term * term
    | First of term
    | Second of term
    | Var of string
    | Let of string * term * term

end
