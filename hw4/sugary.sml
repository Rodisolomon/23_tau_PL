structure Sugary = struct

  datatype term
    = Nat of int (*simple*)
    | True
    | False
    | Unit
    | Add of term * term (*calculation*)
    | Subtract of term * term
    | Mul of term * term
    | Pow of term * term
    | Less of term * term (*compare*)
    | Greater of term * term
    | LessEq of term * term
    | GreaterEq of term * term
    | Not of term (*condition*)
    | And of term * term
    | Or of term * term
    | Xor of term * term
    | Cond of term * term * term
    | Eq of term * term
    | Pair of term * term (*others*)
    | First of term
    | Second of term
    | Var of string
    | Let of string * term * term

end
