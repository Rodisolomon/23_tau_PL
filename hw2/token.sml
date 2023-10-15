structure Token = struct

  datatype token
    = Nat of int
    | T
    | F
    | LBrack
    | RBrack
    | LParen
    | RParen
    | Plus
    | Minus
    | LessThan
    | LessEq
    | GreaterThan
    | GreaterEq
    | ExclamationPoint
    | DoubleAmpersand
    | DoublePipe
    | DoubleCaret
    | QuestionMark
    | Colon
    | DoubleEq
    | Comma
    | OneHash
    | TwoHash

  fun tos (Nat n) = "Nat(" ^ Int.toString n ^ ")"
    | tos T = "T"
    | tos F = "F"
    | tos LBrack = "["
    | tos RBrack = "]"
    | tos LParen = "("
    | tos RParen = ")"
    | tos Plus = "+"
    | tos Minus = "-"
    | tos LessThan = "<"
    | tos LessEq = "<="
    | tos GreaterThan = ">"
    | tos GreaterEq = ">="
    | tos ExclamationPoint = "!"
    | tos DoubleAmpersand = "&&"
    | tos DoublePipe = "||"
    | tos DoubleCaret = "^^"
    | tos QuestionMark = "?"
    | tos Colon = ":"
    | tos DoubleEq = "=="
    | tos Comma = ","
    | tos OneHash = "1#"
    | tos TwoHash = "2#"

end
