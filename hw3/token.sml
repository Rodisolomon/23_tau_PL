structure Token : sig

  datatype token
    = Var of string
    | LParen
    | RParen
    | LBrack
    | RBrack
    | Nat of int
    | AtT
    | AtF
    | ID of string
    | Abbr of string
    | Eq
    | Semicolon
	
  val tos : token -> string

end = struct

  datatype token
    = Var of string
    | LParen
    | RParen
    | LBrack
    | RBrack
    | Nat of int
    | AtT
    | AtF
    | ID of string
    | Abbr of string
    | Eq
    | Semicolon

  fun par s = "(" ^ s ^ ")"
	
  fun tos (Var x) = "Var" ^ par x
    | tos LParen = "LParen"
    | tos RParen = "RParen"
    | tos LBrack = "LBrack"
    | tos RBrack = "RBrack"
    | tos (Nat n) = "Nat" ^ par (Int.toString n)
    | tos AtT = "AtT"
    | tos AtF = "AtF"
    | tos (ID s) = "ID" ^ par s
    | tos (Abbr a) = "Abbr" ^ par a
    | tos Eq = "Eq"
    | tos Semicolon = "Semicolon"
	  
end
