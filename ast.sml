structure AST = struct

  datatype term
    = Zero
    | True
    | False
    | Succ of term
    | Pred of term
    | Add of term * term
    | Subtract of term * term
    | Less of term * term
    | Greater of term * term
    | And of term * term
    | Or of term * term
    | Cond of term * term * term

end
