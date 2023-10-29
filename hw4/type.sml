structure Type = struct

  datatype typ
    = Nat
    | Bool
    | Unit
    | Product of typ * typ

  fun tos Nat = "Nat"
    | tos Bool = "Bool"
    | tos Unit = "Unit"
    | tos (Product (tau1, tau2)) = "(" ^ tos tau1 ^ " * " ^ tos tau2 ^ ")"
			 
end
