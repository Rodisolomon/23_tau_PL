structure Fresh = struct

  val c = ref 0

  fun var () =
    let
      val n = !c
      val _ = (c := n+1)      
    in
      "FRESH" ^ Int.toString n
    end

end
