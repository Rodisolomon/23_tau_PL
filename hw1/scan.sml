structure Scan : sig

  val next : char list -> (Token.token * char list) option
  val scan : string -> Token.token list
  val checkDoubleOperator: char * char * char list -> (Token.token * char list) option
  val lp: char list -> Token.token list 
end = struct

  structure T = Token

  fun checkDoubleOperator (ch1, ch2, rest) =
      if ch1 = #"&" andalso ch2 = #"&" then
          SOME (T.DoubleAmpersand, rest)
      else if ch1 = #"|" andalso ch2 = #"|" then
          SOME (T.DoublePipe, rest)
      else
          NONE;

  fun next (lst) = 
    case lst of 
        #"Z" :: rest => SOME (T.Z, rest)
      | #"T" :: rest => SOME (T.T, rest)
      | #"F" :: rest => SOME (T.F, rest)
      | #"S" :: rest => SOME (T.S, rest)
      | #"P" :: rest => SOME (T.P, rest)
      | #"[" :: rest => SOME (T.LBrack, rest)
      | #"]" :: rest => SOME (T.RBrack, rest)
      | #"+" :: rest => SOME (T.Plus, rest)
      | #"-" :: rest => SOME (T.Minus, rest)
      | #"<" :: rest => SOME (T.LessThan, rest)
      | #">" :: rest => SOME (T.GreaterThan, rest)
      | #"?" :: rest => SOME (T.QuestionMark, rest)
      | #":" :: rest => SOME (T.Colon, rest)
      (* if double operator *)
      | #"&" :: char2 :: rest => checkDoubleOperator (#"&", char2, rest)
      | #"|" :: char2 :: rest => checkDoubleOperator (#"|", char2, rest)
        (* none *)
      | _ => NONE; 

  fun removeWhitespace(charList: char list) =
      List.filter (fn c => not (Char.isSpace c)) charList;

  fun hasWhitespaceBetweenChars (chars : char list) =
      let
          fun hasWhitespace (c : char) = Char.isSpace c
          fun checkWhitespace [] = false
            | checkWhitespace [x] = false
            | checkWhitespace (x :: y :: rest) =
              if (x = #"|" orelse x = #"&") andalso hasWhitespace y
              then true
              else checkWhitespace (y :: rest)
      in
          checkWhitespace chars
      end;

  fun lp chars = 
    case next chars of
        SOME(token, chars') => token :: lp chars'
      | NONE => [];

  fun scan str = 
    let 
      val plain_char_lst = String.explode(str)
      val wrong_and_or = hasWhitespaceBetweenChars plain_char_lst
      val char_lst = removeWhitespace plain_char_lst
      val returned_tokens = lp char_lst
      val list_length = length returned_tokens;
    in
      (* print (if wrong_and_or then "true\n" else "false\n"); *)
      if list_length = 0 then 
        raise Fail "no valid token"
      else if wrong_and_or then
        raise Fail "wrong and/or operator"
      else
        returned_tokens
    end;
  
end
