(* 
asm - Copyright (c) 2020 Dariusz Mikołajczyk 
*)


exception TokenizerError of string

type token = 
  | Tok_Number of string
  | Tok_Sum
  | Tok_Mul
  | Tok_Div
  | Tok_Sub
  | Tok_Or
  | Tok_And
  | Tok_LNot
  | Tok_BNot
  | Tok_Less
  | Tok_More
  | Tok_Equ
  | Tok_Coma
  | Tok_Hash
  | Tok_LParen
  | Tok_RParen
  | Tok_Word of string   (*  abc  *)
  | Tok_Char of char
  | Tok_NewL
  | Tok_Label of string  (* abc:  *)
  | Tok_Direct of string (* .abc  *)
  | Tok_String of string (* "abc" *)
  | Tok_End


(* lexer *)
let tokenize str = 
  
  let rec f pos s = 
  (* Printf.printf "tokenize str: %d -> %s" pos s; *)
  if pos >= String.length s then [Tok_End]
  else
    if(Str.string_match (Str.regexp "\\(\\%[01]+\\)\\|\\(\\$[0-9a-fA-F]+\\)\\|\\([0-9]+\\)") s pos) then
      let token = Str.matched_string s in
      (* Printf.printf "tokenize num: %s\n" token; *)
      (Tok_Number token)::(f (pos + (String.length token)) s)
    else if (Str.string_match (Str.regexp "\\+") s pos) then begin
      (* Printf.printf "tokenize add\n"; *)
      Tok_Sum::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "\\*") s pos) then begin
      (* Printf.printf "tokenize mul\n"; *)
      Tok_Mul::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "\\/") s pos) then begin
      (* Printf.printf "tokenize div\n"; *)
      Tok_Div::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "\\-") s pos) then begin
      (* Printf.printf "tokenize min\n"; *)
      Tok_Sub::(f (pos + 1) s)
    end 
    else if (Str.string_match (Str.regexp "|") s pos) then begin
      (* Printf.printf "tokenize min\n"; *)
      Tok_Or::(f (pos + 1) s)
    end    
  else if (Str.string_match (Str.regexp "&") s pos) then begin
    (* Printf.printf "tokenize min\n"; *)
    Tok_And::(f (pos + 1) s)
  end 
  else if (Str.string_match (Str.regexp "!") s pos) then begin
    (* Printf.printf "tokenize min\n"; *)
    Tok_LNot::(f (pos + 1) s)
  end 
  else if (Str.string_match (Str.regexp "~") s pos) then begin
    (* Printf.printf "tokenize min\n"; *)
    Tok_BNot::(f (pos + 1) s)
  end    
    else if (Str.string_match (Str.regexp "<") s pos) then begin
      (* Printf.printf "tokenize min\n"; *)
      Tok_Less::(f (pos + 1) s)
    end    
    else if (Str.string_match (Str.regexp ">") s pos) then begin
      (* Printf.printf "tokenize min\n"; *)
      Tok_More::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "=") s pos) then begin
      (* Printf.printf "tokenize equ\n"; *)
      Tok_Equ::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "\\,") s pos) then begin
      (* Printf.printf "tokenize coma\n"; *)
      Tok_Coma::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "#") s pos) then begin
      (* Printf.printf "tokenize hash\n"; *)
      Tok_Hash::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "(") s pos) then begin
      (* Printf.printf "tokenize open bra\n"; *)
      Tok_LParen::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp ")") s pos) then begin
      (* Printf.printf "tokenize closed bra\n"; *)
      Tok_RParen::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "\\\'.\\\'") s pos) then begin
      let token = Str.matched_string s in
      (* Printf.printf "tokenize char: %s\n" token; *)
      (Tok_Char token.[1])::(f (pos + 3) s)
    end
    else if (Str.string_match (Str.regexp "\\\"\\([^\\\"]*\\)\\\"") s pos) then begin
      let token = Str.matched_group 1 s in
      (* Printf.printf "tokenize string: %s\n" token; *)
      (Tok_String token)::(f (pos + (String.length token + 2)) s)
    end
    else if (Str.string_match (Str.regexp "[a-zA-Z_@][a-zA-Z_@0-9]*\\:") s pos) then begin
      let token = Str.matched_string s in
      (* Printf.printf "tokenize label: %s\n" token; *)
      (Tok_Label token)::(f (pos + (String.length token)) s)
    end
    else if (Str.string_match (Str.regexp "\\.[a-zA-Z_]+") s pos) then begin
      let token = Str.matched_string s in
      (* Printf.printf "tokenize keyword: %s\n" token; *)
      (Tok_Direct token)::(f (pos + (String.length token)) s)
    end  
    else if (Str.string_match (Str.regexp "[a-zA-Z_@][a-zA-Z_@0-9]*") s pos) then begin
      let token = Str.matched_string s in
      (* Printf.printf "tokenize word: %s\n" token; *)
      (Tok_Word token)::(f (pos + (String.length token)) s)
    end
    else if (Str.string_match (Str.regexp "\\([\t ]+\\)\\|\\(\\;[^\n]*\\)") s pos) then begin
      let token = Str.matched_string s in
      (* Printf.printf "tokenize white: %s\n" token; *)
      (f (pos + (String.length token)) s)
    end
    else if (Str.string_match (Str.regexp "[\n]+") s pos) then begin
      let token = Str.matched_string s in
      (* Printf.printf "tokenize newl: %s\n" token; *)
      Tok_NewL::(f (pos + (String.length token)) s)
    end 
    else
      raise (TokenizerError (Printf.sprintf "\n\tERR00 >> Nobody expects the string: [%s] (or spanish inquisition).\n" (Str.last_chars s ((String.length s) - pos))))
  in
  (f 0 str)

  
let tokenCompare (t1: token) (t2: token) (strict: bool) : bool =

    match t1 with
    | Tok_Number n1 -> (match t2 with |Tok_Number n2 -> if(strict) then n1 = n2 else true |_ -> false)
    | Tok_Sum -> (match t2 with |Tok_Sum -> true |_ -> false)
    | Tok_Mul -> (match t2 with |Tok_Mul -> true |_ -> false)
    | Tok_Div -> (match t2 with |Tok_Div -> true |_ -> false)
    | Tok_Sub -> (match t2 with |Tok_Sub -> true |_ -> false)
    | Tok_Or -> (match t2 with |Tok_Or -> true |_ -> false)
    | Tok_And -> (match t2 with |Tok_And -> true |_ -> false)
    | Tok_BNot -> (match t2 with |Tok_BNot -> true |_ -> false)
    | Tok_LNot -> (match t2 with |Tok_LNot -> true |_ -> false)
    | Tok_Less -> (match t2 with |Tok_Less -> true |_ -> false)
    | Tok_More -> (match t2 with |Tok_More -> true |_ -> false)
    | Tok_Equ -> (match t2 with |Tok_Equ -> true |_ -> false)
    | Tok_Coma -> (match t2 with |Tok_Coma -> true |_ -> false)
    | Tok_Hash -> (match t2 with |Tok_Hash -> true |_ -> false)
    | Tok_LParen -> (match t2 with |Tok_LParen -> true |_ -> false)
    | Tok_RParen -> (match t2 with |Tok_RParen -> true |_ -> false)   
    | Tok_Word s1 -> (match t2 with |Tok_Word s2 -> 
            (* Printf.printf "compare words %s %s\n" s1 s2; *)
            if(strict) then (String.compare s1 s2)==0 else true |_ -> false)
    |Tok_Label s1 -> (match t2 with |Tok_Label s2 -> 
            (* Printf.printf "compare words %s %s\n" s1 s2; *)
            if(strict) then (String.compare s1 s2)==0 else true |_ -> false)
    | Tok_Direct s1 -> (match t2 with |Tok_Direct s2 -> 
            (* Printf.printf "compare words %s %s\n" s1 s2; *)
            if(strict) then (String.compare s1 s2)==0 else true |_ -> false)
    |Tok_String s1 -> (match t2 with |Tok_String s2 -> 
            (* Printf.printf "compare words %s %s\n" s1 s2; *)
            if(strict) then (String.compare s1 s2)==0 else true |_ -> false)
    | Tok_Char c1 -> (match t2 with |Tok_Char c2 -> 
            (* Printf.printf "compare chars %c %c\n" c1 c2; *)
            if(strict) then c1 = c2 else true |_ -> false)
    | Tok_NewL -> (match t2 with |Tok_NewL -> true |_ -> false)
    | Tok_End -> (match t2 with |Tok_End -> true |_ -> false)


let token2str (t: token) : string =

  match t with
    | Tok_Number n -> Printf.sprintf "(Number: %s)" n
    | Tok_Sum -> "(+)"
    | Tok_Mul -> "(*)"
    | Tok_Div -> "(/)"
    | Tok_Sub -> "(-)"
    | Tok_Or -> "(|)"
    | Tok_And -> "(&)"       
    | Tok_BNot -> "(~)" 
    | Tok_LNot -> "(!)"  
    | Tok_Less -> "(<)"
    | Tok_More -> "(>)"
    | Tok_Equ -> "(=)"
    | Tok_Coma -> "(,)"
    | Tok_Hash -> "(#)"
    | Tok_LParen -> "(Tok_LParen)"
    | Tok_RParen -> "(Tok_RParen)"    
    | Tok_Word s -> Printf.sprintf "(%s)" s  
    | Tok_Label s -> Printf.sprintf "(%s:)" s  
    | Tok_Direct s -> Printf.sprintf "(%s)" s  
    | Tok_String s -> Printf.sprintf "(\"%s\")" s  
    | Tok_Char c -> Printf.sprintf "(Char: %c)" c
    | Tok_NewL -> "(\\n)"
    | Tok_End -> "(Tok_End)"


let rec tokensl2str (tokens: token list) : string =

  match tokens with
  | [] -> ""
  | s::xs ->  Printf.sprintf " %s, %s" (token2str s) (tokensl2str xs)


let tokens2str (tokens: token array ref) : string =

  Array.fold_left (fun acc t -> Printf.sprintf " %s\n %s" acc (token2str t)) "" !tokens


let tokensn2str (tokens: token array ref) (subt:int * int) : string =

  let acc = ref "" in
  let beg, len = subt in
  Array.iteri (
    fun i t -> if (i >= beg && i < (beg+len)) then 
                  acc := Printf.sprintf " %s, %s" !acc (token2str t)
  ) !tokens;
  !acc


let showTokens (a : token array ref) =
  Printf.printf "Tokens: %s\n" (tokens2str a)



