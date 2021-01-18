open Str

(* 
testing in utop:

  #load "str.cma";;
  #use "./parser.ml";;
  exp_p.run {tokens= ref (Array.of_list (tokenize "1203*2+5*10  ss")) ; pos= 0};;
  exp_p.run {tokens= ref (Array.of_list (tokenize "(2+(5*3))-8")) ; pos= 0};;
*)

exception TokenizerError of string
exception ParserError of string

type token = 
  | Tok_Num of string
  | Tok_Sum
  | Tok_Mul
  | Tok_Div
  | Tok_Sub
  | Tok_Equ
  | Tok_Coma
  | Tok_Hash
  | Tok_OBra
  | Tok_CBra
  | Tok_Word of string
  | Tok_Char of char
  | Tok_NewL
  | Tok_Label of string
  | Tok_KeyW of string
  | Tok_String of string
  | Tok_End


(* lexer *)
let tokenize str = 
  
  let rec f pos s = 
  Printf.printf "tokenize str: %d -> %s\n" pos s;
  if pos >= String.length s then [Tok_End]
  else
    if(Str.string_match (Str.regexp "\\(\\%[01]+\\)\\|\\(\\$[0-9a-fA-F]+\\)\\|\\([0-9]+\\)") s pos) then
      let token = Str.matched_string s in
      Printf.printf "tokenize num: %s\n" token;
      (Tok_Num token)::(f (pos + (String.length token)) s)
    else if (Str.string_match (Str.regexp "\\+") s pos) then begin
      Printf.printf "tokenize add\n";
      Tok_Sum::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "\\*") s pos) then begin
      Printf.printf "tokenize mul\n";
      Tok_Mul::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "\\/") s pos) then begin
      Printf.printf "tokenize div\n";
      Tok_Div::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "\\-") s pos) then begin
      Printf.printf "tokenize min\n";
      Tok_Sub::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "=") s pos) then begin
      Printf.printf "tokenize equ\n";
      Tok_Equ::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "\\,") s pos) then begin
      Printf.printf "tokenize equ\n";
      Tok_Coma::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "#") s pos) then begin
      Printf.printf "tokenize equ\n";
      Tok_Hash::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "(") s pos) then begin
      Printf.printf "tokenize open bra\n";
      Tok_OBra::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp ")") s pos) then begin
      Printf.printf "tokenize closed bra\n";
      Tok_CBra::(f (pos + 1) s)
    end
    else if (Str.string_match (Str.regexp "\\\'.\\\'") s pos) then begin
      let token = Str.matched_string s in
      Printf.printf "tokenize char: %s\n" token;
      (Tok_Char token.[1])::(f (pos + 3) s)
    end
    else if (Str.string_match (Str.regexp "\\\"\\([^\\\"]*\\)\\\"") s pos) then begin
      let token = Str.matched_group 1 s in
      Printf.printf "tokenize string: %s\n" token;
      (Tok_String token)::(f (pos + (String.length token + 2)) s)
    end
    else if (Str.string_match (Str.regexp "[a-zA-Z_@][a-zA-Z_@0-9]*\\:") s pos) then begin
      let token = Str.matched_string s in
      Printf.printf "tokenize label: %s\n" token;
      (Tok_Label token)::(f (pos + (String.length token)) s)
    end
    else if (Str.string_match (Str.regexp "\\.[a-zA-Z_]+") s pos) then begin
      let token = Str.matched_string s in
      Printf.printf "tokenize keyword: %s\n" token;
      (Tok_KeyW token)::(f (pos + (String.length token)) s)
    end  
    else if (Str.string_match (Str.regexp "[a-zA-Z_@][a-zA-Z_@0-9]*") s pos) then begin
      let token = Str.matched_string s in
      Printf.printf "tokenize word: %s\n" token;
      (Tok_Word token)::(f (pos + (String.length token)) s)
    end
    else if (Str.string_match (Str.regexp "\\([\t ]+\\)\\|\\(\\;[^\n]*\\)") s pos) then begin
      let token = Str.matched_string s in
      Printf.printf "tokenize white: %s\n" token;
      (f (pos + (String.length token)) s)
    end
    else if (Str.string_match (Str.regexp "[\n]+") s pos) then begin
      let token = Str.matched_string s in
      Printf.printf "tokenize newl: %s\n" token;
      Tok_NewL::(f (pos + (String.length token)) s)
    end 
    else
      raise (TokenizerError (Printf.sprintf "\n\tERR00 >> Nobody expects the string: [%s] (or spanish inquisition).\n" (Str.last_chars s ((String.length s) - pos))))
  in
  (f 0 str)

  
let tokenCompare (t1: token) (t2: token) (strict: bool) : bool =

    match t1 with
    | Tok_Num n1 -> (match t2 with |Tok_Num n2 -> if(strict) then n1 = n2 else true |_ -> false)
    | Tok_Sum -> (match t2 with |Tok_Sum -> true |_ -> false)
    | Tok_Mul -> (match t2 with |Tok_Mul -> true |_ -> false)
    | Tok_Div -> (match t2 with |Tok_Div -> true |_ -> false)
    | Tok_Sub -> (match t2 with |Tok_Sub -> true |_ -> false)
    | Tok_Equ -> (match t2 with |Tok_Equ -> true |_ -> false)
    | Tok_Coma -> (match t2 with |Tok_Coma -> true |_ -> false)
    | Tok_Hash -> (match t2 with |Tok_Hash -> true |_ -> false)
    | Tok_OBra -> (match t2 with |Tok_OBra -> true |_ -> false)
    | Tok_CBra -> (match t2 with |Tok_CBra -> true |_ -> false)   
    | Tok_Word s1 -> (match t2 with |Tok_Word s2 -> 
            (* Printf.printf "compare words %s %s\n" s1 s2; *)
            if(strict) then (String.compare s1 s2)==0 else true |_ -> false)
    |Tok_Label s1 -> (match t2 with |Tok_Label s2 -> 
            (* Printf.printf "compare words %s %s\n" s1 s2; *)
            if(strict) then (String.compare s1 s2)==0 else true |_ -> false)
    | Tok_KeyW s1 -> (match t2 with |Tok_KeyW s2 -> 
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

let rec token2str (t: token) : string =

  match t with
    | Tok_Num n -> Printf.sprintf "(Number: %s)" n
    | Tok_Sum -> "(+)"
    | Tok_Mul -> "(*)"
    | Tok_Div -> "(/)"
    | Tok_Sub -> "(-)"
    | Tok_Equ -> "(=)"
    | Tok_Coma -> "(,)"
    | Tok_Hash -> "(#)"
    | Tok_OBra -> "(Tok_OBra)"
    | Tok_CBra -> "(Tok_CBra)"    
    | Tok_Word s -> Printf.sprintf "(%s)" s  
    | Tok_Label s -> Printf.sprintf "(%s:)" s  
    | Tok_KeyW s -> Printf.sprintf "(.%s)" s  
    | Tok_String s -> Printf.sprintf "(\"%s\")" s  
    | Tok_Char c -> Printf.sprintf "(%c)" c
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





(* 
let tok_list = ref [];;

let setTokens t = 
  tok_list := t *)

let showTokens (a : token array ref) =
  Printf.printf "Tokens: %s\n" (tokens2str a)

(* get the head of the token list *)
(* let currToken () = 
  match !tok_list with
      |[] -> raise (ParserError "\n\tERR01 >> \"missing token!\n")
      |h::t -> 
        Printf.printf "currToken %s\n" (token2str h);
        h

(* consumes one token *)
let nextToken a =
      match !tok_list with
      |(x::y::xs) when a = x -> 
                Printf.printf "\nnextToken:%s\n" (token2str y);
                tok_list := y::xs 

      |(x::xs) -> 
                raise (ParserError (Printf.sprintf "\n\tERR02 >> Found token: %s instead of: %s\n"(token2str x) (token2str a)))
      | _ -> 
                raise (ParserError (Printf.sprintf "\n\tERR03 >> Missing token: %s\n" (token2str a)))
 *)




(* parser *)

type input =
  { tokens: token array ref;
    pos: int;
  }

(* let input_sub (start: int) (len: int) (s: input): input =
  { text = String.sub (s.text) start len;
    pos = s.pos + start;
  } *)

let make_input (s: token array ref): input =
  { tokens = s; pos = 0 }

type error = { 

    desc: string;
    pos: int
  }

type 'a parser = {

    run : input -> input * ('a, string) result
  }

let fail (e: string) = { run = fun input -> input, Error e }

let return (x: 'a) = { run = fun input -> input, Ok x}


let map (f: 'a -> 'b) (p: 'a parser): 'b parser =
  { run = fun input ->
          match p.run input with
          | input', Ok x        -> input', Ok (f x)
          | _, Error error -> input, Error error (* if fail, return old input pos *)
  }

let bind (f: 'a -> 'b parser) (p: 'a parser): 'b parser =
  { run = fun input ->
          match p.run input with
          | input', Ok x -> (f x).run input'
          | _, Error error -> input, Error error (* if fail, return old input pos *)
  }

let ( >>= )  (p: 'a parser) (f: 'a -> 'b parser)  : 'b parser  = { 
  
  run = fun input -> 
      (* Printf.printf "begin >>= :[%s] at:[%d]\n" (!(input.tokens).( input.pos ) |> token2str) input.pos; *)
      let input', result = p.run input in
      match result with
          | Ok x -> 
                (* Printf.printf "end >>= 1:[%s] at:[%d]\n" (!(input'.tokens).( input'.pos ) |> token2str) input'.pos; *)
                let input'', result' = (f x).run input' in
                (* Printf.printf "end >>= 2:[%s] at:[%d]\n" (!(input''.tokens).( input''.pos ) |> token2str) input''.pos; *)
                (input'', result')
          | Error error -> 
                (* Printf.printf "end (err) >>= :[%s] at:[%d]\n" (!(input.tokens).( input.pos ) |> token2str) input.pos; *)
                input, Error error (* if fail, return old input pos *)
}

let parse_while (p: token -> bool): (int*int) parser =
  { run = fun input ->
          let n = Array.length !(input.tokens) in
          let i = ref 0 in
          while !i < n && !(input.tokens).( !i ) |> p do
            incr i
          done;
          {tokens = input.tokens; pos = input.pos + !i}, Ok (input.pos, !i)
  }


let is_same input (tokens: token list)  =
  let ti = ref 0 in
  (List.fold_left (
    fun acc t ->  (* Printf.printf "prefix compare %s %s\n" (token2str !(input.tokens).(input.pos + !ti)) (token2str t);     *)  
      if (tokenCompare !(input.tokens).( input.pos + !ti ) t false) then (
        ti := !ti + 1;
        (acc && true)
      )
      else false
    ) true tokens
  , !ti)

let is_exact input (tokens: token list)  =
  let ti = ref 0 in
  (List.fold_left (
    fun acc t ->  (* Printf.printf "prefix compare %s %s\n" (token2str !(input.tokens).(input.pos + !ti)) (token2str t);     *)                    
      if (tokenCompare !(input.tokens).( input.pos + !ti ) t true) then (
        ti := !ti + 1;
        (acc && true)
      )
      else false
    ) true tokens
  , !ti)

let prefix (tokens: token list): (int*int) parser = { 
  
  run = fun input ->
    let unexpected_prefix_error = Printf.sprintf "expected `%s`" (tokensl2str tokens)
    in
    try
      match is_exact input tokens with
      | true, ti -> {tokens = input.tokens; pos = input.pos + ti}, Ok (input.pos, ti)
      | _ ->  input, Error unexpected_prefix_error
    with
      Invalid_argument _ -> input, Error unexpected_prefix_error
}


(*
let ( *> ) (p1: 'a parser) (p2: 'b parser): 'b parser =
  { run = fun input ->
          let input', result = p1.run input in
          match result  with
          | Ok _  -> p2.run input'
          | Error e -> input', Error e
  }
 
let ( <* ) (p1: 'a parser) (p2: 'b parser): 'a parser =
  { run = fun input ->
          let input', result = p1.run input in
          match result with
          | Ok x ->
             let input'', result' = p2.run input' in
             (match result' with
              | Ok _  -> input'', Ok x
              | Error e -> input'', Error e)
          | Error e -> input', Error e
  }

let ( <*> ) (p1: ('a * 'b) parser) (p2: ('c * 'd) parser): ('a * 'd) parser =
  { run = fun input ->
          let input', result = p1.run input in
          match result with
          | Ok (x1, x2) ->
             let input'', result' = p2.run input' in
             (match result' with
              | Ok (y1, y2)  -> input'', Ok (x1, x2+y2)
              | Error e -> input'', Error e)
          | Error e -> input', Error e
  }

*)
let ( <|> ) (p1: 'a parser) (p2: 'a parser): 'a parser = { 
  
  run = fun input -> 
    let input', result = input |> p1.run in
    match result with
    | Ok _ -> (input', result)
    | Error _ -> input |> p2.run  
}

let ( *> ) (p1: 'a parser) (p2: 'b parser): 'b parser = { run = fun input -> 
  input |> (p1 >>= (fun _ -> p2 >>= (fun b -> return b))).run }
let ( <* ) (p1: 'a parser) (p2: 'b parser): 'a parser = { run = fun input -> 
  input |> (p1 >>= (fun a -> p2 >>= (fun _ -> return a))).run }
let ( <*> ) (p1: 'a parser) (p2: 'b parser): ('a * 'b) parser  = { run = fun input -> 
  input |> (p1 >>= (fun a -> p2 >>= (fun b -> return (a,b)))).run }



let optional (p: 'a parser): 'a option parser =
  { run = fun input ->
          let input', result = p.run input in
          match result with
          | Ok x    -> input', Ok (Some x)
          | Error _ -> input, Ok None      (* if fail, return old input pos *)
  }

(* TODO: change to monad ver *)
let many_exact (n: int) (p: 'a parser): 'a list parser = { 
  run = fun input ->

          let rec loop i xs input' =
            if i < n then
              let input'', result = p.run input' in
              match result with
              | Ok x    -> loop (i + 1) (x :: xs) input''
              | Error e -> input', Error e (* if fail, return old input pos *)
            else
              input', Ok (List.rev xs)
          in loop 0 [] input
  }

(* TODO: change to monad ver *)
let many (p: 'a parser): 'a list parser = { 
  run = fun input ->

          let xs = ref [] in
          let rec loop input =
            let input', result = p.run input in
            match result with
            | Ok x ->
               xs := x :: !xs;
               loop input'
            | Error _ ->
               input
          in
          let input' = loop input in
          input', Ok (!xs |> List.rev)
  }

(* TODO: change to monad ver *)
let zeroOrMore (p:'a parser) : 'a list parser = {
  run = fun input -> 

    let rec parse_fun = fun i -> 
      let input', result = p.run i in
      match result with
      | Error _ -> i, Ok []       
      | Ok x -> let input'', result' = parse_fun input' in
            (match result' with
                      | (Error _) -> (input', Ok (x::[]))       
                      | ( Ok x'')-> (input'', Ok ( x::x'')))
    in
    parse_fun input
}  

(* TODO: change to monad ver *)
let oneOrMore (p:'a parser) : 'a list parser = {
  run = fun input -> 

    let rec parse_fun = fun i -> 
      let input', result = p.run i in
      match result with
      | Error error -> i, Error error       
      | Ok x -> let input'', result' = parse_fun input' in
            (match result' with
                      | (Error _) -> (input', Ok (x::[]))       
                      | ( Ok x'')-> (input'', Ok ( x::x'')))
    in
    parse_fun input
}   

(* TODO: change to monad ver *)
let is_a (t: token) : bool parser = {
  run = fun input -> 

    if(tokenCompare !(input.tokens).( input.pos ) t false) then 
        {tokens = input.tokens; pos= input.pos + 1}, Ok true
    else
         input, Error ( Printf.sprintf ": Not a %s at: [%d] " (t |> token2str) input.pos)
}

(* 
  parser stosowany w monadach >>= gdzie nie ma bezpośredniego dostępu do input.pos
  Cofanie o jedną pozycję.
*)
let get_back  : int parser = {
  run = fun input -> 

    if(input.pos > 0) then 
        {tokens = input.tokens; pos = input.pos - 1}, Ok (input.pos -1)
    else
        input, Error ( Printf.sprintf ": try get_back to %s at: [%d] " (!(input.tokens).( input.pos ) |> token2str) input.pos)
}

let getTok : token parser = {
  run = fun input -> 
    Printf.printf "getTok t:[%s] at:[%d]\n" (!(input.tokens).( input.pos ) |> token2str) input.pos;
    if (!(input.tokens) |> Array.length > input.pos) then 
        {tokens = input.tokens; pos = input.pos + 1}, Ok !(input.tokens).( input.pos )
    else
         input, Error "Parser error: Unexpected end of string!"
}

(* example  *)
type exp = 
  | Num of int
  | Min of exp
  | Sub of exp * exp
  | Sum of exp * exp
  | Mul of exp * exp
  | Div of exp * exp

let rec eval exp : int = 
  match exp with
  | Num n -> n
  | Mul (e1, e2) -> eval e1 * eval e2
  | Sum (e1, e2) -> eval e1 + eval e2
  | Div (e1, e2) -> eval e1 / eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Min e1 -> (-1) * (eval e1)  
;;

(* 
  E -> T + E | T - E | T    expression
  T -> F * T | F / T | F    term 
  F -> 0,1,2,3..| (E) | -F  factor
*)  


(* TODO: change to monad ver *)
let number_p: int parser = {
  run = fun input -> 
   
    match !(input.tokens).( input.pos ) with
    | Tok_Num n_str  -> 
        (* Printf.printf "number_p:[%s]" n_str; *)
        let str_len = String.length n_str in
        if(str_len > 0) then (
          match n_str.[0] with
          | '$' -> {tokens = input.tokens; pos = input.pos + 1}, Ok (String.sub n_str 1 (str_len-1) |> Printf.sprintf "0x%s" |> int_of_string)
          | '%' -> {tokens = input.tokens; pos = input.pos + 1}, Ok (String.sub n_str 1 (str_len-1) |> Printf.sprintf "0b%s" |> int_of_string)
          | _ -> {tokens = input.tokens; pos = input.pos + 1}, Ok (int_of_string n_str)
        )
        else input, Error ( Printf.sprintf ": Not a number: %s at: [%d] " n_str input.pos)
    | token -> 
     
      input, Error ( Printf.sprintf ": Number expected but got: %s at: [%d] " (token2str token) input.pos)
}




let rec term_p = 
{ run = fun input -> 
  Printf.printf "BEGIN term_p num:[%s] at:[%d]\n" (!(input.tokens).( input.pos ) |> token2str) input.pos;
  input |> (
      factor_p >>= fun exp1 -> 
      getTok >>= fun token ->
        Printf.printf "term_p/factor_p/getTok:[%s]\n" (token |> token2str);
        match(token) with
            | Tok_Mul -> term_p >>= fun exp2 -> return (Mul (exp1, exp2))
            | Tok_Div -> term_p >>= fun exp2 -> return (Div (exp1, exp2))
            | t ->
              Printf.printf "END term_p/factor_p/getTok/return at:[%s]\n" (t |> token2str);
              get_back >>= fun _ ->  return exp1).run  (*** get_back -> odwijamy o jeden token !! *)
}
and factor_p: exp parser = {
  run = fun input -> 
    input |> (
      (number_p >>= fun res -> return (Num res)) <|> 
      ((is_a Tok_OBra) *> exp_p <* (is_a Tok_CBra)) <|>
      ((is_a Tok_Sub) *> (number_p >>= fun res -> return (Num (-res))))
    ).run
}

      


and exp_p : exp parser  =
{ run = fun input -> 
   Printf.printf "BEGIN exp_p num:[%s] at:[%d]\n" (!(input.tokens).( input.pos ) |> token2str) input.pos;
  input |> (
      term_p >>= fun exp1 -> 
      getTok >>= fun token ->
        Printf.printf "exp_p/term_p/getTok:[%s]\n" (token |> token2str);
        match(token) with
            | Tok_Sum -> exp_p >>= fun exp2 -> return (Sum (exp1, exp2))
            | Tok_Sub -> exp_p >>= fun exp2 -> return (Sub (exp1, exp2))
            | t -> 
              Printf.printf "END exp_p/term_p/getTok/return at:[%s]\n" (t |> token2str);
              get_back >>= fun _ ->  return exp1).run   (*** get_back -> odwijamy o jeden token !! *)
}


(* let pair: pair_t parser =
  let name = parse_while (fun x -> not (is_space x) && x != '=') in
  (wss *> name <* wss <* prefix "=" <* wss) <*> (name <* wss)
 *)




(* let any_token: token parser =
  { run = fun input ->
          let n = String.length input.tokens in
          try
            input_sub 1 (n - 1) input, Ok (String.get input.tokens 0)
          with
            Invalid_argument _ -> input, Error "expected any token"
  } *)

let run (p: 'a parser) (t: token array ref): ('a, error) result =
  match {tokens = t; pos = 0} |> p.run with
  | _     , Ok x    -> Ok x
  | input', Error desc -> Error {pos = input'.pos; desc = desc; }




(*

(* number parser to expression parser wrappers *)
let to_exp_p num_p = num_p >>= fun res -> return (Num res)
let to_negexp_p num_p = num_p >>= fun res -> return (Num (-res))


 let  term_p: exp parser = { (* 2 *)




  run = 
    let rec term_p input = 
      Printf.printf "term_p:[%s] at:[%d]\n" (!(input.tokens).( input.pos ) |> token2str) input.pos;
      let inp, result = input |> factor_p.run in
      Printf.printf "term_p/factor_p:[%s] at:[%d]\n" (!(inp.tokens).( inp.pos ) |> token2str) inp.pos;
      match result with
      | Error error -> inp, Error error       
      | Ok num -> 
            Printf.printf "term_p/factor_p/Ok num:[%s] at:[%d]\n" (!(inp.tokens).( inp.pos ) |> token2str) inp.pos;
            let inp', result' = inp |> getTok.run in
            (match result' with
            (* | Error error -> inp', Error error    *)
            | Ok Tok_Mul -> 
                  Printf.printf "term_p/factor_p/Ok num/tok_mul:[%s] at:[%d]\n" (!(inp'.tokens).( inp'.pos ) |> token2str) inp'.pos;
                  let inp'', result'' = inp' |> term_p in
                   Printf.printf "term_p/factor_p/Ok num/tok_mul/term_p:[%s] at:[%d]\n" (!(inp''.tokens).( inp''.pos ) |> token2str) inp''.pos;
                  (match result'' with
                  | Error error -> inp'', Error error
                  | Ok t -> inp'', Ok (Mul (num, t)))
            | Ok Tok_Div -> 
                   let inp'', result'' = inp' |> term_p in
                  (match result'' with
                  | Error error -> inp'', Error error
                  | Ok t ->inp'', Ok (Div (num, t)))
            (* | Error error -> inp', Error error
            | _ -> inp', (Error "e?") *)
            | _ -> 
              Printf.printf "alt: term_p/factor_p/Ok num:[%s] at:[%d]\n" (!(inp.tokens).( inp.pos ) |> token2str) inp.pos;
              inp, (Ok num) (*just use result from factor_p*)
            )
      in
    term_p
}    

let exp_p: exp parser = { (* 1 *)

  run = 
    let rec exp_p input = 
      Printf.printf "exp_p:[%s] at:[%d]\n" (!(input.tokens).( input.pos ) |> token2str) input.pos;
      let inp, result = input |> term_p.run in
      Printf.printf "exp_p/term_p:[%s] at:[%d]\n" (!(inp.tokens).( inp.pos ) |> token2str) inp.pos;
      match result with
      | Error error -> inp, Error error       
      | Ok num -> 
            Printf.printf "exp_p/term_p Ok num:[%s] at:[%d]\n" (!(inp.tokens).( inp.pos ) |> token2str) inp.pos;
            let inp', result' = inp |> getTok.run in
            Printf.printf "exp_p/term_p Ok num/getTok:[%s] at:[%d]\n" (!(inp'.tokens).( inp'.pos ) |> token2str) inp'.pos;
            (match result' with
            | Ok Tok_Sum -> 
                  Printf.printf "exp_p/term_p/tok_sum:[%s] at:[%d]\n" (!(inp'.tokens).( inp'.pos ) |> token2str) inp'.pos;
                  let inp'', result'' = inp' |> exp_p in
                  (match result'' with
                  | Error error -> inp'', Error error
                  | Ok t -> inp'', Ok (Sum (num, t)))
            | Ok Tok_Sub -> 
                   let inp'', result'' = inp' |> exp_p in
                  (match result'' with
                  | Error error -> inp'', Error error
                  | Ok t -> inp'', Ok (Sub (num, t)))
            | _ -> 
              Printf.printf "alt: exp_p/term_p/Ok num:[%s] at:[%d]\n" (!(inp.tokens).( inp.pos ) |> token2str) inp.pos;
              (inp, (Ok num)) (* just use result of term_p*)
            )
      in
    exp_p
}   *)

(* 
let rec parse_E () =
  Printf.printf "parse_E %s\n" (tokens2str !tok_list);
  let a1 = parse_T () in
  match currToken () with
    | Tok_Sum -> (nextToken Tok_Sum);
              let a2 = parse_E () in
              Sum(a1, a2)
    | Tok_Sub -> (nextToken Tok_Sub);
              let a2 = parse_E () in
              Sum(a1, a2)
    | _ -> a1

and parse_T () = 
  Printf.printf "parse_T %s\n" (tokens2str !tok_list);
  let a1 = parse_P () in
  match currToken () with
    | Tok_Mul -> (nextToken Tok_Mul);
              let a2 = parse_T () in
              Mul(a1, a2)
    | Tok_Div -> (nextToken Tok_Div);
              let a2 = parse_T () in
              Div(a1, a2)
    | _ -> a1

and parse_P () =
  Printf.printf "parse_P %s\n" (tokens2str !tok_list);
  match currToken () with
    | Tok_Num n ->
              nextToken (Tok_Num n);
              let a1 = parse_E () in
              Num (int_of_string  n)

    | Tok_Sub -> 
              nextToken Tok_Sub;
              let a1 = parse_P () in
              (Min a1) 


              (* begin match currToken () with
                | Tok_Num n -> 
                          nextToken (Tok_Num n);
                          Min (int_of_string  n)
                | _ -> 
                    raise (ParserError (Printf.sprintf "\n\tERR04 >> Expected Num token instead of%s\n" (tokens2str !tok_list)))
              end *)

    | _ -> raise (ParserError (Printf.sprintf "\n\tERR05 >> Expected Num token instead of%s\n" (tokens2str !tok_list)))


 *)
