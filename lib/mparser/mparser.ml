
(* 
asm - Copyright (c) 2020 Dariusz Mikołajczyk 
*)

open Printf
open Tokenizer

exception ParserError of string

type identifier_table_t = {

  name: string;
  value: int;
  (* byte_counter: int; <<- for labels this is just value*)
}


(* type ident_v_ref = {

  name: string;
  v_ref: Int32 ref;
} *)

type state = { 
  
    tokens: token array ref;
    token_ix: int;
    byte_counter: int;
    identifiers: identifier_table_t list;
    labels : identifier_table_t list;
  }

  let get_identifier li s  =
  let label_str = sprintf "%s:" s in
  printf "get_identifier searching for: %s, list_size: %d\n" s (List.length li);
  match (List.filter (fun l -> printf "curr identifier: %s\n" l.name; ((String.compare l.name label_str) = 0))) li with
  | x::_ -> printf " >>>success\n";Some x.value
  | _ ->  printf " >>>fail\n";None 


  

let set_state (t, i, c, r, v) = {tokens = t; token_ix = i; byte_counter = c; identifiers = r; labels = v;}

(* increment token_ix *)
let state_next_token s = {s with token_ix = s.token_ix + 1}

(* increment token_ix and update byte counter *)
let state_next_token_w s w = {s with token_ix = s.token_ix + 1; byte_counter = s.byte_counter + w; }


type error = { 

    desc: string;
    token_ix: int
  }

type 'a parser = {

    run : state -> state * ('a, string) result
  }

let fail (e: string) = { 
  run = fun state -> 

    state, Error (sprintf ": %s: %s at: [%d] " e (token2str !( state.tokens ).( state.token_ix )) state.token_ix) 
}


let return (x: 'a) = { run = fun state -> state, Ok x}

let get_state p  = { run = fun state ->  

    printf "get_state token:%s byte:%d\n" (
      if state.token_ix < (Array.length !( state.tokens )) 
      then  (token2str !( state.tokens ).( state.token_ix )) 
      else ">>end<<" ) state.byte_counter;
    match (state |> p.run) with
    | (state', Ok res) -> state', Ok (res, state')
    | (_, Error e) -> state, Error e
}


let get_label ll s  =
      let label_str = sprintf "%s:" s in
      printf "get_label searching for: %s, list_size: %d\n" s (List.length ll);
      match (List.filter (fun l ->  ((String.compare l.name label_str) = 0))) ll with
      | x::_ -> printf " >>>success\n";Some x.value
      | _ ->  printf " >>>fail\n";None 

(* return value of label or -1 *)
let get_label_val w : (int * string * (int option)) parser = {

  run = fun state -> state, Ok (state.byte_counter, w, (get_label state.labels w))
}

let get_label_fun : (string -> int option) parser = {

  run = fun state -> state, Ok (get_label state.labels)
}
let get_labels : (identifier_table_t list) parser = {

  run = fun state -> state, Ok state.labels
}


let map (f: 'a -> 'b) (p: 'a parser): 'b parser =
  { run = fun state ->
          match p.run state with
          | state', Ok x -> state', Ok (f x)
          | _, Error error -> state, Error error (* if fail, return old state token_ix *)
  }


let ( >>= )  (p: 'a parser) (f: 'a -> 'b parser)  : 'b parser  = { 
  
  run = fun state -> 
      let state', result = p.run state in
      match result with
      | Ok x -> (f x).run state'
      | Error error -> state', Error error (* if fail, return old state token_ix *)
}


let bind p f = p >>= f


let ( <|> ) (p1: 'a parser) (p2: 'a parser): 'a parser = { 
  
  run = fun state -> 
    match state |> p1.run with 
    | _, Error e1 -> (match state |> p2.run with

                      | _, Error e2 -> (state, Error (sprintf "%s or \n\t%s" e1 e2))
                      | res2 -> res2
    )
    | res1 -> res1
}

let ( *> ) (p1: 'a parser) (p2: 'b parser): 'b parser = { run = fun state -> 
  state |> (p1 >>= (fun _ -> p2 >>= (fun b -> return b))).run } (* this last run is run from bind operator >>= *)
let ( <* ) (p1: 'a parser) (p2: 'b parser): 'a parser = { run = fun state -> 
  state |> (p1 >>= (fun a -> p2 >>= (fun _ -> return a))).run }
let ( <*> ) (p1: 'a parser) (p2: 'b parser): ('a * 'b) parser  = { run = fun state -> 
  state |> (p1 >>= (fun a -> p2 >>= (fun b -> return (a,b)))).run }




let zeroOrMore (p:'a parser) : 'a list parser = {
  run = fun state -> 

    let rec parse_fun = fun i -> 
      let state', result = p.run i in
      match result with
      | Error _ -> i, Ok []       
      | Ok x -> let state'', result' = parse_fun state' in
          (match result' with
          | (Error _) -> (state', Ok (x::[]))       
          | ( Ok x'') -> (state'', Ok ( x::x'')))
    in
    parse_fun state
}  

let oneOrMore (p:'a parser) : 'a list parser = {
  run = fun state -> 
   
    let rec parse_fun = fun (curr_state: state) -> 
      assert (curr_state.token_ix < (Array.length !( curr_state.tokens )));
      let state', result = p.run curr_state in
      match result with
      | Error s ->  curr_state, Error s       
      | Ok x -> let state'', result' = parse_fun state' in
          (match result' with
          | (Error s) -> printf "******** Error: %s\n" s;
            (state', Ok (x::[]))       
          | ( Ok x'') -> (state'', Ok ( x::x'')))
    in
    parse_fun state
}   


let is_a (t: token) : bool parser = {
  run = fun state -> 

    assert (state.token_ix < (Array.length !( state.tokens )));
    printf "is_a %s ? %s\n"  (token2str t) (token2str !( state.tokens ).( state.token_ix ));
    if (tokenCompare !( state.tokens ).( state.token_ix ) t true)  then  (

      (state_next_token state), Ok true
    )
    else 
      state, Error ( sprintf ": Not a %s at: %d [%s] " (t |> token2str) state.token_ix  (token2str !( state.tokens ).( state.token_ix )))

}

(* return expected string  *)
let word_t (s: string) : string parser = {
  run = fun state -> 
    printf "word_t %s ? %s \n" s (if state.token_ix < (Array.length !( state.tokens )) then  (token2str !( state.tokens ).( state.token_ix )) else ">>end<<");
    match !( state.tokens ).( state.token_ix )  with
    | Tok_Word s2 -> if((String.compare s s2) = 0) then (state_next_token state), Ok s
                     else state, Error ( sprintf ": Not a word %s at: [%d] " s state.token_ix)
    | t ->  state, Error ( sprintf ": Token %s is not a %s at: [%d] " (t |> token2str) s state.token_ix)
}

let word_c(s: string) : string parser = {
  run = fun state -> 
    printf "word_c %s ? %s \n" s (if state.token_ix < (Array.length !( state.tokens )) then  (token2str !( state.tokens ).( state.token_ix )) else ">>end<<");
    match !( state.tokens ).( state.token_ix )  with
    | Tok_Word s2 -> if((String.compare (String.uppercase_ascii s) (String.uppercase_ascii s2)) = 0) then (state_next_token state), Ok s
                     else state, Error ( sprintf ": Not a word %s at: [%d] " s state.token_ix)
    | t ->  state, Error ( sprintf ": Token %s is not a %s at: [%d] " (t |> token2str) s state.token_ix)
}

(* return any string *)
let word_p: string parser = {
  run = fun state -> 
    printf "word_p %s ? " (if state.token_ix < (Array.length !( state.tokens )) then  (token2str !( state.tokens ).( state.token_ix )) else ">>end<<");
    match !( state.tokens ).( state.token_ix ) with
    | Tok_Word l_str -> printf "yes\n"; (state_next_token state), Ok l_str
    | token -> 
      printf "no\n";
      state, Error ( sprintf ": Word expected but got: %s at: [%d] " (token2str token) state.token_ix)
}

(* 
data string parser - parse string in double quotes, increment byte_counter by string length.
*)
let data_string_p: string parser = {
  run = fun state -> 
    match !( state.tokens ).( state.token_ix ) with
    | Tok_String l_str  -> state_next_token_w state (String.length l_str), Ok l_str
    | token -> state, Error ( sprintf ": string in double quotes \" expected but got: %s at: [%d] " (token2str token) state.token_ix)
}


(* 
data number parser - parse number literal, increment byte_counter by number "width".
*)
let data_number_p (width : int): int parser = { 
  run = fun state -> 
     match !( state.tokens ).( state.token_ix ) with
    | Tok_Number n_str  -> 
        let str_len = String.length n_str in
        if(str_len > 0) then (
          let no = 
            match n_str.[0] with
            | '$' -> (state_next_token_w state width), (String.sub n_str 1 (str_len-1) |> sprintf "0x%s" |> Stdlib.int_of_string_opt) 
            | '%' -> (state_next_token_w state width),(String.sub n_str 1 (str_len-1) |> sprintf "0b%s" |> Stdlib.int_of_string_opt)
            | _ -> (state_next_token_w state width), (Stdlib.int_of_string_opt n_str) 
          in
          match no with
          | (stat', Some n) -> stat', Ok n
          | (_, None) -> state |> (fail "Number expeced but got").run 
        )
        else state |> (fail (sprintf "Not a number: %s at:" n_str)).run 
    | _ -> state |> (fail "Number expeced but got").run 
}




(* 
  Decrement token index in token array by one -
  decrementing state.token_ix is only way for some rec parsers this to roll back.
*)
let get_back  : int parser = {
  run = fun state -> 
    printf "get_back token:%s byte:%d\n" (if state.token_ix < (Array.length !( state.tokens )) then  (token2str !( state.tokens ).( state.token_ix )) else ">>end<<") state.byte_counter;
    if(state.token_ix > 0) then 
        {state with token_ix = state.token_ix - 1}, Ok (state.token_ix -1)
    else
        state, Error ( sprintf ": try get_back to %s at: [%d] " (!( state.tokens ).( state.token_ix ) |> token2str) state.token_ix)
}

(* increment byte_counter by i *)
let inc_bcount_p (i:int) : int parser = {
  run = fun state -> 
    printf "inc_bcount_p token:%s byte:%d + %d\n" (if state.token_ix < (Array.length !( state.tokens )) then  (token2str !( state.tokens ).( state.token_ix )) else ">>end<<") state.byte_counter i;
    {state with byte_counter = state.byte_counter + i}, Ok (state.byte_counter + i)
}


let getTok : token parser = {
  run = fun state -> 
    printf "getTok t:[%s] at:[%d]\n" (!( state.tokens ).( state.token_ix ) |> token2str) state.token_ix;
    if (!( state.tokens ) |> Array.length > state.token_ix) then
        (state_next_token state), Ok !( state.tokens ).( state.token_ix )
    else
         state, Error "Parser error: Unexpected end of string!"
}
let new_line_p: unit parser = ((is_a Tok_NewL) >>= fun _ -> return ())

(* expressions *)
(* 
  expression -> term + expression | term - expression | term    
  term -> factror * term | factror / term | factror     
  factor -> (expression) | 0,1,2,3..| -factor  
*)  

type exp = 
  | Identifier of string
  | Num of int
  | Min of exp
  | BNot of exp
  | LNot of exp
  | LPart of exp
  | HPart of exp    
  | Sub of exp * exp
  | Sum of exp * exp
  | Mul of exp * exp
  | Div of exp * exp
  | Or of exp * exp
  | And of exp * exp
  | Null

let rec eval f exp : int option = 
  match exp with
  | Identifier s -> f s
  | Num n -> Some n
  | Mul (e1, e2) -> (match (eval f e1, eval f e2) with |(Some v1, Some v2) -> Some (v1 * v2) | _ -> None)
  | Sum (e1, e2) -> (match (eval f e1, eval f e2) with |(Some v1, Some v2) -> Some (v1 + v2) | _ -> None)
  | Div (e1, e2) -> (match (eval f e1, eval f e2) with |(Some v1, Some v2) -> Some (v1 / v2) | _ -> None)
  | Or  (e1, e2) -> (match (eval f e1, eval f e2) with |(Some v1, Some v2) -> Some (Int.logor v1  v2) | _ -> None)
  | And (e1, e2) -> (match (eval f e1, eval f e2) with |(Some v1, Some v2) -> Some (Int.logand v1 v2) | _ -> None)
  | Sub (e1, e2) -> (match (eval f e1, eval f e2) with |(Some v1, Some v2) -> Some (v1 - v2) | _ -> None)
  | Min   e1 ->     (match (eval f e1) with |Some v1 -> Some ((-1) * v1) |None -> None)
  | BNot  e1 ->     (match (eval f e1) with |Some v1 -> Some (Int.lognot v1) |None -> None)
  | LNot  e1 ->     (match (eval f e1) with |Some v1 -> Some (if (Int.abs v1) > 0 then 0 else 1) |None -> None)
  | LPart e1 ->     (match (eval f e1) with |Some v1 -> Some (Int.logand 0xFF v1) |None -> None)
  | HPart e1 ->     (match (eval f e1) with |Some v1 -> Some (Int.shift_right_logical v1 8) |None -> None)
  | Null -> None
;;


let rec exp2string exp: string = 
  match exp with
  | Identifier s -> (sprintf "Ident %s" s)
  | Num n -> (sprintf "Num %d" n)
  | Mul (e1, e2) -> (sprintf "Mul(%s, %s)" (e1 |> exp2string) (e2 |> exp2string) )
  | Sum (e1, e2) -> (sprintf "Sum(%s, %s)" (e1 |> exp2string) (e2 |> exp2string) )
  | Div (e1, e2) -> (sprintf "Div(%s, %s)" (e1 |> exp2string) (e2 |> exp2string) )
  | Sub (e1, e2) -> (sprintf "Sub(%s, %s)" (e1 |> exp2string) (e2 |> exp2string) )
  | Or (e1, e2) -> (sprintf "Or(%s, %s)" (e1 |> exp2string) (e2 |> exp2string) )
  | And (e1, e2) -> (sprintf "And(%s, %s)" (e1 |> exp2string) (e2 |> exp2string) )  
  | Min e1 -> (sprintf "Min(%s)" (e1 |> exp2string)  )
  | BNot e1 -> (sprintf "BitwiseNot(%s)" (e1 |> exp2string)  )
  | LNot e1 -> (sprintf "LogicalNot(%s)" (e1 |> exp2string)  )
  | LPart e1 -> (sprintf "LPart(%s)" (e1 |> exp2string)  )
  | HPart e1 -> (sprintf "HPart(%s)" (e1 |> exp2string)  )    
  | Null -> (sprintf "Null ()" )
;;

let rec exp_compare instr1 instr2: bool = 
  match instr1 with
  | Identifier n1 -> (match instr2 with | Identifier n2 -> if(n1 = n2) then true else false | _ -> false)
  | Num n1 -> (match instr2 with | Num n2 -> if(n1 = n2) then true else false | _ -> false)
  | Mul (e11, e12) ->  (match instr2 with | Mul (e21, e22) -> if(exp_compare e11 e21 && exp_compare e12 e22) then true else false | _ -> false)
  | Sum (e11, e12) ->  (match instr2 with | Sum (e21, e22) -> if(exp_compare e11 e21 && exp_compare e12 e22) then true else false | _ -> false)
  | Div (e11, e12) ->  (match instr2 with | Div (e21, e22) -> if(exp_compare e11 e21 && exp_compare e12 e22) then true else false | _ -> false) 
  | Sub (e11, e12) ->  (match instr2 with | Sub (e21, e22) -> if(exp_compare e11 e21 && exp_compare e12 e22) then true else false | _ -> false)
  | Or (e11, e12) ->  (match instr2 with | Or (e21, e22) -> if(exp_compare e11 e21 && exp_compare e12 e22) then true else false | _ -> false)
  | And (e11, e12) ->  (match instr2 with | And (e21, e22) -> if(exp_compare e11 e21 && exp_compare e12 e22) then true else false | _ -> false)
  | Min e1 -> (match instr2 with | Min e2 -> if(exp_compare e1 e2) then true else false | _ -> false)
  | BNot e1 -> (match instr2 with | BNot e2 -> if(exp_compare e1 e2) then true else false | _ -> false)
  | LNot e1 -> (match instr2 with | LNot e2 -> if(exp_compare e1 e2) then true else false | _ -> false)
  | LPart e1 -> (match instr2 with | LPart e2 -> if(exp_compare e1 e2) then true else false | _ -> false)
  | HPart e1 -> (match instr2 with | HPart e2 -> if(exp_compare e1 e2) then true else false | _ -> false)
  | Null -> false
;;



(* 
  It's >int option< here as result so there is no chance to inform about error in this place.
  If needed, alternative parser >force_number_p< always return number or rise error message.
*)
let number_p: int option parser = { 
  run = fun state -> 
    printf "number_p token:%s byte:%d\n" (if state.token_ix < (Array.length !( state.tokens )) then  (token2str !( state.tokens ).( state.token_ix )) else ">>end<<") state.byte_counter;
    match !( state.tokens ).( state.token_ix ) with
    | Tok_Number n_str  -> 
        (* printf "number_p:[%s]" n_str; *)
        let str_len = String.length n_str in
        if(str_len > 0) then (
          match n_str.[0] with
          | '$' -> (state_next_token state), Ok (String.sub n_str 1 (str_len-1) |> sprintf "0x%s" |> Stdlib.int_of_string_opt) 
          | '%' -> (state_next_token state), Ok (String.sub n_str 1 (str_len-1) |> sprintf "0b%s" |> Stdlib.int_of_string_opt)
          | _ -> (state_next_token state), Ok (Stdlib.int_of_string_opt n_str)
        )
        else state, Error ( sprintf ": Not a number: %s at: [%d] " n_str state.token_ix)
    | Tok_Char ch -> (state_next_token state), Ok (Some (ch |> Char.code))
    | token -> 
     
      state, Error ( sprintf ": Number expected but got: %s at: [%d] " (token2str token) state.token_ix)
}



let rec term_p = 
{ run = fun state -> 
  (* printf "BEGIN term_p num:[%s] at:[%d]\n" (!( state.tokens ).( state.token_ix ) |> token2str) state.token_ix; *)
  state |> (
    factor_p >>= fun exp1 -> 
    getTok >>= fun token ->
      (* printf "term_p/factor_p/getTok:[%s]\n" (token |> token2str); *)
      match token with
      | Tok_Mul -> term_p >>= fun exp2 -> return (Mul (exp1, exp2))
      | Tok_Div -> term_p >>= fun exp2 -> return (Div (exp1, exp2))
      | _ ->
        (* printf "END term_p/factor_p/getTok/return at:[%s]\n" (t |> token2str); *)
        get_back >>= fun _ ->  return exp1).run  (*** get_back -> get one token back to parser!! *)
}

and factor_p: exp parser = {
  run = fun state -> 
    state |> (
      (number_p >>= fun res -> match res with |Some v -> return (Num v) |_-> fail "Not a number in expression") <|>
      (word_p >>= fun w -> return (Identifier w)) <|> (* if expression is evaluated unrecognized identifier couse expression value = None *)
      ((is_a Tok_LParen) *> exp_p <* (is_a Tok_RParen)) <|>
      ((is_a Tok_Sub) *> (number_p >>= fun res -> match res with |Some v ->  return (Min (Num v))|_ -> fail "Not a number in expression")) <|>
      ((is_a Tok_LNot) *> (number_p >>= fun res -> match res with |Some v ->  return (LNot (Num v))|_ -> fail "Not a number in expression")) <|>
      ((is_a Tok_BNot) *> (number_p >>= fun res -> match res with |Some v ->  return (BNot (Num v))|_ -> fail "Not a number in expression")) <|>
      ((is_a Tok_Less) *> (number_p >>= fun res -> match res with |Some v ->  return (LPart (Num v))|_ -> fail "Not a number in expression")) <|>
      ((is_a Tok_More) *> (number_p >>= fun res -> match res with |Some v ->  return (HPart (Num v))|_ -> fail "Not a number in expression"))
    ).run
}
and exp_p : exp parser  =
{ run = fun state -> 
   (* printf "BEGIN exp_p num:[%s] at:[%d]\n" (!( state.tokens ).( state.token_ix ) |> token2str) state.token_ix; *)
  state |> (
      term_p >>= fun exp1 -> 
      getTok >>= fun token ->
        (* printf "exp_p/term_p/getTok:[%s]\n" (token |> token2str); *)
        match(token) with
          | Tok_Sum -> exp_p >>= fun exp2 -> return (Sum (exp1, exp2))
          | Tok_Sub -> exp_p >>= fun exp2 -> return (Sub (exp1, exp2))
          | Tok_Or  -> exp_p >>= fun exp2 -> return (Or  (exp1, exp2))
          | Tok_And -> exp_p >>= fun exp2 -> return (And (exp1, exp2))
          | _ -> 
            (* printf "END exp_p/term_p/getTok/return at:[%s]\n" (t |> token2str); *)
            get_back >>= fun _ ->  return exp1).run   (*** get_back -> get one token back to parser!! *)
}
