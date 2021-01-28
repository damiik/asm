
(* 
asm - Copyright (c) 2020 Dariusz Mikołajczyk 
*)

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
    ident_v_refs: int list; (* to remove *)
    labels : identifier_table_t list;
  }

let set_state (t, i, c, r, v) = {tokens = t; token_ix = i; byte_counter = c; ident_v_refs = r; labels = v;}
let state_next_token s = {tokens = s.tokens; token_ix = s.token_ix + 1; byte_counter = s.byte_counter; ident_v_refs = s.ident_v_refs; labels = s.labels }

type error = { 

    desc: string;
    token_ix: int
  }

type 'a parser = {

    run : state -> state * ('a, string) result
  }

let fail (e: string) = { 
  run = fun state -> 

    state, Error (Printf.sprintf ": %s: %s at: [%d] " e (token2str !( state.tokens ).( state.token_ix )) state.token_ix) 
}


let return (x: 'a) = { run = fun state -> state, Ok x}
let get_state = { run = fun state -> 

    Printf.printf "get_state token:%s byte:%d\n" (if state.token_ix < (Array.length !( state.tokens )) then  (token2str !( state.tokens ).( state.token_ix )) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
state, Ok state}

let map (f: 'a -> 'b) (p: 'a parser): 'b parser =
  { run = fun state ->
          match p.run state with
          | state', Ok x -> state', Ok (f x)
          | _, Error error -> state, Error error (* if fail, return old state token_ix *)
  }


let ( >>= )  (p: 'a parser) (f: 'a -> 'b parser)  : 'b parser  = { 
  
  run = fun state -> 
      (* Printf.printf "begin >>= :[%s] at:[%d]\n" (!( state.tokens ).( state.token_ix ) |> token2str) state.token_ix; *)
      let state', result = p.run state in
      match result with
          | Ok x -> 
                (* Printf.printf "end >>= 1:[%s] at:[%d]\n" (!(state'.tokens).( state'.token_ix ) |> token2str) state'.token_ix; *)
                let state'', result' = (f x).run state' in
                (* Printf.printf "end >>= 2:[%s] at:[%d]\n" (!(state''.tokens).( state''.token_ix ) |> token2str) state''.token_ix; *)
                (state'', result')
          | Error error -> 
                (* Printf.printf "end (err) >>= :[%s] at:[%d]\n" (!( state.tokens ).( state.token_ix ) |> token2str) state.token_ix; *)
                state, Error error (* if fail, return old state token_ix *)
}

let bind p f = p >>= f


let ( <|> ) (p1: 'a parser) (p2: 'a parser): 'a parser = { 
  
  run = fun state -> 
    let state', result = state |> p1.run in
    match result with
    | Ok _ -> (state', result)
    | Error _ -> state |> p2.run  
}

let ( *> ) (p1: 'a parser) (p2: 'b parser): 'b parser = { run = fun state -> 
  state |> (p1 >>= (fun _ -> p2 >>= (fun b -> return b))).run }
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

    let rec parse_fun = fun i -> 
      let state', result = p.run i in
      match result with
      | Error error -> i, Error error       
      | Ok x -> let state'', result' = parse_fun state' in
            (match result' with
                      | (Error _) -> (state', Ok (x::[]))       
                      | ( Ok x'') -> (state'', Ok ( x::x'')))
    in
    parse_fun state
}   


let is_a (t: token) : bool parser = {
  run = fun state -> 
    Printf.printf "is_a token:%s byte:%d\n" (if state.token_ix < (Array.length !( state.tokens )) then  (token2str !( state.tokens ).( state.token_ix )) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
    if(tokenCompare !( state.tokens ).( state.token_ix ) t true) then 
        (state_next_token state), Ok true
    else
         state, Error ( Printf.sprintf ": Not a %s at: [%d] " (t |> token2str) state.token_ix)
}

(* return expected string  *)
let word_t (s: string) : string parser = {
  run = fun state -> 
    Printf.printf "word_t token:%s byte:%d\n" (if state.token_ix < (Array.length !( state.tokens )) then  (token2str !( state.tokens ).( state.token_ix )) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
    match !( state.tokens ).( state.token_ix )  with
    | Tok_Word s2 -> if((String.compare s s2) = 0) then (state_next_token state), Ok s
                     else state, Error ( Printf.sprintf ": Not a word %s at: [%d] " s state.token_ix)
    | t ->  state, Error ( Printf.sprintf ": Token %s is not a %s at: [%d] " (t |> token2str) s state.token_ix)
}

(* return any string *)
let word_p: string parser = {
  run = fun state -> 
    Printf.printf "word_p token:%s byte:%d\n" (if state.token_ix < (Array.length !( state.tokens )) then  (token2str !( state.tokens ).( state.token_ix )) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
    match !( state.tokens ).( state.token_ix ) with
    | Tok_Word l_str  -> (state_next_token state), Ok l_str
    | token -> 
      state, Error ( Printf.sprintf ": Word expected but got: %s at: [%d] " (token2str token) state.token_ix)
}

(* 
  parser stosowany w monadach >>= gdzie nie ma bezpośredniego dostępu do state.token_ix
  Cofanie o jedną pozycję.
*)
let get_back  : int parser = {
  run = fun state -> 
    Printf.printf "get_back token:%s byte:%d\n" (if state.token_ix < (Array.length !( state.tokens )) then  (token2str !( state.tokens ).( state.token_ix )) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
    if(state.token_ix > 0) then 
        (state.tokens, state.token_ix - 1, state.byte_counter, state.ident_v_refs, state.labels) |> set_state, Ok (state.token_ix -1)
    else
        state, Error ( Printf.sprintf ": try get_back to %s at: [%d] " (!( state.tokens ).( state.token_ix ) |> token2str) state.token_ix)
}

(* increment byte_counter by i *)
let inc_bcount_p (i:int) : int parser = {
  run = fun state -> 
    Printf.printf "inc_bcount_p token:%s byte:%d\n" (if state.token_ix < (Array.length !( state.tokens )) then  (token2str !( state.tokens ).( state.token_ix )) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
  (state.tokens, state.token_ix, state.byte_counter + i, state.ident_v_refs, state.labels) |> set_state, Ok (state.byte_counter + i)
}


let getTok : token parser = {
  run = fun state -> 
    Printf.printf "getTok t:[%s] at:[%d]\n" (!( state.tokens ).( state.token_ix ) |> token2str) state.token_ix;
    if (!( state.tokens ) |> Array.length > state.token_ix) then
        (state_next_token state), Ok !( state.tokens ).( state.token_ix )
    else
         state, Error "Parser error: Unexpected end of string!"
}
let new_line_p: unit parser = ((is_a Tok_NewL) >>= fun _ -> return ())

(* expressions *)
type exp = 
  | Num of int
  | Min of exp
  | Sub of exp * exp
  | Sum of exp * exp
  | Mul of exp * exp
  | Div of exp * exp
  | Null

let rec eval exp : int = 
  match exp with
  | Num n -> n
  | Mul (e1, e2) -> eval e1 * eval e2
  | Sum (e1, e2) -> eval e1 + eval e2
  | Div (e1, e2) -> eval e1 / eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Min e1 -> (-1) * (eval e1)  
  | Null -> 0
;;


let rec exp2string exp: string = 
  match exp with
  | Num n -> (Printf.sprintf "Num %d" n)
  | Mul (e1, e2) -> (Printf.sprintf "Mul(%s, %s)" (e1 |> exp2string) (e2 |> exp2string) )
  | Sum (e1, e2) -> (Printf.sprintf "Sum(%s, %s)" (e1 |> exp2string) (e2 |> exp2string) )
  | Div (e1, e2) -> (Printf.sprintf "Div(%s, %s)" (e1 |> exp2string) (e2 |> exp2string) )
  | Sub (e1, e2) -> (Printf.sprintf "Sub(%s, %s)" (e1 |> exp2string) (e2 |> exp2string) )
  | Min e1 -> (Printf.sprintf "Min(%s)" (e1 |> exp2string)  )
  | Null -> (Printf.sprintf "Null()" )
;;

let rec exp_compare instr1 instr2: bool = 
  match instr1 with
  | Num n1 -> (match instr2 with | Num n2 -> if(n1 = n2) then true else false | _ -> false)
  | Mul (e11, e12) ->  (match instr2 with | Mul (e21, e22) -> if(exp_compare e11 e21 && exp_compare e12 e22) then true else false | _ -> false)
  | Sum (e11, e12) ->  (match instr2 with | Sum (e21, e22) -> if(exp_compare e11 e21 && exp_compare e12 e22) then true else false | _ -> false)
  | Div (e11, e12) ->  (match instr2 with | Div (e21, e22) -> if(exp_compare e11 e21 && exp_compare e12 e22) then true else false | _ -> false) 
  | Sub (e11, e12) ->  (match instr2 with | Sub (e21, e22) -> if(exp_compare e11 e21 && exp_compare e12 e22) then true else false | _ -> false)
  | Min e1 -> (match instr2 with | Min e2 -> if(exp_compare e1 e2) then true else false | _ -> false)
  | Null -> false
;;

(* 
  expression -> term + expression | term - expression | term    
  term -> factror * term | factror / term | factror     
  factor -> (expression) | 0,1,2,3..| -factor  
*)  



let number_p: int option parser = {
  run = fun state -> 
    Printf.printf "number_p token:%s byte:%d\n" (if state.token_ix < (Array.length !( state.tokens )) then  (token2str !( state.tokens ).( state.token_ix )) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
    match !( state.tokens ).( state.token_ix ) with
    | Tok_Number n_str  -> 
        (* Printf.printf "number_p:[%s]" n_str; *)
        let str_len = String.length n_str in
        if(str_len > 0) then (
          match n_str.[0] with
          | '$' -> (state_next_token state), Ok (String.sub n_str 1 (str_len-1) |> Printf.sprintf "0x%s" |> Stdlib.int_of_string_opt)
          | '%' -> (state_next_token state), Ok (String.sub n_str 1 (str_len-1) |> Printf.sprintf "0b%s" |> Stdlib.int_of_string_opt)
          | _ -> (state_next_token state), Ok (Stdlib.int_of_string_opt n_str)
        )
        else state, Error ( Printf.sprintf ": Not a number: %s at: [%d] " n_str state.token_ix)
    | token -> 
     
      state, Error ( Printf.sprintf ": Number expected but got: %s at: [%d] " (token2str token) state.token_ix)
}

let rec term_p = 
{ run = fun state -> 
  Printf.printf "BEGIN term_p num:[%s] at:[%d]\n" (!( state.tokens ).( state.token_ix ) |> token2str) state.token_ix;
  state |> (
    factor_p >>= fun exp1 -> 
    getTok >>= fun token ->
      Printf.printf "term_p/factor_p/getTok:[%s]\n" (token |> token2str);
      match token with
      | Tok_Mul -> term_p >>= fun exp2 -> return (Mul (exp1, exp2))
      | Tok_Div -> term_p >>= fun exp2 -> return (Div (exp1, exp2))
      | t ->
        Printf.printf "END term_p/factor_p/getTok/return at:[%s]\n" (t |> token2str);
        get_back >>= fun _ ->  return exp1).run  (*** get_back -> odwijamy o jeden token !! *)
}
and factor_p: exp parser = {
  run = fun state -> 
    state |> (
      (number_p >>= fun res -> match res with |Some v -> return (Num v) |_-> fail "Not a number in expression") <|> 
      ((is_a Tok_OBra) *> exp_p <* (is_a Tok_CBra)) <|>
      ((is_a Tok_Sub) *> (number_p >>= fun res -> match res with |Some v ->  return (Min (Num v))|_ -> fail "Not a number in expression"))
    ).run
}
and exp_p : exp parser  =
{ run = fun state -> 
   Printf.printf "BEGIN exp_p num:[%s] at:[%d]\n" (!( state.tokens ).( state.token_ix ) |> token2str) state.token_ix;
  state |> (
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
