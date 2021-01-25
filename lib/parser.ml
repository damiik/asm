
(* 
asm - Copyright (c) 2020 Dariusz Mikołajczyk 
*)

open Tokenizer
open C6502instr

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

let set_state t i c r v = {tokens = t; token_ix = i; byte_counter = c; ident_v_refs = r; labels = v;}
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

        state, Error (Printf.sprintf ": %s: %s at: [%d] " e (token2str !(state.tokens).( state.token_ix )) state.token_ix) 
}


let return (x: 'a) = { run = fun state -> state, Ok x}
let get_state = { run = fun state -> 

    Printf.printf "get_state token:%s byte:%d\n" (if state.token_ix < (Array.length !(state.tokens)) then  (token2str !(state.tokens).(state.token_ix)) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
state, Ok state}

let map (f: 'a -> 'b) (p: 'a parser): 'b parser =
  { run = fun state ->
          match p.run state with
          | state', Ok x        -> state', Ok (f x)
          | _, Error error -> state, Error error (* if fail, return old state token_ix *)
  }


let ( >>= )  (p: 'a parser) (f: 'a -> 'b parser)  : 'b parser  = { 
  
  run = fun state -> 
      (* Printf.printf "begin >>= :[%s] at:[%d]\n" (!(state.tokens).( state.token_ix ) |> token2str) state.token_ix; *)
      let state', result = p.run state in
      match result with
          | Ok x -> 
                (* Printf.printf "end >>= 1:[%s] at:[%d]\n" (!(state'.tokens).( state'.token_ix ) |> token2str) state'.token_ix; *)
                let state'', result' = (f x).run state' in
                (* Printf.printf "end >>= 2:[%s] at:[%d]\n" (!(state''.tokens).( state''.token_ix ) |> token2str) state''.token_ix; *)
                (state'', result')
          | Error error -> 
                (* Printf.printf "end (err) >>= :[%s] at:[%d]\n" (!(state.tokens).( state.token_ix ) |> token2str) state.token_ix; *)
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
                      | ( Ok x'')-> (state'', Ok ( x::x'')))
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
                      | ( Ok x'')-> (state'', Ok ( x::x'')))
    in
    parse_fun state
}   


let is_a (t: token) : bool parser = {
  run = fun state -> 
    Printf.printf "is_a token:%s byte:%d\n" (if state.token_ix < (Array.length !(state.tokens)) then  (token2str !(state.tokens).(state.token_ix)) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
    if(tokenCompare !(state.tokens).( state.token_ix ) t true) then 
        (state_next_token state), Ok true
    else
         state, Error ( Printf.sprintf ": Not a %s at: [%d] " (t |> token2str) state.token_ix)
}

(* return expected string  *)
let word_t (s: string) : string parser = {
  run = fun state -> 
    Printf.printf "word_t token:%s byte:%d\n" (if state.token_ix < (Array.length !(state.tokens)) then  (token2str !(state.tokens).(state.token_ix)) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
    match !(state.tokens).( state.token_ix )  with
    | Tok_Word s2 -> if((String.compare s s2) = 0) then (state_next_token state), Ok s
                     else state, Error ( Printf.sprintf ": Not a word %s at: [%d] " s state.token_ix)
    | t ->  state, Error ( Printf.sprintf ": Token %s is not a %s at: [%d] " (t |> token2str) s state.token_ix)
}

(* return any string *)
let word_p: string parser = {
  run = fun state -> 
    Printf.printf "word_p token:%s byte:%d\n" (if state.token_ix < (Array.length !(state.tokens)) then  (token2str !(state.tokens).(state.token_ix)) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
    match !(state.tokens).( state.token_ix ) with
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
    Printf.printf "get_back token:%s byte:%d\n" (if state.token_ix < (Array.length !(state.tokens)) then  (token2str !(state.tokens).(state.token_ix)) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
    if(state.token_ix > 0) then 
        (set_state state.tokens (state.token_ix - 1) state.byte_counter state.ident_v_refs state.labels), Ok (state.token_ix -1)
    else
        state, Error ( Printf.sprintf ": try get_back to %s at: [%d] " (!(state.tokens).( state.token_ix ) |> token2str) state.token_ix)
}

(* increment byte_counter by i *)
let inc_bcount_p (i:int) : int parser = {
  run = fun state -> 
    Printf.printf "inc_bcount_p token:%s byte:%d\n" (if state.token_ix < (Array.length !(state.tokens)) then  (token2str !(state.tokens).(state.token_ix)) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
  (set_state state.tokens state.token_ix (state.byte_counter + i) state.ident_v_refs state.labels), Ok (state.byte_counter + i)
}



let getTok : token parser = {
  run = fun state -> 
    Printf.printf "getTok t:[%s] at:[%d]\n" (!(state.tokens).( state.token_ix ) |> token2str) state.token_ix;
    if (!(state.tokens) |> Array.length > state.token_ix) then
        (state_next_token state), Ok !(state.tokens).( state.token_ix )
    else
         state, Error "Parser error: Unexpected end of string!"
}



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
    Printf.printf "number_p token:%s byte:%d\n" (if state.token_ix < (Array.length !(state.tokens)) then  (token2str !(state.tokens).(state.token_ix)) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
    match !(state.tokens).( state.token_ix ) with
    | Tok_Num n_str  -> 
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
  Printf.printf "BEGIN term_p num:[%s] at:[%d]\n" (!(state.tokens).( state.token_ix ) |> token2str) state.token_ix;
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
   Printf.printf "BEGIN exp_p num:[%s] at:[%d]\n" (!(state.tokens).( state.token_ix ) |> token2str) state.token_ix;
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

type operand_data_t = {

  address_mode  : address_mode_t;
  count_pos     : int;  
  identifier    : string;
}
type operand_t = 
  Fixed of (int list) | Incomplete of operand_data_t

type instruction_t = {

  opcode        : int;
  operand       :operand_t
}

type inst_line =
      | Label of string * int (* label with name and value (address) *)
      | Instr of instruction_t     (* decoded instruction *)
      | Label_Instr of (string * int * instruction_t)
      | Directive of string
      | Data of int list (* .byte 0x00,.. *)
      | Label_Data of (string * int * (int list))
      | Empty

let get_label s ll =
      let label_str = Printf.sprintf "%s:" s in
      Printf.printf "get_label searching for: %s, list_size: %d\n" s (List.length ll);
      match (List.filter (fun l -> Printf.printf "curr label: %s\n" l.name; ((String.compare l.name label_str) = 0))) ll with
      | x::_ -> Printf.printf " >>>success\n";Some x.value
      | _ ->  Printf.printf " >>>fail\n";None 

(* return value of label or -1 *)
let get_label_val w : (int * string * (int option)) parser = {

  run = fun state -> state, Ok (state.byte_counter, w, (get_label w state.labels))
}


let int_list2string l = List.fold_left (fun a n -> 

    if(String.length a) = 0 then (Printf.sprintf "$%02X" n)
    else (Printf.sprintf "%s, $%02X" a n)
) "" l


let instr2string i = 

  match i.operand with
  | Fixed f -> (Printf.sprintf "Instr [$%02X, %s]" i.opcode) (int_list2string f)
  | Incomplete n -> (Printf.sprintf "Instr %s [$%02X] %s %d" (address_mode2string n.address_mode)  i.opcode n.identifier n.count_pos)

let int_listcompare l1 l2 : bool = 
  try
    List.fold_left (fun acc (n1, n2) -> (n1 = n2 && acc)) true (List.combine l1 l2)
  with
    Invalid_argument _ -> false

let instr_compare i1 i2 = 

  match i1.operand with
  | Fixed f1 -> (match i2.operand with |Fixed f2 -> (i1.opcode = i2.opcode) && (int_listcompare f1 f2) | _ -> false)
  | Incomplete f1 -> (match i2.operand with | Incomplete f2 -> ((i1.opcode = i2.opcode) && (f2.address_mode = f1.address_mode) && (String.compare f1.identifier f2.identifier)=0 && f1.count_pos = f2.count_pos ) | _ -> false)


let rec instrline2string instr: string = 
  match instr with
  | Label (s, v) -> (Printf.sprintf "Label %s %d" s v)
  | Instr l -> instr2string l
  | Directive s -> (Printf.sprintf "Directive %s" s)
  | Data l -> (Printf.sprintf "Data [%s]" (int_list2string l))
  | Label_Instr (s, v, i) -> (Printf.sprintf "(Label %s %d, %s)" s v ((Instr i) |> instrline2string))
  | Label_Data (s, v, d) -> (Printf.sprintf "(Label %s %d, Data %s)" s v ((Data d) |> instrline2string))
  | Empty -> (Printf.sprintf "()" )


let rec instr_list2string l : string = 

  match l with
  |(x::[]) -> Printf.sprintf "%s"  (instrline2string x) 
  |(x::xs) -> Printf.sprintf "%s, %s"  (instrline2string x)  (instr_list2string xs)
  | _ -> ""


let rec instr_line_compare instr1 instr2: bool = 
  match instr1 with
  | Label (s1,v1) ->  
                  (match instr2 with | Label (s2, v2) -> 
                                            if((String.compare s1 s2) = 0 && v1 = v2) then true else false 
                                     | _ -> false)
  | Instr l1 -> 
                  (match instr2 with | Instr l2 -> 
                                            if(instr_compare l1 l2) then true else false 
                                     | _ -> false)
  | Directive s1 -> 
                  (match instr2 with | Directive s2 -> 
                                            if(s1 = s2) then true else false 
                                     | _ -> false)
  | Data l1 -> 
                  (match instr2 with | Data l2 -> 
                                            if(int_listcompare l1 l2) then true else false 
                                     | _ -> false)
  | Label_Instr (s1, v1, i1) -> 
                  (match instr2 with | Label_Instr (s2, v2, i2) ->  
                                            (instr_line_compare (Label (s1, v1)) (Label (s2, v2)) ) && (instr_line_compare (Instr i1) (Instr i2) ) 
                                     | _ -> false)
  | Label_Data (s1, v1, d1) -> 
                  (match instr2 with | Label_Data (s2, v2, d2) ->  
                                            (instr_line_compare (Label (s1, v1)) (Label (s2, v2)) ) && (instr_line_compare (Data d1) (Data d2) ) 
                                     | _ -> false)
  | Empty -> 
                  (match instr2 with | Empty ->  true 
                                     | _ -> false)

let rec instr_list_compare l1 l2 : bool = 

  match (l1, l2) with
  | ([], []) -> true
  | (x1::xs1, x2::xs2) -> (instr_line_compare x1 x2) && (instr_list_compare xs1 xs2)
  | _ -> false


let label_p: inst_line parser = {
  run = fun state -> 
   
    match !(state.tokens).( state.token_ix ) with (* określić aktualną pozycję jako pole w state lub obliczać później *)
    | Tok_Label l_str -> 
        let label = Label (l_str, state.byte_counter) in
        (* state: add new label, go next token *)
        let new_labels = ({name=l_str; value=state.byte_counter}::state.labels) in
         Printf.printf "new label: %s size:%d\n" l_str (List.length new_labels) ;
         {tokens = state.tokens; 
         token_ix = (state.token_ix + 1); 
         byte_counter = state.byte_counter; 
         ident_v_refs = state.ident_v_refs; 
         labels = new_labels;}, Ok label
        (* (set_state state.tokens (state.token_ix + 1) state.byte_counter state.ident_v_refs labels), Ok label *)
    | token -> 
      state, Error ( Printf.sprintf ": Label expected but got: %s at: [%d] " (token2str token) state.token_ix)
}

let new_line_p: inst_line parser = ((is_a Tok_NewL) >>= fun _ -> return Empty)

let int2List2B (n : int) : int list   = 
  let u  = if n < 0 then (Int.lognot (-n)) + 1 else n
  in [Int.logand u 0xFF; Int.logand (Int.shift_right_logical u 8) 0xFF]

let int2List1B (n : int) : int list  = 
  [Int.logand (if n < 0 then (Int.lognot (-n)) + 1 else n) 0xFF]

(* #$44 *)
let immediate_p : (address_mode_t * operand_t) parser = 
  is_a(Tok_Hash) *> number_p <* new_line_p >>= fun m -> 
      match m with
      |Some n -> return (Immediate, Fixed (int2List1B n))
      | _ -> fail "Operand is not a number"


(* $4400 *)
let absolute_p : (address_mode_t * operand_t) parser = 
  number_p <* new_line_p >>= fun m -> 
      match m with
      |Some n ->  
            if n <= 255 then fail "Absolute mode but operand is greather than 255" 
            else
              return (Absolute, Fixed (int2List2B n)) 
      | _ -> fail "Operand is not a number"

(* $44->$0044 (for JMP, JSR) *)
let absolute_force_p : (address_mode_t * operand_t) parser = 
  number_p <* new_line_p >>= fun m -> 
      match m with
      |Some n -> return (Absolute, Fixed (int2List2B n))    
      | _ -> fail "Operand is not a number"              

(*   $44   *)
let zeropage_p : (address_mode_t * operand_t) parser = 
  number_p <* new_line_p >>= fun m -> 
      match m with
      |Some n ->  
            if n <= 255 then return (ZeroPage, Fixed (int2List1B n)) 
                              else fail "ZeroPage mode but Operand is greather than 255"  
      | _ -> fail "Operand is not a number"  
(* ------- *)
let implict_p : (address_mode_t * operand_t) parser = 
  new_line_p >>= fun _ -> 
                          return (Implicit, Fixed [])

(* ($4400) *)
let indirect_p : (address_mode_t * operand_t) parser = 
  is_a(Tok_OBra) *> number_p <* is_a(Tok_CBra) <* new_line_p >>= fun m -> 
      match m with
      |Some n -> return (Indirect, Fixed (int2List2B n))
      | _ -> fail "Operand is not a number"  

(* $44,X *)
let x_indexed_zeropage_p : (address_mode_t * operand_t) parser = 
  number_p <* is_a(Tok_Coma) <* (is_a(Tok_Word "X") <|> is_a(Tok_Word "x")) <* new_line_p >>= fun m -> 
      match m with
      |Some n -> if n <= 255 then return (ZeroPageXIndexed, Fixed (int2List1B n)) 
                 else
                    fail "ZeroPageXIndexed mode but Operand is greather than 255" 
      | _ -> fail "Operand is not a number"  

(* $4400,X *)
let x_indexed_absolute_p : (address_mode_t * operand_t) parser = 
  number_p <* is_a(Tok_Coma) <* (is_a(Tok_Word "X") <|> is_a(Tok_Word "x")) <* new_line_p >>= fun m -> 
      match m with
      |Some n -> if n > 255 then return (AbsoluteXIndexed, Fixed (int2List2B n)) 
                 else
                   fail "AbsoluteXIndexed mode but Operand is greather than 255" 
      | _ -> fail "Operand is not a number"  
(* $44,Y *)
let y_indexed_zeropage_p : (address_mode_t * operand_t) parser = 
  number_p <* is_a(Tok_Coma) <* (is_a(Tok_Word "Y") <|> is_a(Tok_Word "y")) <* new_line_p >>= fun m -> 
      match m with
      |Some n -> if n <= 255 then return (ZeroPageYIndexed, Fixed (int2List1B n)) 
                 else
                   fail "ZeroPageYIndexed mode but Operand is greather than 255" 
      | _ -> fail "Operand is not a number"  


(* $4400,Y *)
let y_indexed_absolute_p : (address_mode_t * operand_t) parser = 
  number_p <* is_a(Tok_Coma) <* (is_a(Tok_Word "Y") <|> is_a(Tok_Word "y")) <* new_line_p >>= fun m -> 
      match m with
      |Some n -> if n > 255 then return (AbsoluteYIndexed, Fixed (int2List2B n)) 
                 else
                   fail "ZeroPageYIndexed mode but Operand is greather than 255" 
      | _ -> fail "Operand is not a number"  



(* ($44, X) *)
let x_indexed_indirect_p : (address_mode_t * operand_t) parser = 
  is_a(Tok_OBra) *> number_p <* is_a(Tok_Coma) <* (is_a(Tok_Word "X") <|> is_a(Tok_Word "x")) <* is_a(Tok_CBra) <* new_line_p >>= fun m -> 
      match m with
      |Some n -> if n <= 255 then return (XIndexedIndirect, Fixed (int2List1B n)) 
                 else
                   fail "XIndexedIndirect mode but Operand is greather than 255" 
      | _ -> fail "Operand is not a number"  

(* ($44),Y *)
let indirect_y_indexed_p : (address_mode_t * operand_t) parser = 
  is_a(Tok_OBra) *> number_p  <* is_a(Tok_CBra) <* is_a(Tok_Coma) <* (is_a(Tok_Word "Y") <|> is_a(Tok_Word "y")) <* new_line_p >>= fun m -> 
      match m with
      |Some n -> if n <= 255 then return (IndirectYIndexed, Fixed (int2List1B n)) 
                 else
                   fail "IndirectYIndexed mode but Operand is greather than 255" 
      | _ -> fail "Operand is not a number"  

(*   label:   *)
let relative_p : (address_mode_t * operand_t) parser = 
   (word_p >>= get_label_val) <* new_line_p >>= fun n -> 

      match n with
      | counted_byte, _, Some label_pos -> return (Relative, (Fixed ((label_pos - (counted_byte + 2)) |> int2List1B)))
      | counted_byte, lable_name, None -> 
        return (Relative, Incomplete {address_mode = Relative; count_pos = counted_byte; identifier = lable_name})                       
      
      (* fail "-"  *)
                                


let instruction_p : inst_line parser = {

  run = fun state -> 

    Printf.printf "instruction_p token:%s byte:%d\n" (if state.token_ix < (Array.length !(state.tokens)) then  (token2str !(state.tokens).(state.token_ix)) else ">>end<<") state.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) state.labels;
    state |> ( 

          (word_p <*> immediate_p                 <*> (inc_bcount_p 2))
      <|> (word_t("JMP") <*> absolute_force_p     <*> (inc_bcount_p 3)) (* jmp, jsr always have absolute mode *)
      <|> (word_t("JSR") <*> absolute_force_p     <*> (inc_bcount_p 3))

      <|> (word_t("BPL") <*> relative_p           <*> (inc_bcount_p 2))
      <|> (word_t("BMI") <*> relative_p           <*> (inc_bcount_p 2))
      <|> (word_t("BVC") <*> relative_p           <*> (inc_bcount_p 2))
      <|> (word_t("BVS") <*> relative_p           <*> (inc_bcount_p 2))
      <|> (word_t("BCC") <*> relative_p           <*> (inc_bcount_p 2))
      <|> (word_t("BCS") <*> relative_p           <*> (inc_bcount_p 2))
      <|> (word_t("BNE") <*> relative_p           <*> (inc_bcount_p 2))
      <|> (word_t("BEQ") <*> relative_p           <*> (inc_bcount_p 2))

      <|> (word_p        <*> absolute_p           <*> (inc_bcount_p 3))
      <|> (word_p        <*> zeropage_p           <*> (inc_bcount_p 2))
      <|> (word_p        <*> implict_p            <*> (inc_bcount_p 1))
      <|> (word_p        <*> indirect_p           <*> (inc_bcount_p 3))
      <|> (word_p        <*> x_indexed_absolute_p <*> (inc_bcount_p 3))
      <|> (word_p        <*> x_indexed_zeropage_p <*> (inc_bcount_p 2))      
      <|> (word_p        <*> y_indexed_absolute_p <*> (inc_bcount_p 3))
      <|> (word_p        <*> y_indexed_zeropage_p <*> (inc_bcount_p 2))      
      <|> (word_p        <*> x_indexed_indirect_p <*> (inc_bcount_p 2))
      <|> (word_p        <*> indirect_y_indexed_p <*> (inc_bcount_p 2))

     >>= fun ((opcode,(address_mode, oper)), _) -> 
        (* let (address_mode, operand) = operand in operand może być [-1;-1]) *)
        let found_opcode : int = get_instruction((String.uppercase_ascii opcode), address_mode)
        in
        if (found_opcode < 255) 
        then (* return (Instr {opcode= (Int32.of_int found_opcode); operand=oper}) *)
          match oper with
          | Fixed o -> return (Instr ({opcode = found_opcode; operand = (Fixed o)}))
          | Incomplete o -> return (Instr ({opcode = found_opcode; operand = (Incomplete o)}))      
        else fail ": Instruction expected but got"
  ).run
}


let inst_line_p : (inst_line list) parser =

  oneOrMore (

    new_line_p <|>
    (instruction_p) <|>
    (label_p <* new_line_p) <|>
    ((label_p <*> instruction_p) >>= fun (a,b) -> 
        match a with 
        | Label (s, v) -> (
            match b with 
            | Instr i-> return (Label_Instr (s, v, i))
            | _ -> fail "Instruction expected but got")
        |_-> fail "Label and instruction expected but got")
  ) <*> get_state >>= fun (r, s) -> 

    Printf.printf "postprocess token:%s byte:%d\n" (if s.token_ix < (Array.length !(s.tokens)) then  (token2str !(s.tokens).(s.token_ix)) else ">>end<<") s.byte_counter;
    List.iter (fun l -> Printf.printf "\t\t\t\tlabel:%s %d\n" l.name l.value ) s.labels;
    return (List.map (
      fun i -> (
          match i with
          | Instr x -> (
              match x.operand with
              | Incomplete d -> 
                  (match d.address_mode with
                  | Relative -> 
                      Printf.printf "Relative %d\n" (List.length s.labels);
                      (match get_label d.identifier s.labels with 
                      | Some label_pos -> Printf.printf "-----label:%s, label_pos:%d d.count_pos:%d\n" d.identifier label_pos d.count_pos;
                      Instr {opcode = x.opcode; operand = (Fixed ((label_pos - (d.count_pos + 2)) |> int2List1B))}
                      | None -> Instr x)
                  | Absolute -> 
                      Printf.printf "Absolute %d\n" (List.length s.labels);
                      (match get_label d.identifier s.labels with 
                      | Some label_pos -> Instr {opcode = x.opcode; operand = (Fixed ((label_pos - (d.count_pos + 3)) |> int2List2B))}
                      | None -> Instr x)
                  | _ ->  Instr {opcode= x.opcode; operand= (Fixed [])}
                  )
              | Fixed _ -> Instr x)
          
          | x ->  x)
    
  ) r)


let run (p: 'a parser) (t: token array ref): ('a, error) result =
  match (set_state t 0 0 [] []) |> p.run with
  | _     , Ok x    -> Ok x
  | state', Error desc -> Error {token_ix = state'.token_ix; desc = desc; }

