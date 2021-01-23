
(* module T = Tokenizer *)
open Tokenizer
(* module C = C6502instr *)
open C6502instr

(* open C6502instr *)

(* 
testing in utop:

  #load "str.cma";;
  #use "./parser.ml";;
  exp_p.run {tokens= ref (Array.of_list (tokenize "1203*2+5*10  ss")) ; pos= 0};;
  exp_p.run {tokens= ref (Array.of_list (tokenize "(2+(5*3))-8")) ; pos= 0};;
  exp_p.run {tokens= ref (Array.of_list (tokenize "2+5*-(-%0001001010+3)-8")) ; pos= 0};;



with dune:
  dune utop ./lib
  #use "./lib/parser.ml";;  
  inst_line_p.run {tokens= ref (Array.of_list (tokenize "CMP $4401,X\n")) ; pos= 0};;



  #load "./_build/default/lib/tokenizer/tokenizer.cma";;
  #load "./_build/default/lib/parser.cma";;
*)

exception ParserError of string


type state =
  { tokens: token array ref;
    pos: int;
    byte_counter: int;
  }

let set_state t p c = {tokens = t; pos = p; byte_counter = c}

type error = { 

    desc: string;
    pos: int
  }

type 'a parser = {

    run : state -> state * ('a, string) result
  }

let fail (e: string) = { 
  run = fun state -> 

        state, Error (Printf.sprintf ": %s: %s at: [%d] " e (token2str !(state.tokens).( state.pos )) state.pos) 
}

let return (x: 'a) = { run = fun state -> state, Ok x}


let map (f: 'a -> 'b) (p: 'a parser): 'b parser =
  { run = fun state ->
          match p.run state with
          | state', Ok x        -> state', Ok (f x)
          | _, Error error -> state, Error error (* if fail, return old state pos *)
  }


let ( >>= )  (p: 'a parser) (f: 'a -> 'b parser)  : 'b parser  = { 
  
  run = fun state -> 
      (* Printf.printf "begin >>= :[%s] at:[%d]\n" (!(state.tokens).( state.pos ) |> token2str) state.pos; *)
      let state', result = p.run state in
      match result with
          | Ok x -> 
                (* Printf.printf "end >>= 1:[%s] at:[%d]\n" (!(state'.tokens).( state'.pos ) |> token2str) state'.pos; *)
                let state'', result' = (f x).run state' in
                (* Printf.printf "end >>= 2:[%s] at:[%d]\n" (!(state''.tokens).( state''.pos ) |> token2str) state''.pos; *)
                (state'', result')
          | Error error -> 
                (* Printf.printf "end (err) >>= :[%s] at:[%d]\n" (!(state.tokens).( state.pos ) |> token2str) state.pos; *)
                state, Error error (* if fail, return old state pos *)
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




(* TODO: change to monad ver *)
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

(* TODO: change to monad ver *)
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

    if(tokenCompare !(state.tokens).( state.pos ) t true) then 
        (set_state state.tokens (state.pos + 1) state.byte_counter), Ok true
    else
         state, Error ( Printf.sprintf ": Not a %s at: [%d] " (t |> token2str) state.pos)
}

(* return expected string  *)
let word_t (s: string) : string parser = {
  run = fun state -> 

    match !(state.tokens).( state.pos )  with
    | Tok_Word s2 -> if((String.compare s s2) = 0) then (set_state state.tokens (state.pos + 1) state.byte_counter), Ok s
                     else state, Error ( Printf.sprintf ": Not a word %s at: [%d] " s state.pos)
    | t ->  state, Error ( Printf.sprintf ": Token %s is not a %s at: [%d] " (t |> token2str) s state.pos)
}

(* return any string *)
let word_p: string parser = {
  run = fun state -> 
   
    match !(state.tokens).( state.pos ) with
    | Tok_Word l_str  -> (set_state state.tokens (state.pos + 1) state.byte_counter), Ok l_str
    | token -> 
      state, Error ( Printf.sprintf ": Word expected but got: %s at: [%d] " (token2str token) state.pos)
}

(* 
  parser stosowany w monadach >>= gdzie nie ma bezpośredniego dostępu do state.pos
  Cofanie o jedną pozycję.
*)
let get_back  : int parser = {
  run = fun state -> 

    if(state.pos > 0) then 
        (set_state state.tokens (state.pos - 1) state.byte_counter), Ok (state.pos -1)
    else
        state, Error ( Printf.sprintf ": try get_back to %s at: [%d] " (!(state.tokens).( state.pos ) |> token2str) state.pos)
}

(* increment byte_counter by i *)
let inc_bcount_p (i:int) : int parser = {
  run = fun state -> (set_state state.tokens state.pos (state.byte_counter + i)), Ok (state.byte_counter + i)
}


let getTok : token parser = {
  run = fun state -> 
    Printf.printf "getTok t:[%s] at:[%d]\n" (!(state.tokens).( state.pos ) |> token2str) state.pos;
    if (!(state.tokens) |> Array.length > state.pos) then
        (set_state state.tokens (state.pos + 1) state.byte_counter), Ok !(state.tokens).( state.pos )
    else
         state, Error "Parser error: Unexpected end of string!"
}





(* example  *)
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



let number_p: int parser = {
  run = fun state -> 
   
    match !(state.tokens).( state.pos ) with
    | Tok_Num n_str  -> 
        (* Printf.printf "number_p:[%s]" n_str; *)
        let str_len = String.length n_str in
        if(str_len > 0) then (
          match n_str.[0] with
          | '$' -> (set_state state.tokens (state.pos + 1) state.byte_counter), Ok (String.sub n_str 1 (str_len-1) |> Printf.sprintf "0x%s" |> int_of_string)
          | '%' -> (set_state state.tokens (state.pos + 1) state.byte_counter), Ok (String.sub n_str 1 (str_len-1) |> Printf.sprintf "0b%s" |> int_of_string)
          | _ -> (set_state state.tokens (state.pos + 1) state.byte_counter), Ok (int_of_string n_str)
        )
        else state, Error ( Printf.sprintf ": Not a number: %s at: [%d] " n_str state.pos)
    | token -> 
     
      state, Error ( Printf.sprintf ": Number expected but got: %s at: [%d] " (token2str token) state.pos)
}

let rec term_p = 
{ run = fun state -> 
  Printf.printf "BEGIN term_p num:[%s] at:[%d]\n" (!(state.tokens).( state.pos ) |> token2str) state.pos;
  state |> (
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
  run = fun state -> 
    state |> (
      (number_p >>= fun res -> return (Num res)) <|> 
      ((is_a Tok_OBra) *> exp_p <* (is_a Tok_CBra)) <|>
      ((is_a Tok_Sub) *> (number_p >>= fun res -> return (Min (Num res))))
    ).run
}
and exp_p : exp parser  =
{ run = fun state -> 
   Printf.printf "BEGIN exp_p num:[%s] at:[%d]\n" (!(state.tokens).( state.pos ) |> token2str) state.pos;
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


type inst_line =
      | Label of string * int (* label with name and value (address) *)
      | Instr of int32 list     (* decoded instruction *)
      | Label_Instr of (string * int * (int32 list))
      | Directive of string
      | Data of int32 list (* .byte 0x00,.. *)
      | Label_Data of (string * int * (int32 list))
      | Empty

let int32_list2string l = List.fold_left (fun a n -> 

    if(String.length a) = 0 then (Printf.sprintf "$%02X" (Int32.to_int n))
    else (Printf.sprintf "%s, $%02X" a (Int32.to_int n))
) "" l


let int32_listcompare l1 l2 : bool = 
  try
    List.fold_left (fun acc (n1, n2) -> (n1 = n2 && acc)) true (List.combine l1 l2)
  with
    Invalid_argument _ -> false



let rec instr2string instr: string = 
  match instr with
  | Label (s, v) -> (Printf.sprintf "Label %s %d" s v)
  | Instr l -> (Printf.sprintf "Instr [%s]" (int32_list2string l))
  | Directive s -> (Printf.sprintf "Directive %s" s)
  | Data l -> (Printf.sprintf "Data [%s]" (int32_list2string l))
  | Label_Instr (s, v, i) -> (Printf.sprintf "(Label %s %d, %s)" s v ((Instr i) |> instr2string))
  | Label_Data (s, v, d) -> (Printf.sprintf "(Label %s %d, Data %s)" s v ((Data d) |> instr2string))
  | Empty -> (Printf.sprintf "()" )


let rec instr_list2string l : string = 

  match l with
  |(x::[]) -> Printf.sprintf "%s"  (instr2string x) 
  |(x::xs) -> Printf.sprintf "%s, %s"  (instr2string x)  (instr_list2string xs)
  | _ -> ""


let rec instr_compare instr1 instr2: bool = 
  match instr1 with
  | Label (s1,v1) ->  
                  (match instr2 with | Label (s2, v2) -> 
                                            if((String.compare s1 s2) = 0 && v1 = v2) then true else false 
                                     | _ -> false)
  | Instr l1 -> 
                  (match instr2 with | Instr l2 -> 
                                            if(int32_listcompare l1 l2) then true else false 
                                     | _ -> false)
  | Directive s1 -> 
                  (match instr2 with | Directive s2 -> 
                                            if(s1 = s2) then true else false 
                                     | _ -> false)
  | Data l1 -> 
                  (match instr2 with | Data l2 -> 
                                            if(int32_listcompare l1 l2) then true else false 
                                     | _ -> false)
  | Label_Instr (s1, v1, i1) -> 
                  (match instr2 with | Label_Instr (s2, v2, i2) ->  
                                            (instr_compare (Label (s1, v1)) (Label (s2, v2)) ) && (instr_compare (Instr i1) (Instr i2) ) 
                                     | _ -> false)
  | Label_Data (s1, v1, d1) -> 
                  (match instr2 with | Label_Data (s2, v2, d2) ->  
                                            (instr_compare (Label (s1, v1)) (Label (s2, v2)) ) && (instr_compare (Data d1) (Data d2) ) 
                                     | _ -> false)
  | Empty -> 
                  (match instr2 with | Empty ->  true 
                                     | _ -> false)

let rec instr_list_compare l1 l2 : bool = 

  match (l1, l2) with
  | ([], []) -> true
  | (x1::xs1, x2::xs2) -> (instr_compare x1 x2) && (instr_list_compare xs1 xs2)
  | _ -> false


let label_p: inst_line parser = {
  run = fun state -> 
   
    match !(state.tokens).( state.pos ) with (* określić aktualną pozycję jako pole w state lub obliczać później *)
    | Tok_Label l_str -> (set_state state.tokens (state.pos + 1) state.byte_counter), Ok (Label (l_str, state.byte_counter))
    | token -> 
      state, Error ( Printf.sprintf ": Label expected but got: %s at: [%d] " (token2str token) state.pos)
}

let new_line_p: inst_line parser = ((is_a Tok_NewL) >>= fun _ -> return Empty)


let number_to_list (n: int) ?(list_size = 0) () = 

  if(n>65535 || list_size = 4) then 
    [Int32.logand (Int32.of_int n) (Int32.of_int 0xFF); Int32.shift_right_logical (Int32.of_int n) 8] (* LSB, MSB *)
  else 

  if(n>255 || list_size = 2) then 
    [Int32.logand (Int32.of_int n) (Int32.of_int 0xFF); Int32.shift_right_logical (Int32.of_int n) 8] (* LSB, MSB *)
  else 
    [(Int32.of_int n)]



(* #$44 *)
let immediate_p : (address_mode * (int32 list)) parser = 
  is_a(Tok_Hash) *> number_p <* new_line_p >>= fun n -> 
                              return (Immediate,  (number_to_list n ()))

(* $4400 *)
let absolute_p : (address_mode * (int32 list)) parser = 
  number_p <* new_line_p >>= fun n -> 
                              if n <= 255 then fail "-" 
                              else
                                return (Absolute, (number_to_list n ~list_size:2 ())) 


let absolute_force_p : (address_mode * (int32 list)) parser = 
  number_p <* new_line_p >>= fun n -> 
                                return (Absolute, (number_to_list n ~list_size:2 ()))                           
(* $44 - dla jmp,jsr to jest tryb absolute!*)
let zeropage_p : (address_mode * (int32 list)) parser = 
  number_p <* new_line_p >>= fun n -> 
                              if n <= 255 then return (ZeroPage, (number_to_list n ())) 
                              else fail "-" 

(* ------- *)
let implict_p : (address_mode * (int32 list)) parser = 
  new_line_p >>= fun _ -> 
                          return (Implicit, [])

(* ($4400) *)
let indirect_p : (address_mode * (int32 list)) parser = 
  is_a(Tok_OBra) *> number_p <* is_a(Tok_CBra) <* new_line_p >>= fun n -> 
                              return (Indirect, (number_to_list n ~list_size:2 ()))

(* $44,X *)
let x_indexed_zeropage_p : (address_mode * (int32 list)) parser = 
  number_p <* is_a(Tok_Coma) <* (is_a(Tok_Word "X") <|> is_a(Tok_Word "x")) <* new_line_p >>= fun n -> 
  if n <= 255 then return (ZeroPageXIndexed, (number_to_list n ())) 
  else
    fail "-"


(* $4400,X *)
let x_indexed_absolute_p : (address_mode * (int32 list)) parser = 
  number_p <* is_a(Tok_Coma) <* (is_a(Tok_Word "X") <|> is_a(Tok_Word "x")) <* new_line_p >>= fun n -> 
  if n > 255 then return (AbsoluteXIndexed, (number_to_list n ())) 
  else
    fail "-"

(* $44,Y *)
let y_indexed_zeropage_p : (address_mode * (int32 list)) parser = 
  number_p <* is_a(Tok_Coma) <* (is_a(Tok_Word "Y") <|> is_a(Tok_Word "y")) <* new_line_p >>= fun n -> 
  if n <= 255 then return (ZeroPageYIndexed, (number_to_list n ())) 
  else
    fail "-"


(* $4400,Y *)
let y_indexed_absolute_p : (address_mode * (int32 list)) parser = 
  number_p <* is_a(Tok_Coma) <* (is_a(Tok_Word "Y") <|> is_a(Tok_Word "y")) <* new_line_p >>= fun n -> 
  if n > 255 then return (AbsoluteYIndexed, (number_to_list n ())) 
  else
    fail "-"



(* ($44, X) *)
let x_indexed_indirect_p : (address_mode * (int32 list)) parser = 
  is_a(Tok_OBra) *> number_p <* is_a(Tok_Coma) <* (is_a(Tok_Word "X") <|> is_a(Tok_Word "x")) <* is_a(Tok_CBra) <* new_line_p >>= fun n -> 
  if n <= 255 then return (XIndexedIndirect, (number_to_list n ())) 
  else
    fail "Only byte size number allowed here"

(* ($44),Y *)
let indirect_y_indexed_p : (address_mode * (int32 list)) parser = 
  is_a(Tok_OBra) *> number_p  <* is_a(Tok_CBra) <* is_a(Tok_Coma) <* (is_a(Tok_Word "Y") <|> is_a(Tok_Word "y")) <* new_line_p >>= fun n -> 
  if n <= 255 then return (IndirectYIndexed, (number_to_list n ())) 
  else
    fail "Only byte size number allowed here"


let instruction_p : inst_line parser = {

  run = fun state -> state |> ( 

          (word_p <*> immediate_p                 <*> (inc_bcount_p 2))
      <|> (word_t("JMP") <*> absolute_force_p     <*> (inc_bcount_p 3)) (* jmp, jsr always have absolute mode *)
      <|> (word_t("JSR") <*> absolute_force_p     <*> (inc_bcount_p 3))
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

     >>= fun ((opcode,operand), _) -> 
      let (addrmode, int_list) = operand in
      let found_opcode : int =
        (List.fold_left (
          fun a (i : instruction) -> 
            if ((String.compare i.desc (String.uppercase_ascii opcode) = 0) && i.addrmode = addrmode) 
            then i.code else a
          ) 255 instructions
        )
      in
      if (found_opcode < 255) 
      then return (Instr ((Int32.of_int found_opcode)::int_list))
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
      |Label (s, v)->(match b with 
                | Instr i-> return (Label_Instr (s, v, i))
                | _ -> fail "Instruction expected but got"
                )
      |_-> fail "Label and instruction expected but got"
    )
    
  )




let run (p: 'a parser) (t: token array ref): ('a, error) result =
  match (set_state t 0 0) |> p.run with
  | _     , Ok x    -> Ok x
  | state', Error desc -> Error {pos = state'.pos; desc = desc; }

