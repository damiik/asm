
module T = Tokenizer
open Tokenizer

(* open C6502instr *)

(* 
testing in utop:

  #load "str.cma";;
  #use "./parser.ml";;
  exp_p.run {tokens= ref (Array.of_list (tokenize "1203*2+5*10  ss")) ; pos= 0};;
  exp_p.run {tokens= ref (Array.of_list (tokenize "(2+(5*3))-8")) ; pos= 0};;
  exp_p.run {tokens= ref (Array.of_list (tokenize "2+5*-(-%0001001010+3)-8")) ; pos= 0};;
*)

exception ParserError of string


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

let bind p f = p >>= f

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


let rec exp2string exp: string = 
  match exp with
  | Num n -> (Printf.sprintf "Num %d" n)
  | Mul (e1, e2) -> (Printf.sprintf "Mul(%s, %s)" (e1 |> exp2string) (e2 |> exp2string) )
  | Sum (e1, e2) -> (Printf.sprintf "Sum(%s, %s)" (e1 |> exp2string) (e2 |> exp2string) )
  | Div (e1, e2) -> (Printf.sprintf "Div(%s, %s)" (e1 |> exp2string) (e2 |> exp2string) )
  | Sub (e1, e2) -> (Printf.sprintf "Sub(%s, %s)" (e1 |> exp2string) (e2 |> exp2string) )
  | Min e1 -> (Printf.sprintf "Min(%s)" (e1 |> exp2string)  )
;;

let rec exp_compare exp1 exp2: bool = 
  match exp1 with
  | Num n1 -> (match exp2 with | Num n2 -> if(n1 = n2) then true else false | _ -> false)
  | Mul (e11, e12) ->  (match exp2 with | Mul (e21, e22) -> if(exp_compare e11 e21 && exp_compare e12 e22) then true else false | _ -> false)
  | Sum (e11, e12) ->  (match exp2 with | Sum (e21, e22) -> if(exp_compare e11 e21 && exp_compare e12 e22) then true else false | _ -> false)
  | Div (e11, e12) ->  (match exp2 with | Div (e21, e22) -> if(exp_compare e11 e21 && exp_compare e12 e22) then true else false | _ -> false) 
  | Sub (e11, e12) ->  (match exp2 with | Sub (e21, e22) -> if(exp_compare e11 e21 && exp_compare e12 e22) then true else false | _ -> false)
  | Min e1 -> (match exp2 with | Min e2 -> if(exp_compare e1 e2) then true else false | _ -> false)
;;

(* 
  E -> T + E | T - E | T    expression
  T -> F * T | F / T | F    term 
  F -> 0,1,2,3..| (E) | -F  factor
*)  



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
      ((is_a Tok_Sub) *> (number_p >>= fun res -> return (Min (Num res))))
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



let run (p: 'a parser) (t: token array ref): ('a, error) result =
  match {tokens = t; pos = 0} |> p.run with
  | _     , Ok x    -> Ok x
  | input', Error desc -> Error {pos = input'.pos; desc = desc; }

