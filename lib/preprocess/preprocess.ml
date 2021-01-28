open Tokenizer
open Mparser


(* Preprocessor takes list of tokens, then convert to array and parse this tokens with preprocessor directives.
   As result we get other list of tokens prepared to parse by main asmparser.
 *)

(* let new_line_p: inst_line parser = ((is_a Tok_NewL) >>= fun _ -> return Empty) *)


let rec directives_p (l: token list) : token list parser = {

  run = fun state -> 

  state |> (
    
    ((Tok_Direct ".equ" |> is_a) *> word_p <*> exp_p <* new_line_p <*> get_state >>= fun ((i, v), s) -> 
    
      for t_ix = 0 to ((Array.length !(s.tokens) )-1) do
        (* looking in state.tokens for identifier i and put token Tok_Number with evaluated expr v*)
        (match (!(s.tokens).(t_ix)) with 
        | Tok_Word w -> if (String.compare w i)=0 then !(s.tokens).(t_ix) <- (Tok_Number (Printf.sprintf "%d" (eval v))) (* TODO: Tok_Number with int instead of string *)
        | _ -> ()) 
      done;

      (* let new_state = Array.map (fun t -> 
      match t with | Tok_Word w -> if (String.compare w i)=0 then (Tok_Number v) else (Tok_Word w) | t -> t
      ) !(s.tokens) in *)
      l |> directives_p ) <|>
    ((getTok) >>= fun token -> match token with Tok_End -> (return (l@(token::[]))) | t -> l@(t::[]) |> directives_p )
    
  ).run
}
let preprocess_tokens (tokens: token list)  : token list =

  match (tokens |> Array.of_list |> ref, 0, 0, [], []) |> set_state |> ([] |> directives_p).run with
  | _     , Ok x -> Printf.printf "after preprocess tokens:---------------\n%s\n---------------\n" (tokensl2str x);
                    x (* TODO: evaluate all identifiers here before return tokens *)
  | _', Error _ -> []
 

  (* match tokens with
  | x::xs -> (match x with 

                | Tok_Direct ".equ" -> []
                | t -> t::[]
              ) @ xs
  | _ ->  *)


