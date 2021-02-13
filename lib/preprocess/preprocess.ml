open Printf
open Tokenizer
open Mparser


(* Preprocessor takes list of tokens, then convert to array and parse this tokens with preprocessor directives.
   As result we get other list of tokens prepared to parse by main asmparser.
 *)

(* let new_line_p: inst_line parser = ((is_a Tok_NewL) >>= fun _ -> return Empty) *)


let rec directives_p (l: token list) : token list parser = {

  run = fun state -> 

  state |> (

    ((Tok_Direct ".org" |> is_a) *> exp_p <* new_line_p  >>= fun v -> 
      inc_bcount_p (match (eval (get_label state.labels) v) with Some n -> n |_-> 0) >>= fun _ ->
    
      l |> directives_p ) <|>
    
    (get_state ((Tok_Direct ".equ" |> is_a) *> word_p <*> exp_p <* new_line_p) >>= fun ((i, v), s) -> 
    
      for t_ix = 0 to ((Array.length !(s.tokens) )-1) do
        (* looking in state.tokens for identifier >i< then setting token Tok_Number with evaluated expr >v< *)
        (match (!(s.tokens).(t_ix)) with 
        | Tok_Word w -> if (String.compare w i)=0 then !(s.tokens).(t_ix) <- (Tok_Number (sprintf "%d" (match eval (get_identifier s.identifiers) v with | Some n -> n | None -> 0))) (* TODO: Tok_Number with int instead of string *)
        | _ -> ()) 
      done;

      l |> directives_p ) <|>

      (get_state((word_p <* (is_a Tok_Equ)) <*> (exp_p <* new_line_p)) >>= fun ((i, v), s) -> 
        for t_ix = 0 to ((Array.length !(s.tokens) )-1) do
          (* looking in state.tokens for identifier >i< then setting token Tok_Number with evaluated expr >v< *)
          (match (!(s.tokens).(t_ix)) with 
          | Tok_Word w -> if (String.compare w i)=0 then !(s.tokens).(t_ix) <- (Tok_Number (sprintf "%d" (match eval (get_identifier s.identifiers) v with | Some n -> n | None -> 0))) (* TODO: Tok_Number with int instead of string *)
          | _ -> ()) 
        done;
  
        l |> directives_p ) <|>

    (* if not directive token checkout if last token, if not call directives_p again *)
    ((getTok) >>= fun token -> 
      match token with 
      | Tok_End -> (return (l@(token::[]))) (* if last token end of recursion, return preprocessed list of tokens *)
      | t -> l@(t::[]) |> directives_p ) (* call directives_p again with preprocessed list of tokens + current token *)
    
  ).run
}

let preprocess_tokens (tokens: token list)  : state =

  match (tokens |> Array.of_list |> ref, 0, 0, [], []) |> set_state |> (directives_p [] ).run with
  | s, Ok x    -> printf "after preprocess tokens:---------------\n%s\n---------------\n" (tokensl2str x);
                     (* TODO: evaluate all identifiers here before return tokens *)
                  set_state (x |> Array.of_list |> ref, 0, s.byte_counter, [], [])
  | s, Error _ -> set_state ([] |> Array.of_list |> ref, 0, s.byte_counter, [], [])
 

