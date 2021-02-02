
(* 
asm - Copyright (c) 2020 Dariusz Mikołajczyk 
*)

open Tokenizer
open C6502instr
open Mparser

type operand_data_t = {

  address_mode  : address_mode_t;
  count_pos     : int;  
  expr          : exp
}
type operand_t = 
  Fixed of (int list) | Incomplete of operand_data_t

type instruction_t = {

  opcode        : int;
  operand       :operand_t
}

type asm_line =
      | Label of string * int (* label with name and value (address) *)
      | Instr of instruction_t     (* decoded instruction *)
      | Label_Instr of (string * int * instruction_t)
      | Directive of string
      | Data of int list (* .byte 0x00,.. *)
      | Label_Data of (string * int * (int list))
      | Empty



let int_list2string l = List.fold_left (fun a n -> 

    if(String.length a) = 0 then (Printf.sprintf "$%02X" n)
    else (Printf.sprintf "%s, $%02X" a n)
) "" l

let int_list2string2 l = List.fold_left (fun acc n -> 

  if(String.length acc) = 0 then (Printf.sprintf "0x%02X" n)
  else (Printf.sprintf "%s; 0x%02X" acc n)
) "" l


let instr2string i = 
  match i.operand with
  | Fixed f -> (Printf.sprintf "Instr [$%02X, %s]" i.opcode) (int_list2string f)
  | Incomplete n -> (Printf.sprintf "Instr %s [$%02X] %s %d" (address_mode2string n.address_mode)  i.opcode (n.expr |> exp2string) n.count_pos)

let instr2string2 i = 
  match i.operand with
  | Fixed f -> 
    (match (int_list2string2 f) with 
    | "" -> Printf.sprintf "0x%02X" i.opcode
    | s -> Printf.sprintf "0x%02X; %s" i.opcode s)
  | Incomplete n -> Printf.sprintf "0x%02X; %s" i.opcode (n.expr |> exp2string) 


let int_listcompare l1 l2 : bool = 
  try
    List.fold_left (fun acc (n1, n2) -> (n1 = n2 && acc)) true (List.combine l1 l2)
  with
    Invalid_argument _ -> false

let instr_compare i1 i2 = 

  match i1.operand with
  | Fixed f1 -> (match i2.operand with |Fixed f2 -> (i1.opcode = i2.opcode) && (int_listcompare f1 f2) | _ -> false)
  | Incomplete f1 -> (match i2.operand with | Incomplete f2 -> ((i1.opcode = i2.opcode) && (f2.address_mode = f1.address_mode) && (exp_compare f1.expr f2.expr) && f1.count_pos = f2.count_pos ) | _ -> false)


let rec asm_line2string instr: string = 
  match instr with
  | Label (s, v) -> (Printf.sprintf "Label %s %d" s v)
  | Instr l -> instr2string l
  | Directive s -> (Printf.sprintf "Directive %s" s)
  | Data l -> (Printf.sprintf "Data [%s]" (int_list2string l))
  | Label_Instr (s, v, i) -> (Printf.sprintf "(Label %s %d, %s)" s v ((Instr i) |> asm_line2string))
  | Label_Data (s, v, d) -> (Printf.sprintf "(Label %s %d, %s)" s v ((Data d) |> asm_line2string))
  | Empty -> (Printf.sprintf "()" )

let rec asm_line2string2 instr: string = 
  match instr with
  | Label (_, _) -> ""
  | Instr l -> instr2string2 l
  | Directive _ -> ""
  | Data l -> int_list2string2 l
  | Label_Instr (_, _, i) -> (Instr i) |> asm_line2string2
  | Label_Data (_, _, d) -> (Data d) |> asm_line2string2
  | Empty -> ""

let rec asm_line2int_list instr: int list = 
  match instr with
  | Instr l ->  (match l.operand with
      | Fixed f -> l.opcode :: f
      | Incomplete _ -> l.opcode :: [-1])
  | Data l -> l
  | Label_Instr (_, _, i) -> (Instr i) |> asm_line2int_list
  | Label_Data (_, _, d) -> (Data d) |> asm_line2int_list
  | Empty | Label (_, _) | Directive _ -> []
  
  

let rec asm_line_list2string l : string = 

  match l with
  |(x::[]) -> Printf.sprintf "%s" (asm_line2string x) 
  |(x::xs) -> Printf.sprintf "%s, %s" (asm_line2string x)  (asm_line_list2string xs)
  | _ -> ""


  let rec asm_line_list2string2 l : string = 

    match l with
    |(x::[]) -> Printf.sprintf "%s" (asm_line2string2 x) 
    |(x::xs) -> let x_str = (asm_line2string2 x) in
                if (x_str |> String.length > 0) then 
                  Printf.sprintf "%s; %s" x_str  (asm_line_list2string2 xs)
                else 
                  Printf.sprintf "%s" (asm_line_list2string2 xs)
    | _ -> ""
  

    let rec asm_line_list2int_list l : int list = 
      match l with
      |(x::[]) -> asm_line2int_list x 
      |(x::xs) -> (asm_line2int_list x) @ (asm_line_list2int_list xs)
      | _ -> []
      

let rec asm_line_compare instr1 instr2: bool = 
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
                                            (asm_line_compare (Label (s1, v1)) (Label (s2, v2)) ) && (asm_line_compare (Instr i1) (Instr i2) ) 
                                     | _ -> false)
  | Label_Data (s1, v1, d1) -> 
                  (match instr2 with | Label_Data (s2, v2, d2) ->  
                                            (asm_line_compare (Label (s1, v1)) (Label (s2, v2)) ) && (asm_line_compare (Data d1) (Data d2) ) 
                                     | _ -> false)
  | Empty -> 
                  (match instr2 with | Empty ->  true 
                                     | _ -> false)

let rec asm_line_list_compare l1 l2 : bool = 

  match (l1, l2) with
  | ([], []) -> true
  | (x1::xs1, x2::xs2) -> (asm_line_compare x1 x2) && (asm_line_list_compare xs1 xs2)
  | _ -> false


let label_p: asm_line parser = {
  run = fun state -> 
   
    match !( state.tokens ).( state.token_ix ) with (* określić aktualną pozycję jako pole w state lub obliczać później *)
    | Tok_Label l_str -> 
        let label = Label (l_str, state.byte_counter) in
        (* state: add new label, go next token *)
        let new_labels = ({name=l_str; value=state.byte_counter}::state.labels) in
         Printf.printf "new label: %s size:%d\n" l_str (List.length new_labels) ;
         {tokens = state.tokens; 
         token_ix = (state.token_ix + 1); 
         byte_counter = state.byte_counter; 
         identifiers = state.identifiers; 
         labels = new_labels;}, Ok label
        (* (set_state state.tokens (state.token_ix + 1) state.byte_counter state.ident_v_refs labels), Ok label *)
    | token -> 
      state, Error ( Printf.sprintf ": Label expected but got: %s at: [%d] " (token2str token) state.token_ix)
}


let new_linei_p: asm_line parser = new_line_p >>= fun () -> return Empty


let int2List2B (n : int) : int list   = 
  let u  = if n < 0 then (Int.lognot (-n)) + 1 else n
  in [Int.logand u 0xFF; Int.logand (Int.shift_right_logical u 8) 0xFF]

let int2List1B (n : int) : int list  = 
  [Int.logand (if n < 0 then (Int.lognot (-n)) + 1 else n) 0xFF]

(* #$44 *)
let immediate_p : (address_mode_t * operand_t) parser = 
  is_a(Tok_Hash) *> exp_p <*> get_labels <* new_linei_p >>= fun (m, ll) ->
    Printf.printf "\t\t\t\t\t\t\t\t detected addr mode: >>>>>>> immediate_p\n";
    match eval (get_label ll) m with
    | Some n -> return (Immediate, Fixed ( n |> int2List1B))
    | None -> return (Immediate, Incomplete {address_mode = Immediate; count_pos = -1; expr = m})


(*   $44   *)
let zeropage_p : (address_mode_t * operand_t) parser = (* zeropage have to be tested before absolute! *)
  exp_p <*> get_labels <* new_linei_p >>= fun (m, ll) -> 
    Printf.printf "\t\t\t\t\t\t\t\t detected addr mode: >>>>>>> zeropage_p\n";
    match eval (get_label ll) m with
    | Some n -> if n <= 255 then return (ZeroPage, Fixed (int2List1B n)) 
                else fail "ZeroPage mode but Operand is greather than 255"  
    | None -> return (ZeroPage, Incomplete {address_mode = ZeroPage; count_pos = -1; expr = m})


(* $4400 *)
let absolute_p : (address_mode_t * operand_t) parser = 
  exp_p <*> get_labels <* new_linei_p >>= fun (m, ll) -> 
    Printf.printf "\t\t\t\t\t\t\t\t detected addr mode: >>>>>>> absolute_p\n";
    match eval (get_label ll) m with
    | Some n -> return (Absolute, Fixed (n |> int2List2B)) 
    | None -> return (Absolute, Incomplete {address_mode = Absolute; count_pos = -1; expr = m})


 
(* $44->$0044 (for JMP, JSR) *)
(* let absolute_force_p : (address_mode_t * operand_t) parser = 
  exp_p <* new_linei_p >>= fun m -> return (Absolute, Fixed ((eval (get_label ll) m) |> int2List2B ))     *)
      


(* ------- *)
let implict_p : (address_mode_t * operand_t) parser = new_linei_p >>= fun _ -> 
  Printf.printf "\t\t\t\t\t\t\t\t detected addr mode: >>>>>>> implict_p\n";
  return (Implicit, Fixed [])


(* ($4400) *) 
let indirect_p : (address_mode_t * operand_t) parser =  (* indirect have to be tested before absolute! *)
  is_a(Tok_LParen) *> exp_p <*> get_labels <* is_a(Tok_RParen) <* new_linei_p >>= fun (m, ll) -> 
    Printf.printf "\t\t\t\t\t\t\t\t detected addr mode: >>>>>>> indirect_p\n";
    match eval (get_label ll) m with
    | Some n -> return (Indirect, Fixed (n |> int2List2B))
    | None -> 
      Printf.printf "indirect_p expr:%s\n" (exp2string m);
      return (Indirect, Incomplete {address_mode = Indirect; count_pos = -1; expr = m})


(* $44,X *)
let x_indexed_zeropage_p : (address_mode_t * operand_t) parser =  (* zeropage have to be tested before absolute! *)
  exp_p <*> get_labels <* is_a(Tok_Coma) <* (word_c "X") <* new_linei_p >>= fun (m, ll) -> 
    Printf.printf "\t\t\t\t\t\t\t\t detected addr mode: >>>>>>> x_indexed_zeropage_p\n";
    match eval (get_label ll) m with
    | Some n -> if n <= 255 then return (ZeroPageXIndexed, Fixed (int2List1B n)) 
                else
                  fail "ZeroPageXIndexed mode but operand is greather than 255" 
    | None -> (* there is no way to detect if this is zero page if we don't know address*)
        fail "ZeroPageXIndexed mode but operand is undefined" 


(* $44,Y *)
let y_indexed_zeropage_p : (address_mode_t * operand_t) parser =  (* zeropage have to be tested before absolute! *)
  exp_p <*> get_labels <* is_a(Tok_Coma) <* (word_c "Y") <* new_linei_p >>= fun (m, ll) -> 
    Printf.printf "\t\t\t\t\t\t\t\t detected addr mode: >>>>>>> y_indexed_zeropage_p\n";
    match eval (get_label ll) m with
    | Some n -> if n <= 255 then (
                  Printf.printf "\t\t\t\t\t\t\t\t return ZeroPageYIndexed with: %d\n" n;
                  return (ZeroPageYIndexed, Fixed (int2List1B n)) )
                else
                  fail "ZeroPageYIndexed mode but Operand is greather than 255" 
    | None -> (* there is no way to detect if this is zero page if we don't know address*)
        fail "ZeroPageYIndexed mode but operand is undefined" 


(* $4400,X *)
let x_indexed_absolute_p : (address_mode_t * operand_t) parser = 
  exp_p <*> get_labels <* is_a(Tok_Coma) <* (word_c "X") <* new_linei_p >>= fun (m, ll) -> 
    Printf.printf "\t\t\t\t\t\t\t\t detected addr mode: >>>>>>> x_indexed_absolute_p\n";
    match eval (get_label ll) m with
    | Some n -> return (AbsoluteXIndexed, Fixed (n |> int2List2B)) 
    | None -> (* we don't know address yet, so always assume it's absolute (not zeropage) *)
        return (AbsoluteXIndexed, Incomplete {address_mode = AbsoluteXIndexed; count_pos = -1; expr = m})


(* $4400,Y *)
let y_indexed_absolute_p : (address_mode_t * operand_t) parser = 
  exp_p <*> get_labels <* is_a(Tok_Coma) <* (word_c "Y") <* new_linei_p >>= fun (m, ll) -> 
    Printf.printf "\t\t\t\t\t\t\t\t detected addr mode: >>>>>>> y_indexed_absolute_p\n";
    match eval (get_label ll) m with
    | Some n -> return (AbsoluteYIndexed, Fixed (n |> int2List2B))
    | None -> (* we don't know address yet, so always assume it's absolute (not zeropage) *)
        return (AbsoluteYIndexed, Incomplete {address_mode = AbsoluteYIndexed; count_pos = -1; expr = m})


(* ($44, X) *)
let x_indexed_indirect_p : (address_mode_t * operand_t) parser = 
  is_a(Tok_LParen) *> exp_p <*> get_labels <* is_a(Tok_Coma) <* (word_c "X") <* is_a(Tok_RParen) <* new_linei_p >>= fun (m, ll) -> 
    Printf.printf "\t\t\t\t\t\t\t\t detected addr mode: >>>>>>> x_indexed_indirect_p\n";
    match eval (get_label ll) m with
    | Some n -> if n <= 255 then return (XIndexedIndirect, Fixed (int2List1B n)) 
                else
                  fail "XIndexedIndirect mode but Operand is greather than 255" 
    | None -> return (XIndexedIndirect, Incomplete {address_mode = XIndexedIndirect; count_pos = -1; expr = m})
  

(* ($44), Y *)
let indirect_y_indexed_p : (address_mode_t * operand_t) parser = 
  is_a(Tok_LParen) *> exp_p <*> get_labels  <* is_a(Tok_RParen) <* is_a(Tok_Coma) <* (word_c "Y") <* new_linei_p >>= fun (m, ll) -> 
    Printf.printf "\t\t\t\t\t\t\t\t detected addr mode: >>>>>>> indirect_y_indexed_p\n";
    match eval (get_label ll) m with
    | Some n -> if n <= 255 then return (IndirectYIndexed, Fixed (int2List1B n)) 
                else
                  fail "IndirectYIndexed mode but Operand is greather than 255" 
    | None -> return (IndirectYIndexed, Incomplete {address_mode = IndirectYIndexed; count_pos = -1; expr = m})

(*   label:   *)
let relative_lab_p : (address_mode_t * operand_t) parser = 
  (word_p >>= get_label_val) <* new_linei_p >>= fun n -> 
    Printf.printf "\t\t\t\t\t\t\t\t detected addr mode: >>>>>>> relative_lab_p\n";
    match n with
    | counted_byte, _, Some label_pos -> return (Relative, (Fixed ((label_pos - (counted_byte + 2)) |> int2List1B)))
    | counted_byte, lable_name, None -> 
      return (Relative, Incomplete {address_mode = Relative; count_pos = counted_byte; expr = (Identifier lable_name)})                       
    
    (* fail "-"  *)
                                

let absolute_lab_p : (address_mode_t * operand_t) parser = 
  (word_p >>= get_label_val) <* new_linei_p >>= fun n -> 
    Printf.printf "\t\t\t\t\t\t\t\t detected addr mode: >>>>>>> absolute_lab_p\n";
    match n with
    | _, _, Some label_pos -> return (Absolute, (Fixed (label_pos |> int2List2B)))
    | _, lable_name, None -> 
          return (Absolute, Incomplete {address_mode = Absolute; count_pos = -1; expr = (Identifier lable_name)})   


let instruction_p : asm_line parser = {

  run = fun state -> 

    Printf.printf "----------------------------------------------------------------- try: instruction_p token:%s byte:%d\n" (if state.token_ix < (Array.length !( state.tokens )) then  (token2str !( state.tokens ).( state.token_ix )) else ">>end<<") state.byte_counter;
    state |> ( 

          (word_p        <*> immediate_p          <*> (inc_bcount_p 2))

      <|> (word_c("JMP") <*> indirect_p           <*> (inc_bcount_p 3)) 
      <|> (word_c("JSR") <*> indirect_p           <*> (inc_bcount_p 3))
      <|> (word_c("JMP") <*> absolute_p           <*> (inc_bcount_p 3))
      <|> (word_c("JSR") <*> absolute_p           <*> (inc_bcount_p 3))

      <|> (word_c("BPL") <*> relative_lab_p       <*> (inc_bcount_p 2))
      <|> (word_c("BMI") <*> relative_lab_p       <*> (inc_bcount_p 2))
      <|> (word_c("BVC") <*> relative_lab_p       <*> (inc_bcount_p 2))
      <|> (word_c("BVS") <*> relative_lab_p       <*> (inc_bcount_p 2))
      <|> (word_c("BCC") <*> relative_lab_p       <*> (inc_bcount_p 2))
      <|> (word_c("BCS") <*> relative_lab_p       <*> (inc_bcount_p 2))
      <|> (word_c("BNE") <*> relative_lab_p       <*> (inc_bcount_p 2))
      <|> (word_c("BEQ") <*> relative_lab_p       <*> (inc_bcount_p 2))

      <|> (word_p        <*> x_indexed_indirect_p <*> (inc_bcount_p 2))  
      <|> (word_p        <*> indirect_y_indexed_p <*> (inc_bcount_p 2)) (* must be before zeropage and absolute *) 
 
      <|> (word_p        <*> y_indexed_zeropage_p <*> (inc_bcount_p 2)) (* must be before absolute *)      
      <|> (word_p        <*> x_indexed_zeropage_p <*> (inc_bcount_p 2)) (* must be before absolute *)      
 
      <|> (word_p        <*> x_indexed_absolute_p <*> (inc_bcount_p 3))
      <|> (word_p        <*> y_indexed_absolute_p <*> (inc_bcount_p 3))
      
      
      <|> (word_p        <*> indirect_p           <*> (inc_bcount_p 3)) (* must be before zeropage and absolute *)  
      <|> (word_p        <*> zeropage_p           <*> (inc_bcount_p 2)) (* must be before absolute *)
      <|> (word_p        <*> absolute_p           <*> (inc_bcount_p 3))
      <|> (word_p        <*> implict_p            <*> (inc_bcount_p 1))      

      <|> (word_p        <*> absolute_lab_p       <*> (inc_bcount_p 3))

     >>= fun ((opcode,(address_mode, oper)), _) -> 
        Printf.printf "----------------------------------------------------------------- PASS :%s\n" opcode;
        (* let (address_mode, operand) = operand in operand może być [-1;-1]) *)
        let found_opcode : int = get_instruction((String.uppercase_ascii opcode), address_mode)
        in
        if (found_opcode < 255) (* 255 opcode is illigal *)
        then (* return (Instr {opcode= (Int32.of_int found_opcode); operand=oper}) *)
          match oper with
          | Fixed o -> return (Instr ({opcode = found_opcode; operand = (Fixed o)}))
          | Incomplete o -> return (Instr ({opcode = found_opcode; operand = (Incomplete o)}))      
        else (

          Printf.printf 
{|
*************************** INSTRUCTION NOT FOUND! ************************
opcode: %s, address mode: %s
***************************************************************************

|} opcode (address_mode2string2 address_mode);
          fail ": Instruction expected but got"
        )
  ).run
}   

(* 
Parse data and return list of numbers, function conv_f is used to covert data to bytes list or word list 
*)
let get_string_data_p : int list parser = 
  data_string_p  >>= fun str -> return (str |> String.to_seq |> List.of_seq |> List.map (fun ch -> Char.code ch))

let data_number_list_p w = data_number_p  w >>= fun n -> return [n]

let get_data_p w : int list parser = 
  oneOrMore(
    (((data_number_list_p w) <|> get_string_data_p) <* (Tok_Coma |> is_a) <* new_linei_p) <|>
    (((data_number_list_p w) <|> get_string_data_p) <* (Tok_Coma |> is_a)) <|>  
    (((data_number_list_p w) <|> get_string_data_p) <* new_linei_p) <|>
     ((data_number_list_p w) <|> get_string_data_p)
  ) >>= fun i -> match w with |1 -> return (List.fold_left (fun acc e -> acc@(e |> int2List1B)) [] (List.flatten i) ) (* elements of result list are converted in fold_left by function get_data_p *)
                              |_ -> return (List.fold_left (fun acc e -> acc@(e |> int2List2B)) [] (List.flatten i) ) (* elements of result list are converted in fold_left by function get_data_p *)


                              
let data_p : asm_line parser = 
  ((Tok_Direct ".word" |> is_a) *> (get_data_p 2)) <|>
  ((Tok_Direct ".byte" |> is_a) *> (get_data_p 1)) >>= fun l -> 
    return (Data l)


let data_strz_p : asm_line parser = 
  ((Tok_Direct ".asciiz" |> is_a) *> (get_data_p 1) <* (inc_bcount_p 1)) >>= fun l -> 
    return (Data (l@[0]))
    

let postprocess (i: instruction_t) (s: state) = 
  match i.operand with
  | Incomplete d -> 
      (match d.address_mode with

      | Relative -> (* BNE, BEQ ...*)
          Printf.printf "Relative %d\n" (List.length s.labels);
          (match eval (get_label s.labels) d.expr with
          | Some label_pos -> Instr {opcode = i.opcode; operand = (Fixed ((label_pos - (d.count_pos + 2)) |> int2List1B))}
          | None -> Instr i)

      | Indirect | Absolute | AbsoluteXIndexed | AbsoluteYIndexed -> (*  adr  *)
          Printf.printf "%s %d\n" (address_mode2string d.address_mode) (List.length s.labels);
          (match eval (get_label s.labels) d.expr with 
          | Some label_pos -> Instr {opcode = i.opcode; operand = (Fixed (label_pos |> int2List2B))}
          | None -> Instr i)

      | IndirectYIndexed | XIndexedIndirect | ZeroPage (* |  ZeroPageXIndexed | ZeroPageYIndexed *) -> (* (adr),Y *)
          Printf.printf "%s %d\n" (address_mode2string d.address_mode) (List.length s.labels);
          (match eval (get_label s.labels) d.expr with 
          | Some label_pos -> Instr {opcode = i.opcode; operand = (Fixed (label_pos |> int2List1B))}
          | None -> Instr i)

      | _ ->  Instr {opcode= i.opcode; operand= (Fixed [])}
      )
  | Fixed _ -> Instr i

let asm_lines_p : (asm_line list) parser =

  oneOrMore (

    new_linei_p <|>
    (instruction_p) <|>
    (label_p <* new_linei_p) <|>
    (
      (label_p <*> instruction_p) >>= fun (a,b) -> 
        match a with 
        | Label (s, v) -> (
            match b with 
            | Instr i-> return (Label_Instr (s, v, i))
            | _ -> fail "Instruction expected but got")
        |_-> fail "Label and instruction expected but got"
    ) <|>
    (
      label_p <*> (data_p <|> data_strz_p) >>= fun (a,b) -> 
        match a with 
        | Label (s, v) -> (
            match b with 
            | Data i -> return (Label_Data (s, v, i))
            | _ -> fail "Data expected but got")
        |_-> fail "Label and data expected but got"
    
    ) <|>
    (
      (data_p <|> data_strz_p)
    )
  ) <*> get_state >>= fun (r, s) -> (* state can't be modified this way, but can be used for postprocess  *)

    Printf.printf "\n\n******** postprocess ******** token:%s byte:%d\n" (if s.token_ix < (Array.length !(s.tokens)) then  (token2str !(s.tokens).(s.token_ix)) else ">>end<<") s.byte_counter;
    return (List.map (
              fun i -> (
                match i with
                | Instr i -> (postprocess i s)
                | Label_Instr (_, _, i) -> (postprocess i s)
                | x ->  x)
          
              ) r
            )



let run (p: 'a parser) (t: token array ref): ('a, error) result =
  match (t, 0, 0, [], []) |> set_state |> p.run with
  | _     , Ok x    -> Ok x
  | state', Error desc -> Error {token_ix = state'.token_ix; desc = desc; }

