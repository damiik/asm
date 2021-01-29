# asm
<p>This is my experimental assembler for 6502 processor in Ocaml with monadic parser described here: https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf. </p>
<p>This program was mainly inspired by Tsoding parcoom parser: https://github.com/tsoding/parcoom</p>
<p>
As input to parser I used tokens from lexer (instead of string) and this simple solution lets me to forget about whitespaces and comments in parser grammar.</p>
<p>Now assembling is processed in 3 passes: pre-processing parse with directives calculations (preprocess), parsing tokens and generating instructions (by asmparser) and post-process with calculating labels relative and absolute positions in code (by asmparser). </p>

---------------------
  ## testing with utop:
  * dune test

  ## executing main
  * dune exec asm

  ## with utop:
 * utop>> `#load "str.cma";;`
 * utop>> `#use "./asmparser.ml";;`
 * utop>> `exp_p.run {tokens= ref (Array.of_list (tokenize "1203*2+5*10  ss")) ; pos= 0};;`
 * utop>> `exp_p.run {tokens= ref (Array.of_list (tokenize "(2+(5*3))-8")) ; pos= 0};;`
 * utop>> `exp_p.run {tokens= ref (Array.of_list (tokenize "2+5*-(-%0001001010+3)-8")) ; pos= 0};;`
---------------------
  ## with dune & utop:
 * `dune utop ./lib`
 * utop>> `#use "./lib/asmparser.ml";;`
 * utop>> `#use "./lib/preprocess/preprocess.ml";;`
 * utop>> 
```
({|
.equ value1 (2*10)
.equ value2 %1000000 + value1

lab0: INY
      BEQ lab0
      BNE lab1
      LDA $10
      CMP $4401
lab1: JMP value2
aData: .word 1 2 3 4 5 6 7 $AA0 
bData: .word %10000000000, $AABB, $CCDD,
$EEEE, $FFFF
|} |> tokenize |> preprocess_tokens |> Array.of_list |> ref, 0, 0, [], []) |> set_state |> inst_line_p.run;;
```

## Output:
```
...
>>>success
-----label:lab1, label_pos:10 d.count_pos:3
- : state * (inst_line list, string) result =
({tokens =
   {contents =
     [|Tok_NewL; Tok_Label "lab0:"; Tok_Word "INY"; Tok_NewL; Tok_Word "BEQ";
       Tok_Word "lab0"; Tok_NewL; Tok_Word "BNE"; Tok_Word "lab1"; Tok_NewL;
       Tok_Word "LDA"; Tok_Number "$10"; Tok_NewL; Tok_Word "CMP";
       Tok_Number "$4401"; Tok_NewL; Tok_Label "lab1:"; Tok_Word "JMP";
       Tok_Number "84"; Tok_NewL; Tok_Label "aData:"; Tok_Direct ".word";
       Tok_Number "1"; Tok_Number "2"; Tok_Number "3"; Tok_Number "4";
       Tok_Number "5"; Tok_Number "6"; Tok_Number "7"; Tok_Number "$AA0";
       Tok_NewL; Tok_Label "bData:"; Tok_Direct ".word";
       Tok_Number "%10000000000"; Tok_Coma; Tok_Number "$AABB"; Tok_Coma;
       Tok_Number "$CCDD"; Tok_Coma; Tok_NewL; Tok_Number "$EEEE"; Tok_Coma;
       Tok_Number "$FFFF"; Tok_NewL; Tok_End|]};
  token_ix = 44; byte_counter = 13; ident_v_refs = [];
  labels =
   [{name = "bData:"; value = 13}; {name = "aData:"; value = 13};
    {name = "lab1:"; value = 10}; {name = "lab0:"; value = 0}]},
 Ok
  [Empty; Label_Instr ("lab0:", 0, {opcode = 200; operand = Fixed []});
   Instr {opcode = 240; operand = Fixed [253]};
   Instr {opcode = 208; operand = Fixed [5]};
   Instr {opcode = 165; operand = Fixed [16]};
   Instr {opcode = 205; operand = Fixed [1; 68]};
   Label_Instr ("lab1:", 10, {opcode = 76; operand = Fixed [84; 0]});
   Label_Data
    ("aData:", 13, [1; 0; 2; 0; 3; 0; 4; 0; 5; 0; 6; 0; 7; 0; 160; 10]);
   Label_Data ("bData:", 13, [0; 4; 187; 170; 221; 204; 238; 238; 255; 255])])
```
