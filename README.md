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
{| 
  .org $8000
  .equ Char1 'A'
  Char2 = ('A' + 1)

  l0: JMP (l1)
      LDA heart, X
      CMP #Char1 + 1
      BNE l1
      CMP #Char2
      LDA $A0, X
  l1: JMP l0
  heart: .byte %00000, %01010, %11111
|} |> tokenize |> preprocess_tokens  |> asm_lines_p.run;;
```

## Output:
```
...
 >>>success
- : state * (asm_line list, string) result =
({tokens =
   {contents =
     [|Tok_NewL; Tok_Label "l0:"; Tok_Word "JMP"; Tok_LParen; Tok_Word "l1";
       Tok_RParen; Tok_NewL; Tok_Word "LDA"; Tok_Word "heart"; Tok_Coma;
       Tok_Word "X"; Tok_NewL; Tok_Word "CMP"; Tok_Hash; Tok_Number "65";
       Tok_Sum; Tok_Number "1"; Tok_NewL; Tok_Word "BNE"; Tok_Word "l1";
       Tok_NewL; Tok_Word "CMP"; Tok_Hash; Tok_Number "66"; Tok_NewL;
       Tok_Word "LDA"; Tok_Number "$A0"; Tok_Coma; Tok_Word "X"; Tok_NewL;
       Tok_Label "l1:"; Tok_Word "JMP"; Tok_Word "l0"; Tok_NewL;
       Tok_Label "heart:"; Tok_Direct ".byte"; Tok_Number "%00000"; Tok_Coma;
       Tok_Number "%01010"; Tok_Coma; Tok_Number "%11111"; Tok_NewL; Tok_End|]};
  token_ix = 42; byte_counter = 32788; identifiers = [];
  labels =
   [{name = "heart:"; value = 32785}; {name = "l1:"; value = 32782};
    {name = "l0:"; value = 32768}]},
 Ok
  [Empty; Instr {opcode = 108; operand = Fixed [14; 128]};
   Instr {opcode = 189; operand = Fixed [17; 128]};
   Instr {opcode = 201; operand = Fixed [66]};
   Instr {opcode = 208; operand = Fixed [4]};
   Instr {opcode = 201; operand = Fixed [66]};
   Instr {opcode = 181; operand = Fixed [160]};
   Instr {opcode = 76; operand = Fixed [0; 128]};
   Label_Data ("heart:", 32785, [0; 10; 31])])
```
