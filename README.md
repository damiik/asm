# asm
My experimental 6502 processor assembler in Ocaml with monadic parser described here https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf. As input to parser I used tokens from lexer, not a string of text like in mentioned documet. This simple solution lets me to forget about whitespaces and comments in parser grammar.
---------------------
  ## testing in utop:
 * utop>> `#load "str.cma";;`
 * utop>> `#use "./parser.ml";;`
 * utop>> `exp_p.run {tokens= ref (Array.of_list (tokenize "1203*2+5*10  ss")) ; pos= 0};;`
 * utop>> `exp_p.run {tokens= ref (Array.of_list (tokenize "(2+(5*3))-8")) ; pos= 0};;`
 * utop>> `exp_p.run {tokens= ref (Array.of_list (tokenize "2+5*-(-%0001001010+3)-8")) ; pos= 0};;`
---------------------
  ## testing with dune:
 * `dune utop ./lib`
 * utop>> `#use "./lib/parser.ml";;`
 * utop>> `inst_line_p.run (set_state (ref (Array.of_list (tokenize "JMP $09\nCMP $4401,X\nl1: LDA $1000\n"))) 0 0 [] []);;[$%02X]`
