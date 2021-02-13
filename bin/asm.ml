(* 
asm - Copyright (c) 2020 Dariusz Mikołajczyk 
*)

open Printf
open Tokenizer
open Mparser
open Preprocess
open Asmparser


let () =

(* (if( (Array.length Sys.argv) > 0) then (Sys.argv.(1)) else a)));; *)



(* showTokens ( tokens_arr );; *)
match 
	(* {|
	.equ value1 (2*10)
	.equ value2 %1000000 + value1

	lab0: INY
				CMP #'Z'+1
				BEQ lab0
				BNE lab1
				LDA $10
				CMP $4401
				LDA c_data
				CMP a_data
	lab1: JMP value2

	c_data: .byte "Lot of fun with Ocaml programming :)"
	a_data: .word 1 2 3 4 5 6 7 $AA0 
	b_data: .word %10000000000, $AABB, $CCDD,
	$EEEE, $FFFF
	|}  *)

(* {|
SRC:     .word $0400     ;source string pointer ($40)
DST:     .word $0500     ;destination string pointer ($42)
;
;;       ORG $0600       ;execution start address
;
				 JMP DONE
TOLOWER: LDY #$00        ;starting index
;
LOOP:   LDA (SRC+1),Y     ; get from source string 
        BEQ DONE        ;end of string
;
        CMP #'A'        ;if lower than UC alphabet...
        BCC SKIP        ;copy unchanged
;
        CMP #'Z'+1      ;if greater than UC alphabet...
        BCS SKIP        ;copy unchanged
;
        ORA #%00100000  ;convert to lower case
;
SKIP:    
				STA (DST),Y     ;store to destination string
        INY             ;bump index
        BNE LOOP        ;next character
;
; NOTE: If Y wraps the destination string will be left in an undefined
;  state.  We set carry to indicate this to the calling function.
;
        SEC             ;report string too long error &...
        RTS             ;return to caller
;
DONE:    STA (DST),Y     ;terminate destination string
        CLC             ;report conversion completed &...
        RTS             ;return to caller
;
;;        .END


|} *)

(* {|.org $8000
	.equ Char1 'A'
	Char2 ='A' + 1

	l0: JMP (l1)
			CMP #Char1 + 1
			BNE l1
			CMP #Char2
	l1: JMP l0
|} *)

{| .org $8000
   .equ Char1 'A'
   Char2 = ( ~$FFE + 2)

   l0: JMP (l1)
      LDA heart, X
      CMP #Char1 + 1
      BNE l1
      CMP #Char2
      LDA $A0, X
   l1: JMP l0
	 heart: .byte %00000, %01010, %11111
	 
|}
	|> tokenize |> preprocess_tokens |> asm_lines_p.run with
(* | Ok (pos, len), a ->  
                  printf "Ok at: %d  length: %d\n%s" pos len (tokensn2str tokens_arr (pos, len))*)
| _, Ok a -> 
	 	(* List.iter (fun l -> printf "label:%s %d\n" l.name l.value) state.labels; *)
		printf "Success with %s" (asm_line_list2string a);
| _, Error e -> printf "Error %s" e




