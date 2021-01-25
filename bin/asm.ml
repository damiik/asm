open Tokenizer
open Parser

module T = Tokenizer
module P = Parser
let a1 = {|

message: .byte "hello, my beautyfull world!"
end_msg:

NEWLINE = $0D
UPPERCASE = $8E
VERA_BASE_REG = $9F20

.proc   mypopa

; .if (.cpu .bitand ::CPU_ISET_65SC02)
        ; lda     (sp)
; .else
        ldy     #0              ; (2)
        lda     (sp),y          ; (7) Read byte
; .endif
        inc     sp              ; (12)
        beq     @L1             ; (14)
        rts                     ; (20)

@L1:    inc     sp+1
        rts

.endproc

__sei:
	sei
	rts

__cli:
  cli
	rts

__vinit:
	; JSR popptr1           ; Get
	; ldy     #0            ; ccount in struct outdesc
	; lda     (ptr1),y
  ; LDA #%00000000;
	; jsr mypopa

	STZ VERA_ctrl						;ADDR_SEL = 0  --> ADDR0_L / ADDR0_M / ADDR0_H.
 	
	; layer0 Tile Base Address Bits 11-16 x xxxx x000 0000 0000, 
	; b1: tile height (8 or 16 pixels), 
	; b0: tile width (8 or 16 pixels)
	; LDA $00	(a loaded as parameter)							
	STA VERA_L0_tilebase	 

	; b7-b6: map height (32, 64, 128, 256 tiles),
	; b5-b4: map width (32, 64, 128, 256 tiles), 
	; b3:T256, b2:BitMap Mode, 
	; b1-b0:Color depth (1bpp, 2bpp, 4bpp, 8bpp)
	LDA #%00000111					; REM 64 X 128 TILES 4 BPP MODE
	STA VERA_L0_config      

|} 
;;

(* let a = "2+5*(-%0001001010+3)-8" *)
(* "$125/10-2*$ff z" *)

(* let a = inst_line_p.run (input_rec (ref (Array.of_list (tokenize ""))) 0 0);; *)


(* (if( (Array.length Sys.argv) > 0) then (Sys.argv.(1)) else a)));; *)



(* showTokens ( tokens_arr );; *)
match (inst_line_p.run (set_state (ref (Array.of_list (tokenize 
{|  BNE l1
		LDA $10
		CMP $4401
l1: JMP $09
|} ))) 0 0 [] [])) with
(* | Ok (pos, len), a ->  
                  Printf.printf "Ok at: %d  length: %d\n%s" pos len (tokensn2str tokens_arr (pos, len))*)
| state, Ok a -> 
	 	List.iter (fun l -> Printf.printf "label:%s %d\n" l.name l.value ) state.labels;
		Printf.printf "Success with %s" (instr_list2string a);
| _, Error e -> Printf.printf "Error %s" e


(* Printf.printf "%d" (eval (Parser.parse_E ()) );; *)


