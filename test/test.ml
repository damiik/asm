(* 
asm - Copyright (c) 2020 Dariusz Mikołajczyk 
*)
open Printf
open Tokenizer
open Mparser
open Preprocess
open Asmparser

module A = Alcotest

let quickcase (descr, case) = A.test_case descr `Quick case 

(* test expressions *)

let my_expr_compare = exp_compare |> ((fun ppf exp -> Fmt.pf ppf "expression %s" (exp |> exp2string)) |> A.testable) 
let checkparse_exp inp_s result = A.(check my_expr_compare) inp_s result (

  match (inp_s |> tokenize |> Array.of_list |> ref, 0, 0, [], []) |> set_state |> exp_p.run with
  | _, Ok expr -> expr
  | _, Error _ -> Null
)
let test_suite_exp = List.map quickcase [
    ("test numbers", fun () -> (
        checkparse_exp "3" (Num 3);
        checkparse_exp "$12513561254" (Num 0x12513561254);
        checkparse_exp "1203*2+5*10" (Sum ( Mul ( (Num 1203), (Num 2) ), Mul ((Num 5), (Num 10))))
      )
    );
    ("test braces", fun () -> (
        checkparse_exp "2+5*(-%0001001010+3)-8" (Sum(Num 2, Sub(Mul(Num 5, Sum(Min (Num 74), Num 3)), Num 8)));
        checkparse_exp "(2+(5*3))-8" (Sub(Sum(Num 2, Mul(Num 5, Num 3)), Num 8))
      )
    );
  ] 

(* test instructions *)


let my_asm_line_list_compare = asm_line_list_compare |> ((fun ppf asm_line -> Fmt.pf ppf "instruction %s" (asm_line |> asm_line_list2string)) |> A.testable) 

let checkparse_instr inp_s result  = A.(check my_asm_line_list_compare) inp_s result (

    match inp_s |> tokenize |> preprocess_tokens |> asm_lines_p.run with
    | _, Ok instr -> instr
    | _, Error _ -> [Empty]
)
let mk_instr (i:int) (l:int list) : asm_line = Instr {opcode = i; operand = Fixed l}
let mk_lab_instr (n:string) (v:int) (i:int) (l:int list) : asm_line = Label_Instr (n, v, {opcode = i; operand = Fixed l})
let mk_lab_data (n:string) (v:int) (l:int list) : asm_line = Label_Data(n, v, l)

let mk_instr_in_rel (i:int) (c:int) (l:string) : asm_line = Instr {opcode = i; operand = (Incomplete {address_mode = Relative; count_pos = c; expr = Identifier l}) }
let test_suite_instr = List.map quickcase [
    ("test instructions", fun () -> (
        (* checkparse_instr "\n" [Empty]; *)
        checkparse_instr "label1:\n" [(Label ("label1:", 0))];
        checkparse_instr "LDA #$FF\n"[mk_instr 0xA9 [0xFF]]; 
        checkparse_instr "LDA $FF\n" [mk_instr 0xA5 [0xFF]];
        (* checkparse_instr "LDA #$100\n" (Instr (m32 [0xA9;0x00;0x01])); <-- fail *) 
        checkparse_instr "LDA $1000\n" [mk_instr 0xAD [0x00;0x10]];
        checkparse_instr "LDA $1000\nlabel2:JMP ($5597)\n" [mk_instr 0xAD [0x00;0x10]; mk_instr 0x6C [0x97;0x55]];
        checkparse_instr "label1: lda #%00001110\n" [mk_instr  169 [0x0e]];
      )
    );
    ("test instructions addr modes", fun () -> (
        checkparse_instr "JMP $5597\nl1:\n" [mk_instr 0x4C [0x97;0x55]; Label ("l1:", 3)];
        checkparse_instr "JMP ($5597)\nl1:\n" [mk_instr 0x6C [0x97;0x55]; Label ("l1:", 3)];
        checkparse_instr "CMP $4401,X\nl1:\n" [mk_instr 0xDD [0x01;0x44]; Label ("l1:", 3)];
        checkparse_instr "CMP $4401,Y\nl1:\n" [mk_instr 0xD9 [0x01;0x44]; Label ("l1:", 3)];
        checkparse_instr "CMP $44,X\nl1:\n" [mk_instr 0xD5 [0x44]; Label ("l1:", 2)];
        checkparse_instr "STX $44,Y\nl1:\n" [mk_instr 0x96 [0x44]; Label ("l1:", 2)];
        checkparse_instr "SBC ($FF,X)\nl1:\n" [mk_instr 0xE1 [0xFF]; Label ("l1:", 2)];
        checkparse_instr "SBC ($FF),Y\nl1:\n" [mk_instr 0xF1 [0xFF]; Label ("l1:", 2)];
        checkparse_instr "LDA $1000\nCMP $4401,X\nl1: JMP $09\n" [mk_instr 0xAD [0x00; 0x10] ; mk_instr 0xDD [0x01; 0x44]; mk_instr 0x4C [0x09; 0x00]];
        checkparse_instr "LDA $10\nCMP $4401\nl1: JMP $09\n" [mk_instr 0xA5 [0x10]; mk_instr 0xCD [0x01; 0x44]; mk_instr 0x4C [0x09; 0x00]];

        checkparse_instr {| BNE l1
                            LDA $10
                            CMP $4401
                        l2: JMP $09
                        |} [mk_instr_in_rel 0xD0 0 "l1"; mk_instr 0xA5 [0x10]; mk_instr 0xCD [0x01; 0x44]; mk_instr 0x4C [0x09; 0x00]];
 
        checkparse_instr {| l0: INY
                                BEQ l0
                                BNE l1
                                LDA $10
                                CMP $4401
                            l1: JMP $09
                          |} [mk_instr 0xC8 []; mk_instr 0xF0 [0xFD];mk_instr 0xD0 [0x05]; mk_instr 0xA5 [0x10]; mk_instr 0xCD [0x01; 0x44]; mk_instr 0x4C [0x09; 0x00]];
                         
        checkparse_instr {| .org $8000
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
                          |} [mk_instr 0x6C [0x0E; 0x80];mk_instr 0xBD [0x11; 0x80];mk_instr 0xC9 [0x42]; mk_instr 0xD0 [0x04]; mk_instr 0xC9 [0x42]; mk_instr 0xB5 [0xA0]; mk_instr 0x4C [0x00; 0x80]; mk_lab_data "heart:" 32785 [0x00; 0x0A; 0x1F]];
              
      )
    );
  ] 

  let rec int_list_compare (l1 : int list) (l2 : int list) : bool = 

    match (l1, l2) with
    | ([], []) -> true
    | (x1::xs1, x2::xs2) -> (x1 = x2) && (int_list_compare xs1 xs2)
    | _ -> false

  let rec int_list_2string (l : int list) : string = 

    match l with
    | [] -> ""
    | x::[]-> sprintf "%02X" x
    | x::xs -> sprintf "%02X; %s" x (xs |> int_list_2string )


  let my_asm_line_list_compare2 = int_list_compare |> ((fun ppf asm_line -> Fmt.pf ppf "code block %s" (asm_line |> int_list_2string)) |> A.testable) 


  let checkparse_progr inp_s result = A.(check my_asm_line_list_compare2) inp_s result (
  
      match inp_s |> tokenize |> preprocess_tokens |> asm_lines_p.run with
      | _, Ok instr ->  asm_line_list2int_list instr
      | _, Error _ -> []
  )
  let test_suite_asm = List.map quickcase [
      ("test programs", fun () -> (

          checkparse_progr 
          
          {| 
          ; example code from: www.tejotron.com
          PORTB = $6000
          PORTA = $6001
          DDRB = $6002
          DDRA = $6003
          
          E  = %10000000
          RW = %01000000
          RS = %00100000
          
            .org $8000
          
          reset:
             ldx #$ff       ; initialise stack pointer
             txs
          
             lda #%11111111 ; Set all pins on VIA port B to output
             sta DDRB
             lda #%11100000 ; Set top 3 pins on VIA port A to output
             sta DDRA
          
             lda #%00111000 ; Set 8-bit mode; 2-line display; 5x8 font
             jsr lcd_instruction
             lda #%00001110 ; Display on; cursor on; blink off
             jsr lcd_instruction
             lda #%00000111 ; Increment and shift cursor; shift display
             jsr lcd_instruction
             lda #%00000001 ; Clear display
             jsr lcd_instruction
             lda #%00000010 ; Home
             jsr lcd_instruction
          
             lda #%01001000 ; Set Cursor at CGRAM 0x8
             jsr lcd_instruction
          
             ldx #0
             ldy #16
          cgram:
             lda heart,x
             jsr print_char
             inx
             dey
             bne cgram
          
             lda #%10010000 ; Set Cursor at position 16
             jsr lcd_instruction
          
             ldx #0
          print1:
             lda line1,x
             beq done1
             jsr print_char
             inx
             jmp print1
          
          done1:
             lda #%00000110 ; Increment and shift cursor; no shift display
             jsr lcd_instruction
          
             lda #%11010000 ; Set Cursor at position 0x50
             jsr lcd_instruction
          
             ldx #0
          print2:
             lda line2,x
             beq done2
             jsr print_char
             inx
             jmp print2
          done2:
          
             ldy #40
          shift40:
             lda #%00011000 ; Shift display left
             jsr lcd_instruction
             dey
             bne shift40
          
          loop:
             jmp loop
          
          ;                 0123456789012345678901234567890123456789
          line1:    .asciiz  "www.tejotron.com"
          
          line2: 
                .byte  2," Hello world! ",1,0
          
          
          heart: .byte %00000,%01010,%11111,%11111,%01110,%00100,%00000,%00000
          alien: .byte %11111,%10101,%11111,%11111,%01110,%01010,%11011,%00000
          
          lcd_wait:
             pha
             lda #%00000000 ; Set all pins on VIA port B to input
             sta DDRB
          lcdbusy:
             lda #RW
             sta PORTA
             lda #(RW | E)
             sta PORTA
             lda PORTB
             and #%10000000 ; filter out the address bits, leaving the busy flag
             bne lcdbusy    ; branch if not zero
             lda #RW
             sta PORTA
             lda #%11111111 ; Set all pins on VIA port B to output
             sta DDRB
             pla
             rts
          
          lcd_instruction:
             jsr lcd_wait
             sta PORTB
             lda #0         ; Clear RS/RW/E bits - RS=0=instruction register
             sta PORTA
             lda #E         ; Set E bit to send instruction
             sta PORTA
             lda #0         ; Clear RS/RW/E bits
             sta PORTA
             rts
          
          print_char:
             jsr lcd_wait
             sta PORTB
             lda #RS        ; Set RS bit; Clear RW/E bits - RS=1=data register
             sta PORTA
             lda #(RS | E)  ; Set RS + E bit to send instruction
             sta PORTA
             lda #RS        ; Clear E bit
             sta PORTA
             rts
          |} 
          
          [0xA2; 0xFF; 0x9A; 0xA9; 0xFF; 0x8D; 0x02; 0x60; 0xA9; 0xE0; 0x8D; 0x03; 0x60; 0xA9; 0x38; 0x20; 
           0xC6; 0x80; 0xA9; 0x0E; 0x20; 0xC6; 0x80; 0xA9; 0x07; 0x20; 0xC6; 0x80; 0xA9; 0x01; 0x20; 0xC6; 
           0x80; 0xA9; 0x02; 0x20; 0xC6; 0x80; 0xA9; 0x48; 0x20; 0xC6; 0x80; 0xA2; 0x00; 0xA0; 0x10; 0xBD; 
           0x93; 0x80; 0x20; 0xDC; 0x80; 0xE8; 0x88; 0xD0; 0xF6; 0xA9; 0x90; 0x20; 0xC6; 0x80; 0xA2; 0x00; 
           0xBD; 0x71; 0x80; 0xF0; 0x07; 0x20; 0xDC; 0x80; 0xE8; 0x4C; 0x40; 0x80; 0xA9; 0x06; 0x20; 0xC6; 
           0x80; 0xA9; 0xD0; 0x20; 0xC6; 0x80; 0xA2; 0x00; 0xBD; 0x82; 0x80; 0xF0; 0x07; 0x20; 0xDC; 0x80; 
           0xE8; 0x4C; 0x58; 0x80; 0xA0; 0x28; 0xA9; 0x18; 0x20; 0xC6; 0x80; 0x88; 0xD0; 0xF8; 0x4C; 0x6E; 
           0x80; 0x77; 0x77; 0x77; 0x2E; 0x74; 0x65; 0x6A; 0x6F; 0x74; 0x72; 0x6F; 0x6E; 0x2E; 0x63; 0x6F; 
           0x6D; 0x00; 0x02; 0x20; 0x48; 0x65; 0x6C; 0x6C; 0x6F; 0x20; 0x77; 0x6F; 0x72; 0x6C; 0x64; 0x21; 
           0x20; 0x01; 0x00; 0x00; 0x0A; 0x1F; 0x1F; 0x0E; 0x04; 0x00; 0x00; 0x1F; 0x15; 0x1F; 0x1F; 0x0E; 
           0x0A; 0x1B; 0x00; 0x48; 0xA9; 0x00; 0x8D; 0x02; 0x60; 0xA9; 0x40; 0x8D; 0x01; 0x60; 0xA9; 0xC0; 
           0x8D; 0x01; 0x60; 0xAD; 0x00; 0x60; 0x29; 0x80; 0xD0; 0xEF; 0xA9; 0x40; 0x8D; 0x01; 0x60; 0xA9; 
           0xFF; 0x8D; 0x02; 0x60; 0x68; 0x60; 0x20; 0xA3; 0x80; 0x8D; 0x00; 0x60; 0xA9; 0x00; 0x8D; 0x01; 
           0x60; 0xA9; 0x80; 0x8D; 0x01; 0x60; 0xA9; 0x00; 0x8D; 0x01; 0x60; 0x60; 0x20; 0xA3; 0x80; 0x8D; 
           0x00; 0x60; 0xA9; 0x20; 0x8D; 0x01; 0x60; 0xA9; 0xA0; 0x8D; 0x01; 0x60; 0xA9; 0x20; 0x8D; 0x01; 
           0x60; 0x60];
        )
      );
    ] 



let () = Printexc.record_backtrace true; A.run "parser" [
    "stack", [
      (* A.test_case "" `Quick test_xxx;*)
    ];
    "parser - expressions", test_suite_exp;
    "parser - instructions", test_suite_instr;
    "asm - programs", test_suite_asm;

    (* "miscellanous programs", Testprogram.test_suite; *)
  ]

