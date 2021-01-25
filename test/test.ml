open Tokenizer
open Parser

module T = Tokenizer
module P = Parser
module A = Alcotest

let quickcase (descr, case) = A.test_case descr `Quick case 

(* test expressions *)
let my_expr_compare = (Alcotest.testable (fun ppf exp -> Fmt.pf ppf "expression %s" (P.exp2string exp))) P.exp_compare

let checkparse_exp inp_s result = A.(check my_expr_compare) inp_s result (

  match exp_p.run (set_state (ref (Array.of_list (tokenize inp_s))) 0 0 [] []) with
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


let my_instr_list_compare = (Alcotest.testable (fun ppf exp -> Fmt.pf ppf "instruction %s" (P.instr_list2string exp))) P.instr_list_compare

let checkparse_instr inp_s result  = A.(check my_instr_list_compare) inp_s result (

    match inst_line_p.run (set_state (ref (Array.of_list (tokenize inp_s))) 0 0 [] []) with
    (* | (_,l) -> l *)
    | _, Ok instr -> instr
    | _, Error _ -> [Empty]
)
let mk_instr (i:int) (l:int list) : inst_line = Instr {opcode = i; operand = Fixed l}
let mk_lab_instr (n:string) (v:int) (i:int) (l:int list) : inst_line = Label_Instr (n, v, {opcode = i; operand = Fixed l})

let mk_instr_in_rel (i:int) (c:int) (l:string) : inst_line = Instr {opcode = i; operand = (Incomplete {address_mode = Relative; count_pos = c; identifier = l}) }
let test_suite_instr = List.map quickcase [
    ("test instructions", fun () -> (
        (* checkparse_instr "\n" [Empty]; *)
        checkparse_instr "label1:\n" [(Label ("label1:", 0))];
        checkparse_instr "LDA #$FF\n"[mk_instr 0xA9 [0xFF]]; 
        checkparse_instr "LDA $FF\n" [mk_instr 0xA5 [0xFF]];
        (* checkparse_instr "LDA #$100\n" (Instr (m32 [0xA9;0x00;0x01])); <-- fail *) 
        checkparse_instr "LDA $1000\n" [mk_instr 0xAD [0x00;0x10]];
        checkparse_instr "LDA $1000\nlabel2:JMP ($5597)\n" [mk_instr 0xAD [0x00;0x10]; mk_lab_instr "label2:" 3 0x6C [0x97;0x55]];
        checkparse_instr "label1: lda #%00001110\n" [mk_lab_instr "label1:" 0 169 [0x0e]];
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
        checkparse_instr "LDA $1000\nCMP $4401,X\nl1: JMP $09\n" [mk_instr 0xAD [0x00; 0x10] ; mk_instr 0xDD [0x01; 0x44]; mk_lab_instr "l1:" 6 0x4C [0x09; 0x00]];
        checkparse_instr "LDA $10\nCMP $4401\nl1: JMP $09\n" [mk_instr 0xA5 [0x10]; mk_instr 0xCD [0x01; 0x44]; mk_lab_instr "l1:" 5 0x4C [0x09; 0x00]];
        checkparse_instr {| BNE l1
                            LDA $10
                            CMP $4401
                        l2: JMP $09
                        |} [mk_instr_in_rel 0xD0 0 "l1"; mk_instr 0xA5 [0x10]; mk_instr 0xCD [0x01; 0x44]; mk_lab_instr "l2:" 7 0x4C [0x09; 0x00]];
        checkparse_instr {| l0: INY
                                BEQ l0
                                BNE l1
                                LDA $10
                                CMP $4401
                            l1: JMP $09
                        |} [mk_instr 0xD0 [0x05]; mk_instr 0xA5 [0x10]; mk_instr 0xCD [0x01; 0x44]; mk_lab_instr "l1:" 7 0x4C [0x09; 0x00]];
                         
      )
    );
  ] 

let () = Printexc.record_backtrace true; A.run "parser" [
    "stack", [
      (* A.test_case "" `Quick test_xxx;*)
    ];
    "parser - expressions", test_suite_exp;
    "parser - instructions", test_suite_instr;

    (* "miscellanous programs", Testprogram.test_suite; *)
  ]


