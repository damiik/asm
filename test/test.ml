open Tokenizer
open Parser

module T = Tokenizer
module P = Parser
module A = Alcotest

let quickcase (descr, case) = A.test_case descr `Quick case 

(* test expressions *)
let my_expr_compare = (Alcotest.testable (fun ppf exp -> Fmt.pf ppf "expression %s" (P.exp2string exp))) P.exp_compare

let checkparse_exp inp_s result = A.(check my_expr_compare) inp_s result (

  match exp_p.run (input_rec (ref (Array.of_list (tokenize inp_s))) 0 0) with
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

    match inst_line_p.run (input_rec (ref (Array.of_list (tokenize inp_s))) 0 0) with
    (* | (_,l) -> l *)
    | _, Ok instr -> instr
    | _, Error _ -> [Empty]
)
let m32(l:int list) = List.map (fun e -> (Int32.of_int e)) l
let test_suite_instr = List.map quickcase [
    ("test instructions", fun () -> (
        (* checkparse_instr "\n" [Empty]; *)
        checkparse_instr "label1:\n" [(Label ("label1:", 0))];
        checkparse_instr "LDA #$FF\n"[Instr (m32 [0xA9;0xFF])];
        checkparse_instr "LDA $FF\n" [Instr (m32 [0xA5;0xFF])];
        (* checkparse_instr "LDA #$100\n" (Instr (m32 [0xA9;0x00;0x01])); <-- fail *) 
        checkparse_instr "LDA $1000\n" [Instr (m32 [0xAD;0x00;0x10])];
        checkparse_instr "LDA $1000\nlabel2:JMP ($5597)\n" [Instr (m32 [0xAD;0x00;0x10]); Label_Instr ("label2:", 3, m32 [0x6C;0x97;0x55])];
        checkparse_instr "label1: lda #%00001110\n" [Label_Instr ("label1:", 0, (m32 [169;0x0e]))];
      )
    );
    ("test instructions addr modes", fun () -> (
        checkparse_instr "JMP $5597\nl1:\n" [Instr (m32 [0x4C;0x97;0x55]); Label ("l1:", 3)];
        checkparse_instr "JMP ($5597)\nl1:\n" [Instr (m32 [0x6C;0x97;0x55]); Label ("l1:", 3)];
        checkparse_instr "CMP $4401,X\nl1:\n" [Instr (m32 [0xDD;0x01;0x44]); Label ("l1:", 3)];
        checkparse_instr "CMP $4401,Y\nl1:\n" [Instr (m32 [0xD9;0x01;0x44]); Label ("l1:", 3)];
        checkparse_instr "CMP $44,X\nl1:\n" [Instr (m32 [0xD5;0x44]); Label ("l1:", 2)];
        checkparse_instr "STX $44,Y\nl1:\n" [Instr (m32 [0x96;0x44]); Label ("l1:", 2)];
        checkparse_instr "SBC ($FF,X)\nl1:\n" [Instr (m32 [0xE1;0xFF]); Label ("l1:", 2)];
        checkparse_instr "SBC ($FF),Y\nl1:\n" [Instr (m32 [0xF1;0xFF]); Label ("l1:", 2)];
        checkparse_instr "LDA $1000\nCMP $4401,X\nl1: JMP $09\n" [Instr (m32[0xAD; 0x00; 0x10]) ; Instr (m32[0xDD; 0x01; 0x44]); Label_Instr("l1:", 6, m32[0x4C; 0x09; 0x00])];
        checkparse_instr "LDA $10\nCMP $4401\nl1: JMP $09\n" [Instr (m32[0xA5; 0x10]) ; Instr (m32[0xCD; 0x01; 0x44]); Label_Instr("l1:", 5, m32[0x4C; 0x09; 0x00])];
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


