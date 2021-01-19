open Tokenizer
open Parser

module T = Tokenizer
module P = Parser
module A = Alcotest

let pp_expr ppf exp = Fmt.pf ppf "expression %s" (P.exp2string exp)
let myexpr = Alcotest.testable pp_expr P.exp_compare
(* let mypurity = Alcotest.testable pp_puret equal_puret *)
let parse_test inp_s =
        match exp_p.run {tokens= ref (Array.of_list (tokenize inp_s)) ; pos= 0} with
        | _, Ok expr -> expr
        | _, Error _ -> Num 1
let checkparse e result = A.(check myexpr) e result (parse_test e)


  (* (P.run ( bind exp_p ( fun a -> P.return a)
    
    ) ( ref (Array.of_list (tokenize inp_s)) )) *)

(* let test_case = fun a -> () *)

let test_numbers () =
   checkparse "3" (Num 3);
   checkparse "$1251356125" (Num 78671864101);
   checkparse "1203*2+5*10" (Sum ( Mul ( (Num 1203), (Num 2) ), Mul ((Num 5), (Num 10))));
   checkparse "2+5*(-%0001001010+3)-8" (Sum(Num 2, Sub(Mul(Num 5, Sum(Min (Num 74), Num 3)), Num 8)))
   (* checkparse "(2+(5*3))-8" (NumInt(0));
   checkparse "2+5*-(-%0001001010+3)-8" (NumInt(0)); *)

let quickcase (descr, case) = A.test_case descr `Quick case 
let test_suite = List.map quickcase [
    ("parse numbers", test_numbers);

  ] 

let () = Printexc.record_backtrace true; A.run "parser" [
    "stack", [
      (* A.test_case "stack underflow" `Quick test_stack_underflow; *)
      (* A.test_case "stack overflow" `Quick test_stack_overflow *)
    ];
    "parser", test_suite;

    (* "miscellanous programs", Testprogram.test_suite; *)
  ]

(* let () = A.run "yourpackagename" [
  "testsuite1", [
    A.test_case "test1" `Quick test_case;
  ];
  "test_numbers", [

    test_suite;
  ]
] *)

